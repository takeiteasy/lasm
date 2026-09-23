;;;; mode.lisp
;;;; DEFMODE: a declarative addressing-mode grammar (LASM-plan.md sec. 3.4).
;;;; Each mode is a literal/token pattern with one or more `expr` holes and an
;;;; optional default operand :WIDTH; matching a mode against a statement's
;;;; operand tokens consumes its literal tokens and parses each `expr` hole
;;;; with the shared Pratt parser (parser.lisp).
;;;;
;;;; This replaces M1's *BUILTIN-MODE-PREFIXES* table (formerly in
;;;; instruction.lisp) -- IMMEDIATE and ABSOLUTE are now ordinary DEFMODE
;;;; forms defined at the bottom of this file, rather than a special case
;;;; MATCH-OPERAND-MODE has to know about.
;;;;
;;;; Registration happens inside an EVAL-WHEN, like DEFMACHINE (machine.lisp):
;;;; DEFINSTRUCTION resolves mode names against this registry at
;;;; macroexpansion time, so a mode must be visible as soon as its DEFMODE
;;;; form is compiled, not only after the file loads.
;;;;
;;;; Modes are a global registry (mirroring *LEXERS*, lexer.lisp), not scoped
;;;; per machine -- a mode is a syntax concept, like a lexer's surface syntax,
;;;; not part of any one machine's storage model. Two machines wanting the
;;;; same mode name with different syntax is a follow-up concern.

(in-package #:lasm)

(defvar *register-alias-elements* nil)

;;; Mode descriptor
;;;
;;; Wrapped in an EVAL-WHEN, like the DEFVAR below, so MAKE-MODE-DESCRIPTOR
;;; is callable at :COMPILE-TOPLEVEL time from this same file's built-in
;;; DEFMODE forms.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct mode-descriptor
    name       ; symbol, upcased on lookup like instruction mnemonics
    pattern    ; list of (:literal "text") | (:expr), in match order
    width      ; default operand byte width, or nil (caller/machine decides)
    relativep  ; T if this mode's operand is a PC-relative offset (#23), not
               ; an absolute value -- the assembler computes the offset from
               ; the branch's own address at encode time (assembler.lisp).
               ; Implies SIGNEDP (below); a RELATIVE mode is always signed,
               ; since a branch offset can go either direction.
    signedp    ; T if this mode's operand is a signed quantity (#30, split off
               ; RELATIVE): the decoder sign-extends the fetched operand
               ; (decoder.lisp) before handing it to semantics, and the
               ; assembler's mode selector range-checks candidate values
               ; against the signed range rather than the unsigned one
               ; (assembler.lisp's %CHOOSE-VARIANT).
    suffix     ; string, or nil -- a gas-style mnemonic suffix (e.g. "w" for
               ; ABSOLUTE, "z" for ZERO-PAGE, #40) a program can append to a
               ; mnemonic (lda.w target) to force this mode regardless of
               ; what the operand's value folds to, bypassing relaxation's
               ; floor and value filters entirely (assembler.lisp's
               ; %CHOOSE-VARIANT). Not every mode needs one -- only modes
               ; that share operand syntax with another mode (so relaxation
               ; has an actual choice to override) benefit from a suffix;
               ; LASM's built-ins give one only to ZERO-PAGE/ABSOLUTE.
    strictp    ; T if an operand encoded through this mode that doesn't fit
               ; its own width is an ASSEMBLY-ERROR rather than silently
               ; wrapping (#74, absorbing #28/#43) -- checked at encode time
               ; by %ENCODE's :INSTRUCTION branch (assembler.lisp), alongside
               ; the *STRICT-OPERAND-RANGE* global switch (diagnostic.lisp)
               ; that makes every mode strict, including a mode-less
               ; instruction's bare operand. Default NIL preserves #28/#43's
               ; original wrap-on-overflow behavior.
    varyingp)  ; T if this mode's pattern has a varying ONE-OF element.
               ; %MODE-HOLE-TUPLES (below) returns one tuple per combination.
               ; %CHECK-ONE-OF-ELEMENTS! rejects any mode declaring VARYINGP
               ; T as another mode's own :ONE-OF alternative -- a varying
               ; :ONE-OF nested inside another is not yet supported.
  )

;; Registry of defined addressing modes, keyed by name -- mirrors *LEXERS*
;; (lexer.lisp) and *MACHINES* (storage.lisp). Unlike those two, this needs
;; an EVAL-WHEN around its own DEFVAR: DEFMODE's registration runs at
;; :COMPILE-TOPLEVEL (below), and this file defines its own built-in modes
;; (IMMEDIATE, ABSOLUTE, ...) at the bottom of this same file -- a plain
;; top-level DEFVAR's initial value is only guaranteed set at load time, which
;; is too late for a DEFMODE compiled later in the same compilation unit.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *modes* (make-hash-table :test 'eq)))

(defun find-mode-descriptor (name)
  "Look up the MODE-DESCRIPTOR registered under NAME (a symbol) with DEFMODE.
Signals an error if none is registered."
  (or (gethash name *modes*)
      (error "No addressing mode named ~S has been defined with DEFMODE" name)))

;; Both wrapped in an EVAL-WHEN, like BUILD-MODE-DESCRIPTOR itself (below) --
;; BUILD-MODE-DESCRIPTOR calls %CHECK-SUFFIX-COLLISION, which calls
;; FIND-MODE-BY-SUFFIX, at :COMPILE-TOPLEVEL time for this same file's own
;; built-in DEFMODE forms (bottom of this file), so a plain DEFUN (only
;; guaranteed callable at load time) would be too late.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-mode-by-suffix (suffix)
    "Look up the MODE-DESCRIPTOR whose :SUFFIX (a string, #40) equals SUFFIX
case-insensitively, or NIL if none declares one. A linear scan over *MODES*
rather than a second suffix -> descriptor table -- there are only a handful
of modes registered at once, and a parallel table would need its own
invalidation whenever a DEFMODE is redefined dropping (or changing) its
suffix. Used by the assembler's forced-mode operand syntax (assembler.lisp's
%CHOOSE-VARIANT) to resolve e.g. \"w\" in \"lda.w\" to ABSOLUTE."
    (loop for mode being the hash-values of *modes*
          when (and (mode-descriptor-suffix mode)
                    (string-equal (mode-descriptor-suffix mode) suffix))
            return mode))

  (defun %check-suffix-collision (name suffix)
    "Signal an error if SUFFIX (non-NIL) is already claimed by a mode other
than NAME -- e.g. two DEFMODE forms both declaring :SUFFIX \"w\" would make
FIND-MODE-BY-SUFFIX's lookup ambiguous. Compares by MODE-DESCRIPTOR-NAME,
not object identity: DEFMODE re-registering the same NAME (a plain file
reload, e.g. under ASDF) builds a fresh MODE-DESCRIPTOR struct each time, so
an EQ check would spuriously reject a mode reclaiming its own suffix."
    (when suffix
      (let ((existing (find-mode-by-suffix suffix)))
        (when (and existing (not (eq (mode-descriptor-name existing) name)))
          (error "DEFMODE ~S: suffix ~S is already used by mode ~S"
                 name suffix (mode-descriptor-name existing)))))))

;;; DEFMODE pattern parsing
;;;
;;; These run inside an EVAL-WHEN, not just plain DEFUNs, because this same
;;; file's built-in DEFMODE forms (bottom of this file) call BUILD-MODE-
;;; DESCRIPTOR at :COMPILE-TOPLEVEL time -- a plain DEFUN's body is only
;;; guaranteed callable at load time, which is too late within one
;;; compilation unit (see the DEFVAR comment above for the same issue).
;;;
;;; %MODE-HOLE-COUNT and %PATTERN-HOLE-COUNT (below) also live in this
;;; EVAL-WHEN, not just at top level like a plain accessor would -- ONE-OF
;;; validation (%CHECK-ONE-OF-ELEMENTS!, #103) calls %MODE-HOLE-COUNT at
;;; DEFMODE's own :COMPILE-TOPLEVEL time, the same reason everything else
;;; here needs one.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %one-of-element-p (el)
    "T if EL is a raw DEFMODE pattern element spelled (ONE-OF mode...) (#103)
-- a list headed by a symbol named ONE-OF, matched case-insensitively like
EXPR below."
    (and (consp el) (symbolp (first el)) (string-equal (symbol-name (first el)) "ONE-OF")))

  (defun %one-of-slot (el)
    "Return the optional named slot in a ONE-OF element."
    (cond ((eq (second el) :named) (third el))
          ((consp (second el)) (first (second el)))))

  (defun %one-of-alternatives (el)
    "Return a ONE-OF element's alternative mode names."
    (cond ((eq (second el) :named) (cdddr el))
          ((consp (second el)) (rest (second el)))
          (t (rest el))))

  (defun %parse-mode-pattern (elements)
    "ELEMENTS is DEFMODE's pattern spine: a mix of string literals, the
symbol EXPR, and (ONE-OF mode...) alternations (#103). Returns a list of
(:literal string) | (:expr) | (:one-of mode-name...) pattern elements, plus
any trailing keyword options untouched by this function (the caller splits
those off first)."
    (mapcar (lambda (el)
              (cond
                ((stringp el) (list :literal el))
                ((and (symbolp el) (string-equal (symbol-name el) "EXPR")) (list :expr))
                ((and (consp el) (symbolp (first el))
                      (string-equal (symbol-name (first el)) "EXPR"))
                 (destructuring-bind (expr &key register (relative nil relative-given-p)
                                             (signed nil signed-given-p)) el
                   (declare (ignore expr))
                   (when (and register (not (symbolp register)))
                     (error "Malformed DEFMODE expression hole ~S -- :REGISTER needs a register name" el))
                   (when (and (member :register (rest el)) (null register))
                     (error "Malformed DEFMODE expression hole ~S -- :REGISTER needs a register name" el))
                   (when (and relative-given-p (not (member relative '(nil t))))
                     (error "Malformed DEFMODE expression hole ~S -- :RELATIVE must be T or NIL" el))
                   (when (and signed-given-p (not (member signed '(nil t))))
                     (error "Malformed DEFMODE expression hole ~S -- :SIGNED must be T or NIL" el))
                   (when (and relative (eq signed nil) signed-given-p)
                     (error "DEFMODE expression hole ~S: :RELATIVE T implies :SIGNED T" el))
                   (append (list :expr register)
                           (when relative-given-p (list :relative relative))
                           (when signed-given-p (list :signed signed)))))
                 ((%one-of-element-p el)
                  (let ((slot (%one-of-slot el))
                        (alternatives (%one-of-alternatives el)))
                    (unless (and (>= (length alternatives) 2)
                                 (every #'symbolp alternatives))
                      (error "Malformed DEFMODE pattern element ~S -- (ONE-OF ...) needs at ~
least two mode-name symbols" el))
                    (if slot
                        (list* :one-of :named slot alternatives)
                        (list* :one-of alternatives))))
                (t (error "Malformed DEFMODE pattern element ~S -- expected a string ~
literal, the symbol EXPR, or (ONE-OF mode...)" el))))
            elements))

  (defun %split-mode-clause (body)
    "Split DEFMODE's BODY into (VALUES pattern-elements keyword-plist). Keyword
options start at the first keyword symbol; everything before it is pattern."
    (let ((pos (position-if #'keywordp body)))
      (if pos
          (values (subseq body 0 pos) (subseq body pos))
          (values body nil))))

  (defun %pattern-one-of-min-hole-count (alt-names &optional seen)
    "The minimum MODE-HOLE-COUNT across ALT-NAMES (a :ONE-OF element's own
alternative mode-name symbols) -- since #120, an alternative may contribute
more holes than its siblings, so a :ONE-OF element's own contribution to a
hole count is its *shortest* alternative, not (as before #120) simply its
first one; %CHECK-ONE-OF-ELEMENTS! no longer requires every alternative to
agree."
    (reduce #'min (mapcar (lambda (n) (%mode-hole-count (find-mode-descriptor n) seen)) alt-names)))

  (defun %pattern-hole-count (pattern &optional seen)
    "Total :EXPR holes in PATTERN (a MODE-DESCRIPTOR's own pattern list, or a
DEFMODE-in-progress's) at its *minimum* shape -- a :ONE-OF element
contributes its shortest alternative's own hole count (#120,
%PATTERN-ONE-OF-MIN-HOLE-COUNT); MODE-HOLE-TUPLES (below) is what a caller
wanting every alternative shape, not just the minimum, should use instead.
SEEN is the list of mode names already on this recursion path -- signals an
error rather than recursing forever if a :ONE-OF element names a mode
already being walked (a hand-written DEFMODE cycle: redefining a mode some
:ONE-OF already references so the reference loops back to it). A plain file
reload can't create a cycle (it replays the same patterns in the same
order), so this only ever fires on a genuinely circular redefinition,
caught here at DEFMODE time rather than as an unbounded recursion at some
later, unrelated call."
    (loop for element in pattern
          sum (ecase (first element)
                (:literal 0)
                (:expr 1)
                (:one-of (%pattern-one-of-min-hole-count (%one-of-alternatives element) seen)))))

  (defun %mode-hole-count (mode &optional seen)
    (let ((name (mode-descriptor-name mode)))
      (when (member name seen)
        (error "DEFMODE ~S: ONE-OF cycle -- ~{~S~^ -> ~} -> ~S references itself"
               name (reverse seen) name))
      (%pattern-hole-count (mode-descriptor-pattern mode) (cons name seen))))

  (defun %pattern-hole-alternatives (pattern &optional seen)
    "One entry per hole in PATTERN, in hole order -- NIL for a plain :EXPR
hole, or the list of :ONE-OF alternative mode-name symbols governing a
:ONE-OF-produced hole (#104). A multi-hole :ONE-OF element repeats its own
alt-names list once per hole it contributes -- the whole element's choice of
alternative governs each of its holes alike, mirroring the hole-alignment
%MATCH-MODE-ELEMENTS' CHOICES return value now uses. A nested :ONE-OF (inside
one alternative of an outer one) is walked via that alternative's own
pattern -- see %MODE-HOLE-ALTERNATIVES; this is the pattern-only half, mirroring
%PATTERN-HOLE-COUNT/%MODE-HOLE-COUNT's own split. Used by DEFINSTRUCTION's
word-encoded (CHOICE M) selector validation (instruction.lisp) to check M
against the actual alternatives available at a given hole. SEEN guards
against a DEFMODE cycle, same as %PATTERN-HOLE-COUNT/%MODE-HOLE-COUNT."
    (loop for element in pattern
          append (ecase (first element)
                   (:literal nil)
                    (:expr (list nil))
                    (:one-of (let* ((alt-names (%one-of-alternatives element))
                                    (holes (%pattern-one-of-min-hole-count alt-names seen)))
                              (make-list holes :initial-element alt-names))))))

  (defun %mode-hole-alternatives (mode &optional seen)
    (%pattern-hole-alternatives (mode-descriptor-pattern mode) seen))

  (defun %expr-hole-attribute (element mode attribute)
    (let* ((options (cddr element))
           (relative (if (member :relative options)
                         (getf options :relative)
                         (mode-descriptor-relativep mode)))
           (signed (if (member :signed options)
                       (getf options :signed)
                       (mode-descriptor-signedp mode))))
      (when (and relative (member :signed options) (not signed))
        (error "DEFMODE ~S: a relative EXPR hole cannot be unsigned"
               (mode-descriptor-name mode)))
      (ecase attribute
        (:relative relative)
        (:signed (or relative signed)))))

  (defun %mode-hole-attributes (mode attribute)
    "Return default attributes in pattern order."
    (loop for element in (mode-descriptor-pattern mode)
          append (ecase (first element)
                   (:literal nil)
                   (:expr (list (%expr-hole-attribute element mode attribute)))
                   (:one-of (%mode-hole-attributes
                             (find-mode-descriptor (first (%one-of-alternatives element)))
                             attribute)))))

  (defun %pattern-nested-one-of-signed-p (pattern &optional seen)
    "T if any :ONE-OF element nested anywhere in PATTERN -- at any depth, not
just PATTERN's own top-level elements -- has an alternative declaring
:SIGNED T. Used by %CHECK-ONE-OF-ELEMENTS! to reject a nested :ONE-OF's
:SIGNED alternative: %MATCH-MODE-ELEMENTS' outermost-ONE-OF-wins rule (this
file) means only the *outermost* :ONE-OF a hole belongs to ever gets that
hole's CHOICES entry, so a :SIGNED declared on some inner alternative -- two
:ONE-OF levels down from the hole a DEFINSTRUCTION site actually sees -- has
no decode-time record anywhere that could recover it; letting it through here
would silently not honor it later instead of erroring where the mistake is
made. SEEN guards the same hand-written-redefinition-cycle case
%MODE-HOLE-COUNT does, for the same reason."
    (loop for element in pattern
          thereis (when (eq (first element) :one-of)
                     (let ((alts (mapcar #'find-mode-descriptor (%one-of-alternatives element))))
                      (or (some #'mode-descriptor-signedp alts)
                          (some (lambda (alt)
                                  (some (lambda (hole)
                                          (and (eq (first hole) :expr)
                                               (or (getf (cddr hole) :signed)
                                                   (getf (cddr hole) :relative))))
                                        (mode-descriptor-pattern alt)))
                                alts)
                          (some (lambda (alt)
                                  (let ((alt-name (mode-descriptor-name alt)))
                                    (unless (member alt-name seen)
                                      (%pattern-nested-one-of-signed-p
                                       (mode-descriptor-pattern alt) (cons alt-name seen)))))
                                alts))))))

  (defun %pattern-nested-one-of-width-p (pattern &optional seen)
    "T if any :ONE-OF element nested anywhere in PATTERN -- at any depth, not
just PATTERN's own top-level elements -- has an alternative declaring
:WIDTH. Used by %CHECK-ONE-OF-ELEMENTS! to reject a nested :ONE-OF's :WIDTH
alternative, for the same reason %PATTERN-NESTED-ONE-OF-SIGNED-P (above)
rejects a nested :SIGNED one: %MATCH-MODE-ELEMENTS' outermost-ONE-OF-wins
rule means only the outermost :ONE-OF a hole belongs to ever gets that
hole's CHOICES entry, so a :WIDTH declared on some inner alternative has no
decode-time record anywhere that could recover it -- error where the mistake
is made rather than silently not honoring it later. SEEN guards the same
hand-written-redefinition-cycle case %MODE-HOLE-COUNT does, for the same
reason."
    (loop for element in pattern
          thereis (when (eq (first element) :one-of)
                     (let ((alts (mapcar #'find-mode-descriptor (%one-of-alternatives element))))
                      (or (some #'mode-descriptor-width alts)
                          (some (lambda (alt)
                                  (let ((alt-name (mode-descriptor-name alt)))
                                    (unless (member alt-name seen)
                                      (%pattern-nested-one-of-width-p
                                       (mode-descriptor-pattern alt) (cons alt-name seen)))))
                                alts))))))

  (defun %check-one-of-elements! (name pattern)
    "Validate alternative syntax and supported ONE-OF nesting.
Alternatives may vary in arity and operand attributes; DEFINSTRUCTION
validates encoding support. Whole-mode suffixes, ambiguous syntax and
nested varying modes are rejected."
    (dolist (element pattern)
      (when (eq (first element) :one-of)
         (let* ((alt-names (%one-of-alternatives element))
               (alts (mapcar #'find-mode-descriptor alt-names)))
          (when (< (length alts) 2)
            (error "DEFMODE ~S: ONE-OF needs at least two alternative modes, got ~S"
                   name alt-names))
          (dolist (alt alts)
            (when (mode-descriptor-varyingp alt)
              (error "DEFMODE ~S: ONE-OF alternative ~S itself has a ONE-OF whose own ~
alternatives disagree on hole count -- nesting a varying ONE-OF inside another is not yet ~
supported (#120)"
                     name (mode-descriptor-name alt)))
            (when (%pattern-nested-one-of-signed-p (mode-descriptor-pattern alt))
              (error "DEFMODE ~S: ONE-OF alternative ~S has a nested ONE-OF whose own ~
alternative declares :SIGNED T or :RELATIVE T -- only the outermost ONE-OF a hole belongs ~
to keeps its CHOICES entry, so a nested :SIGNED/:RELATIVE can never be recovered at decode ~
time; give ~S itself :SIGNED T or :RELATIVE T instead, or move the alternative up to this ~
ONE-OF directly"
                     name (mode-descriptor-name alt) (mode-descriptor-name alt)))
            (when (%pattern-nested-one-of-width-p (mode-descriptor-pattern alt))
              (error "DEFMODE ~S: ONE-OF alternative ~S has a nested ONE-OF whose own ~
alternative declares :WIDTH -- only the outermost ONE-OF a hole belongs to keeps its ~
CHOICES entry, so a nested :WIDTH can never be recovered at decode time; give ~S itself ~
:WIDTH instead, or move the :WIDTH alternative up to this ONE-OF directly"
                     name (mode-descriptor-name alt) (mode-descriptor-name alt))))
          (loop for (alt . later) on alts
                do (dolist (other later)
                     (when (and (equalp (mode-descriptor-pattern alt) (mode-descriptor-pattern other))
                                (not (and (mode-descriptor-suffix alt) (mode-descriptor-suffix other))))
                       (error "DEFMODE ~S: ONE-OF alternatives ~S and ~S have identical syntax ~
-- nothing could ever choose between them (give both a :SUFFIX to select by prefix)"
                              name (mode-descriptor-name alt) (mode-descriptor-name other)))))))))

  (defun %pattern-varying-one-of-element (pattern &optional seen)
    "The first ONE-OF element whose alternatives disagree on hole count."
    (find-if (lambda (element)
               (and (eq (first element) :one-of)
                    (> (length (remove-duplicates
                                (mapcar (lambda (n)
                                          (%mode-hole-count (find-mode-descriptor n) seen))
                                         (%one-of-alternatives element))))
                       1)))
             pattern))

  (defstruct mode-hole-group
    base-start
    start
    base-count
    count
    alternatives
    alt-name
    slot)

  (defstruct mode-hole-tuple
    groups
    hole-alternatives
    hole-sources)

  (defun %mode-hole-tuples (mode &optional seen)
    "Fixed-arity shapes across independently varying ONE-OF elements."
    (labels ((walk (pattern base-start start groups holes sources)
               (if (null pattern)
                   (list (make-mode-hole-tuple :groups (reverse groups)
                                              :hole-alternatives holes
                                              :hole-sources sources))
                   (let* ((element (first pattern))
                           (alts (and (eq (first element) :one-of) (%one-of-alternatives element)))
                          (base-count (ecase (first element)
                                        (:literal 0)
                                        (:expr 1)
                                        (:one-of (%pattern-one-of-min-hole-count alts seen))))
                          (minimum (remove-if-not (lambda (n)
                                                    (= (%mode-hole-count (find-mode-descriptor n) seen)
                                                       base-count))
                                                  alts))
                          (over (remove-if (lambda (n)
                                             (= (%mode-hole-count (find-mode-descriptor n) seen)
                                                base-count))
                                           alts))
                           (split-minimum-p (and (%one-of-slot element)
                                                 (> (length minimum) 1))))
                     (loop for alt in (if split-minimum-p
                                           (append minimum over)
                                           (cons nil over))
                           for count = (if alt
                                           (%mode-hole-count (find-mode-descriptor alt) seen)
                                           base-count)
                           append (walk (rest pattern) (+ base-start base-count) (+ start count)
                                        (if (or over split-minimum-p)
                                            (cons (make-mode-hole-group
                                                   :base-start base-start :start start
                                                   :base-count base-count :count count
                                                    :alternatives alts :alt-name alt
                                                    :slot (%one-of-slot element))
                                                  groups)
                                            groups)
                                        (append holes (make-list count :initial-element alts))
                                        (append sources
                                                (ecase (first element)
                                                  (:literal nil)
                                                  (:expr (list (list :expr element)))
                                                  (:one-of
                                                   (loop for index below count
                                                         collect (list :one-of alts index alt)))))))))))
      (walk (mode-descriptor-pattern mode) 0 0 nil nil nil)))

  (defun %hole-source-attribute (mode source attribute &optional choice)
    (ecase (first source)
      (:expr (%expr-hole-attribute (second source) mode attribute))
      (:one-of
       (let* ((alt (find-mode-descriptor (or choice (fourth source)
                                             (first (second source)))))
              (attributes (%mode-hole-attributes alt attribute)))
         (nth (third source) attributes)))))

  (defun %mode-hole-sources (mode)
    (mode-hole-tuple-hole-sources (first (%mode-hole-tuples mode))))

  (defun %mode-hole-count-range (mode &optional seen)
    "(VALUES MIN MAX) hole count across every one of MODE's alternative-tuples
(#120, %MODE-HOLE-TUPLES) -- MIN equals %MODE-HOLE-COUNT; both equal it for
a mode with no varying :ONE-OF element."
    (let ((counts (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple)))
                           (%mode-hole-tuples mode seen))))
      (values (reduce #'min counts) (reduce #'max counts))))

  (defun build-mode-descriptor (name body)
    (multiple-value-bind (pattern-elements options) (%split-mode-clause body)
      (when (null pattern-elements)
        (error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
      (let ((pattern (%parse-mode-pattern pattern-elements)))
        (%check-one-of-elements! name pattern)
        ;; A literal-only mode is useful as a fixed alternative in a ONE-OF.
        ;; It contributes no operand value; the enclosing instruction can
        ;; attach its encoding with a named choice slot.
        (destructuring-bind (&key width relative signed suffix strict) options
          (when (and relative (not (eq signed t)) (member :signed options))
            (error "DEFMODE ~S: :RELATIVE T implies :SIGNED T -- do not pass ~
:SIGNED NIL alongside it" name))
          (%check-suffix-collision name suffix)
          (let ((descriptor
                  (make-mode-descriptor :name name :pattern pattern :width width
                                        :relativep relative :signedp (or relative signed)
                                        :suffix suffix :strictp strict
                                        :varyingp (and (%pattern-varying-one-of-element pattern) t))))
            (dolist (element pattern)
              (when (eq (first element) :expr)
                (%expr-hole-attribute element descriptor :signed)))
            descriptor))))))

(defmacro defmode (name &body pattern)
  "Define a mode from literal tokens, EXPR holes, and ONE-OF alternatives.
A hole may use (EXPR :REGISTER name :SIGNED boolean :RELATIVE boolean).
Hole options override mode-wide :SIGNED and :RELATIVE defaults. A relative
hole is signed, and any number of holes may be relative. :WIDTH supplies
the default operand width; :SUFFIX forces a mode at assembly time; :STRICT
checks ordinary operand ranges. See docs/modes.md."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *modes*) (build-mode-descriptor ',name ',pattern))
     ',name))

;;; Pattern matching

(defun %register-hole-count (mode)
  "The number of register-qualified (EXPR :REGISTER r) holes in MODE's own
pattern. Nested ONE-OF alternatives are not counted."
  (count-if (lambda (el) (and (eq (first el) :expr) (second el)))
            (mode-descriptor-pattern mode)))

(defun %one-of-tie-record (holes-from-end slot chosen tied)
  "A tie record (HOLES-FROM-END SLOT CHOSEN . RUNNERS-UP) for a :ONE-OF element
whose CHOSEN alternative matched with the same literal count as each of TIED,
or NIL when none of TIED remains. A runner-up with fewer register-qualified
holes than CHOSEN is less specific, so it is not a tie."
  (let ((runners-up (remove-if (lambda (alt) (< (%register-hole-count alt)
                                                (%register-hole-count chosen)))
                               tied)))
    (and runners-up (list* holes-from-end slot chosen runners-up))))

(defun %match-mode-elements (tokens elements i end &optional require-end)
  "Match ELEMENTS (a suffix of some mode's pattern) against TOKENS from
position I (bounded by END). Returns (VALUES asts choices next-i okp
failure-token message selections score): on success ASTS is the list of EXPR-* ASTs parsed
from each :EXPR hole and CHOICES the parallel, HOLE-ALIGNED list -- one entry
per hole in ASTS, NIL for a hole not governed by any :ONE-OF, or the chosen
MODE-DESCRIPTOR for a hole that came from one (#104) -- both in pattern
order, and NEXT-I the token position just past the match; on failure OKP is
NIL and FAILURE-TOKEN/MESSAGE describe why.

CHOICES is always the same length as ASTS: a multi-hole :ONE-OF alternative
contributes its own chosen MODE-DESCRIPTOR to *every* hole it produces, not
just one entry for the element as a whole -- this is what lets a caller
(DEFINSTRUCTION's word-encoded (CHOICE M) selector, instruction.lisp) key
directly off hole position, the same position (OPERAND ...) subclauses
already use. When an alternative's own pattern nests another :ONE-OF, the
*outer* element's chosen alternative overwrites whatever the nested match
would have reported for those holes -- the outermost :ONE-OF a hole belongs
to always wins its CHOICES entry, preserving \"CHOICES[i] is one of the
alternatives named by the pattern element that produced hole i\" as an
invariant callers can validate against (mirrored by mode.lisp's
%MODE-HOLE-ALTERNATIVES, the pattern-only version of this same walk). Nested
:ONE-OF is legal (BUILD-MODE-DESCRIPTOR's hole-count check treats it like a
plain :EXPR) -- see tests/mode.lisp for coverage of both the hole-counting
and this outermost-wins CHOICES behavior.

A hand-written DEFMODE cycle -- redefining a mode that some :ONE-OF already
references so the reference loops back to it -- is guarded against
elsewhere: %MODE-HOLE-COUNT (#115) signals rather than recursing forever
when a mode name reappears on its own recursion path. A plain file reload
can't create a cycle, since it replays the same patterns in the same order.

Alternatives match with the remaining pattern and input. The successful path
with the most literal tokens wins; declaration order breaks ties. The eighth
return value is that path's literal count. The tenth lists one
%ONE-OF-TIE-RECORD per :ONE-OF element on the winning path whose pick was
decided by declaration order."
  (if (null elements)
       (if (and require-end (< i end))
           (values nil nil nil nil (%tok tokens i end) "Unexpected trailing token in operand")
           (values nil nil i t nil nil nil 0 nil nil))
      (let ((element (first elements)) (rest-elements (rest elements)))
        (ecase (first element)
          (:literal
           (let ((tok (%tok tokens i end)))
             (if (and tok (string-equal (token-text tok) (second element)))
            (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
                (%match-mode-elements tokens rest-elements (1+ i) end require-end)
                (if okp
                        (values asts choices next-i t nil nil selections (1+ score) suffixes ties)
                        (values nil nil nil nil failure-token message)))
                 (values nil nil nil nil tok
                         (format nil "Operand does not match addressing mode ~
(expected ~S~@[, found ~S~])"
                                 (second element) (and tok (token-text tok)))))))
           (:expr
            (let* ((register (second element))
                   (prefix (and (eq (token-type (or (%tok tokens i end) (make-token))) :hole-prefix)
                                (token-value (%tok tokens i end))))
                   (start (if prefix (1+ i) i))
                  (last-failure-token nil)
                  (last-message nil)
                  (best nil)
                  (best-score -1))
              (labels ((valid-register-p (ast)
                         (and register
                              (expr-label-p ast)
                              *register-alias-elements*
                              (let ((owner (gethash (expr-label-name ast)
                                                    *register-alias-elements*)))
                                (and owner (string-equal (symbol-name (storage-element-name owner))
                                                         (symbol-name register))))))
                       (try (ast next-i-hole)
                         (if (or (null register) (valid-register-p ast))
                              (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
                                  (%match-mode-elements tokens rest-elements next-i-hole end require-end)
                                (if okp
                                    (when (> score best-score)
                                      (setf best (list (cons ast asts) (cons nil choices) next-i
                                                       t nil nil selections score (cons prefix suffixes) ties)
                                            best-score score))
                                   (setf last-failure-token failure-token
                                         last-message message)))
                             (setf last-failure-token (%tok tokens i end)
                                   last-message (format nil "Expected an alias of register bank ~S"
                                                        register)))))
                (handler-case
                    (multiple-value-bind (ast next-i-hole)
                        (parse-expression tokens :start start :end end)
                      (try ast next-i-hole)
                      ;; The expression parser intentionally remains greedy. A literal
                      ;; plus immediately following this hole is the one contextual
                      ;; separator for which mode matching retries a shorter prefix.
                      (when (and (first rest-elements)
                                 (eq (first (first rest-elements)) :literal)
                                 (string= (second (first rest-elements)) "+"))
                        (loop for split from (1+ start) below next-i-hole
                              when (eq (%punct-value (%tok tokens split end)) :plus)
                              do (handler-case
                                     (multiple-value-bind (short short-next)
                                         (parse-expression tokens :start start :end split)
                                       (when (= short-next split)
                                         (try short short-next)))
                                   (parse-failure () nil))))
                      (if best
                          (values-list best)
                          (values nil nil nil nil last-failure-token last-message)))
                  (parse-failure (c)
                    (values nil nil nil nil
                            (make-token :line (lasm-syntax-error-line c)
                                        :column (lasm-syntax-error-column c))
                            (lasm-syntax-error-message c)))))))
          (:one-of
           (let* ((tok (%tok tokens i end))
                  (alt-prefix (and tok (eq (token-type tok) :hole-prefix)
                                   (find-if (lambda (n) (let ((sfx (mode-descriptor-suffix (find-mode-descriptor n))))
                                                          (and sfx (string-equal sfx (token-value tok)))))
                                            (%one-of-alternatives element))
                                   (token-value tok)))
                  (start (if alt-prefix (1+ i) i))
                  last-failure-token last-message best best-alt (best-score -1) tied)
             (dolist (alt-name (%one-of-alternatives element))
               (let ((alt (find-mode-descriptor alt-name)))
                 (when (or (null alt-prefix)
                           (and (mode-descriptor-suffix alt)
                                (string-equal (mode-descriptor-suffix alt) alt-prefix)))
                  (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
                      (%match-mode-elements tokens
                                            (append (mode-descriptor-pattern alt) rest-elements)
                                            start end require-end)
                    (cond
                      ((not okp) (setf last-failure-token failure-token last-message message))
                      ((= score best-score) (cl:push alt tied))
                      ((> score best-score)
                       (let* ((slot (%one-of-slot element))
                              (count (%mode-hole-count alt))
                              (selection (and slot (cons slot (mode-descriptor-name alt)))))
                         (setf best-score score
                               best-alt alt
                               tied nil
                               best
                               (list asts
                                     (append (make-list count :initial-element alt)
                                             (nthcdr count choices))
                                     next-i t nil nil
                                     (if slot
                                         (cons selection
                                               (remove slot selections :key #'car :test #'eq))
                                         selections)
                                     score suffixes
                                     (cons (length asts) ties))))))))))
             (when best
               ;; The tenth entry holds (holes-from-end . ties) until the
               ;; tied set is final.
               (destructuring-bind (holes-from-end . ties) (tenth best)
                 (let ((record (and (null alt-prefix)
                                    (%one-of-tie-record holes-from-end (%one-of-slot element)
                                                        best-alt (reverse tied)))))
                   (setf (tenth best) (if record (cons record ties) ties)))))
             (if best
                 (values-list best)
                 (values nil nil nil nil last-failure-token last-message))))))))

(defun %match-mode-pattern (tokens mode)
  "Match the SIMPLE-VECTOR TOKENS against MODE's pattern from the start.
Returns (VALUES asts okp failure-token message choices): on success ASTS is
the list of EXPR-* ASTs parsed from each :EXPR hole (in pattern order) and
OKP is T; on failure OKP is NIL and FAILURE-TOKEN/MESSAGE describe why --
FAILURE-TOKEN is NIL only when the underlying PARSE-FAILURE (an :EXPR hole's
own malformed expression) itself carried no token to point at (e.g. an empty
expression at end of input), never as a way of discarding a position that
was available. CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole in ASTS, NIL for a hole not governed by any
:ONE-OF -- see %MATCH-MODE-ELEMENTS -- a trailing value existing callers
that only bind the first four are unaffected by."
  (let ((end (length tokens)))
    (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
        (%match-mode-elements tokens (mode-descriptor-pattern mode) 0 end t)
      (declare (ignore next-i score))
      (cond
         ((not okp) (values nil nil failure-token message nil nil nil nil))
         ;; The sixth value is intentionally new. Existing callers only bind
         ;; the hole-aligned CHOICES value; named ONE-OF slots use this
         ;; additional selection metadata, including zero-hole alternatives.
          (t (values asts t nil nil choices selections suffixes
                     (loop for (holes-from-end . rest) in ties
                           collect (cons (- (length asts) holes-from-end) rest))))))))

(defun try-match-operand-mode (tokens mode)
  "Like MATCH-OPERAND-MODE, but returns (VALUES asts T choices) on a match or
(VALUES NIL NIL NIL) on a mismatch instead of signalling -- the assembler's
mode candidate filter (assembler.lisp) uses this to try several modes in
turn. MODE, like MATCH-OPERAND-MODE's, may be a MODE-DESCRIPTOR or a symbol
naming one. CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole, NIL for a hole not governed by any :ONE-OF --
see %MATCH-MODE-ELEMENTS. The fifth value is the hole-aligned list of
forcing-prefix names written before each hole (a string, or NIL). The sixth
lists (HOLE SLOT CHOSEN . RUNNERS-UP) for each ONE-OF pick decided by
declaration order alone, HOLE being the element's first hole index."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message choices selections suffixes ties) (%match-mode-pattern tokens mode)
      (declare (ignore failure-token message))
       (if okp (values asts t choices selections suffixes ties) (values nil nil nil nil nil nil)))))

(defun match-operand-mode (tokens mode)
  "Match TOKENS (a SIMPLE-VECTOR of raw tokens, e.g. an OPERAND's TOKENS or a
STATEMENT's OPERAND-TOKENS) against addressing MODE's pattern (a
MODE-DESCRIPTOR, or a symbol naming one): consume MODE's literal tokens in
order and parse each :EXPR hole as an expression. Returns (VALUES first-ast
all-asts choices) -- FIRST-AST alone is what every current single-hole mode
needs; CHOICES (#103, hole-aligned per #104) is the list of chosen
MODE-DESCRIPTORs, one per hole, NIL for a hole not governed by any :ONE-OF --
see %MATCH-MODE-ELEMENTS. Signals PARSE-FAILURE
if TOKENS don't match MODE or leave a trailing token unconsumed -- with the
failing token's own line/column (#74), not just its message, even when the
failure came from a nested :EXPR hole's own PARSE-FAILURE rather than a
literal mismatch."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message choices selections) (%match-mode-pattern tokens mode)
      (unless okp
        (%parse-error failure-token message))
       (values (first asts) asts choices selections))))

;;; Built-in modes -- M1's *BUILTIN-MODE-PREFIXES* table, expressed as
;;; ordinary DEFMODE forms. "#" already lexes to :HASH (lexer.lisp) for
;;; exactly this purpose.

(defmode immediate "#" expr :width 1)
;; ZERO-PAGE/ABSOLUTE (#40): the only two built-in modes that share operand
;; syntax (a bare expr) and so are the only pair relaxation ever has to pick
;; between -- each gets a suffix ("z"/"w") so a program can force one over
;; the other. IMMEDIATE/INDEXED-X/INDIRECT-Y/RELATIVE below are already
;; syntactically unambiguous, so a suffix would buy them nothing.
(defmode zero-page expr :width 1 :suffix "z")
(defmode absolute expr :suffix "w")
(defmode indexed-x expr "," "X")
(defmode indirect-y "(" expr ")" "," "Y")

;; RELATIVE (#23): syntactically identical to ABSOLUTE (a bare expr), but its
;; operand is a signed offset from the address of the *next* instruction, not
;; an absolute target -- computed by the assembler once layout has placed
;; both the branch and its target (assembler.lisp's %ENCODE). :RELATIVE T
;; implies :SIGNED T (#30): the emulator sign-extends it on fetch
;; (emulator.lisp's STEP-MACHINE) so semantics can write (set! pc (+ pc
;; operand)) with no width of its own to worry about. :WIDTH 1 is only this
;; mode's default -- a machine with wider branches overrides it per
;; instruction via the existing (operand :width n).
(defmode relative expr :width 1 :relative t)

;; STACK-RELATIVE (#50): syntactically "n,S" -- an expr hole followed by the
;; literal ",S", 6502/65816-flavoured like INDEXED-X/INDIRECT-Y above. Like
;; every mode, this is pure operand *syntax*: the parsed value is just an
;; offset, and it says nothing about which stack it indexes into or what that
;; offset means -- an instruction's semantics resolves it against a named
;; stack via STACK-REF (storage.lisp), with STACK-REF's own top-relative,
;; unsigned convention (offset 0 = the top). No :SUFFIX -- unlike ZERO-PAGE/
;; ABSOLUTE, this mode shares its syntax with no other built-in mode, so
;; there is nothing for a forced suffix to disambiguate.
(defmode stack-relative expr "," "S" :width 1)
