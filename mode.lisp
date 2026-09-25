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
;;;; Modes live in a global registry (mirroring *LEXERS*, lexer.lisp), plus an
;;;; optional per-machine table: (DEFMODE (NAME (:MACHINE M)) ...) registers a
;;;; mode only M and its descendants see, shadowing a global of the same name.
;;;; Names resolve through *MODE-SCOPE*, the machine being defined, assembled
;;;; or decoded (#29).

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
    machine    ; machine name for a machine-local mode (#29), or nil for a global one
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
    shape-cache)  ; (GENERATION SCOPE VARYING KEYED STRICTP), read through %MODE-SHAPE
  )

;; Registry of defined addressing modes, keyed by name -- mirrors *LEXERS*
;; (lexer.lisp) and *MACHINES* (storage.lisp). Unlike those two, this needs
;; an EVAL-WHEN around its own DEFVAR: DEFMODE's registration runs at
;; :COMPILE-TOPLEVEL (below), and this file defines its own built-in modes
;; (IMMEDIATE, ABSOLUTE, ...) at the bottom of this same file -- a plain
;; top-level DEFVAR's initial value is only guaranteed set at load time, which
;; is too late for a DEFMODE compiled later in the same compilation unit.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *modes* (make-hash-table :test 'eq))
  ;; Bumped by every DEFMODE, invalidating the cached %MODE-SHAPE of modes
  ;; that reference a redefined one.
  (defvar *mode-generation* 0)
  (defvar *shape-in-progress* nil)
  ;; Machine name -> (mode name -> descriptor) for machine-local modes (#29).
  (defvar *machine-modes* (make-hash-table :test 'eq))
  (defvar *mode-scope* nil
    "The machine whose local modes shadow the global ones, or NIL for globals only."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %scope-chain (scope)
    "SCOPE and its :EXTENDS ancestors, nearest first."
    (and scope (cons scope (%machine-ancestors scope))))

  (defun %lookup-mode (name scope)
    (or (loop for machine in (%scope-chain scope)
              for table = (gethash machine *machine-modes*)
              thereis (and table (gethash name table)))
        (gethash name *modes*)))

  (defun %visible-modes (scope)
    "Every mode SCOPE resolves a name to: local modes nearest-first, then unshadowed globals."
    (let (result seen)
      (dolist (machine (%scope-chain scope))
        (let ((table (gethash machine *machine-modes*)))
          (when table
            (maphash (lambda (name mode)
                       (unless (member name seen)
                         (cl:push name seen)
                         (cl:push mode result)))
                     table))))
      (maphash (lambda (name mode)
                 (unless (member name seen) (cl:push mode result)))
               *modes*)
      result))

  (defun %mode-table (machine)
    (if machine
        (or (gethash machine *machine-modes*)
            (setf (gethash machine *machine-modes*) (make-hash-table :test 'eq)))
        *modes*)))

(defun find-mode-descriptor (name &optional (scope *mode-scope*))
  "Look up the MODE-DESCRIPTOR registered under NAME (a symbol) with DEFMODE.
A machine-local mode of SCOPE (a machine name) or one of its ancestors shadows
a global one. Signals an error if none is registered."
  (or (%lookup-mode name scope)
      (%lookup-error 'unknown-mode name "No addressing mode named ~S has been defined with DEFMODE" name)))

;; Both wrapped in an EVAL-WHEN, like BUILD-MODE-DESCRIPTOR itself (below) --
;; BUILD-MODE-DESCRIPTOR calls %CHECK-SUFFIX-COLLISION, which calls
;; FIND-MODE-BY-SUFFIX, at :COMPILE-TOPLEVEL time for this same file's own
;; built-in DEFMODE forms (bottom of this file), so a plain DEFUN (only
;; guaranteed callable at load time) would be too late.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-mode-by-suffix (suffix &optional (scope *mode-scope*))
    "Look up the MODE-DESCRIPTOR visible from SCOPE whose :SUFFIX (a string, #40)
equals SUFFIX case-insensitively, or NIL if none declares one. A linear scan
rather than a second suffix -> descriptor table -- there are only a handful
of modes registered at once, and a parallel table would need its own
invalidation whenever a DEFMODE is redefined dropping (or changing) its
suffix. Used by the assembler's forced-mode operand syntax (assembler.lisp's
%CHOOSE-VARIANT) to resolve e.g. \"w\" in \"lda.w\" to ABSOLUTE."
    (find-if (lambda (mode)
               (and (mode-descriptor-suffix mode)
                    (string-equal (mode-descriptor-suffix mode) suffix)))
             (%visible-modes scope)))

  (defun %check-suffix-collision (name suffix machine)
    "Signal an error if SUFFIX (non-NIL) is already claimed, as seen from MACHINE,
by a mode other than NAME -- e.g. two DEFMODE forms both declaring :SUFFIX \"w\"
would make FIND-MODE-BY-SUFFIX's lookup ambiguous. A global mode is checked
against every machine's view as well. Compares by MODE-DESCRIPTOR-NAME, not
object identity: DEFMODE re-registering the same NAME (a plain file reload,
e.g. under ASDF) builds a fresh MODE-DESCRIPTOR struct each time, so an EQ
check would spuriously reject a mode reclaiming its own suffix."
    (when suffix
      (dolist (scope (if machine
                         (list machine)
                         (cons nil (loop for m being the hash-keys of *machine-modes* collect m))))
        (let ((existing (find-mode-by-suffix suffix scope)))
          (when (and existing (not (eq (mode-descriptor-name existing) name)))
            (%defmode-error "DEFMODE ~S: suffix ~S is already used by mode ~S"
                            name suffix (mode-descriptor-name existing))))))))

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
                 (%definition-bind (expr &key register (relative nil relative-given-p)
                                             (signed nil signed-given-p)) el
                   (declare (ignore expr))
                   (when (and register (not (symbolp register)))
                     (%defmode-error "Malformed DEFMODE expression hole ~S -- :REGISTER needs a register name" el))
                   (when (and (member :register (rest el)) (null register))
                     (%defmode-error "Malformed DEFMODE expression hole ~S -- :REGISTER needs a register name" el))
                   (when (and relative-given-p (not (member relative '(nil t))))
                     (%defmode-error "Malformed DEFMODE expression hole ~S -- :RELATIVE must be T or NIL" el))
                   (when (and signed-given-p (not (member signed '(nil t))))
                     (%defmode-error "Malformed DEFMODE expression hole ~S -- :SIGNED must be T or NIL" el))
                   (when (and relative (eq signed nil) signed-given-p)
                     (%defmode-error "DEFMODE expression hole ~S: :RELATIVE T implies :SIGNED T" el))
                   (append (list :expr register)
                           (when relative-given-p (list :relative relative))
                           (when signed-given-p (list :signed signed)))))
                 ((%one-of-element-p el)
                  (let ((slot (%one-of-slot el))
                        (alternatives (%one-of-alternatives el)))
                    (unless (and (>= (length alternatives) 2)
                                 (every #'symbolp alternatives))
                      (%defmode-error "Malformed DEFMODE pattern element ~S -- (ONE-OF ...) needs at ~
least two mode-name symbols" el))
                    (if slot
                        (list* :one-of :named slot alternatives)
                        (list* :one-of alternatives))))
                (t (%defmode-error "Malformed DEFMODE pattern element ~S -- expected a string ~
literal, the symbol EXPR, or (ONE-OF mode...)" el))))
            elements))

  (defun %split-mode-clause (body)
    "Split DEFMODE's BODY into (VALUES pattern-elements keyword-plist). Keyword
options start at the first keyword symbol; everything before it is pattern."
    (let ((pos (position-if #'keywordp body)))
      (if pos
          (values (subseq body 0 pos) (subseq body pos))
          (values body nil))))

  (defun %key-head (key)
    (if (consp key) (first key) key))

  (defun %key-subkeys (key)
    "The subkeys of an option KEY, one per keyed ONE-OF of its head alternative."
    (and (consp key) (rest key)))

  (defun %key-names (key)
    "Every mode name in KEY, outermost first."
    (if (consp key)
        (cons (first key) (mapcan #'%key-names (rest key)))
        (list key)))

  (defun %cartesian (lists)
    "Every choice of one element from each of LISTS, the first list varying slowest."
    (if (null lists)
        (list nil)
        (loop for x in (first lists)
              append (mapcar (lambda (more) (cons x more)) (%cartesian (rest lists))))))

  (defun %element-min-hole-count (element &optional seen)
    (reduce #'min (mapcar #'cdr (%one-of-element-options element seen))))

  (defun %option-hole-count (key &optional seen)
    "Hole count of an option KEY (see %ONE-OF-ELEMENT-OPTIONS)."
    (if (consp key)
        (let ((alt (find-mode-descriptor (first key))))
          (+ (%mode-hole-count alt seen)
             (loop for element in (%mode-keyed-elements alt)
                   for sub in (rest key)
                   sum (- (%option-hole-count sub seen) (%element-min-hole-count element seen)))))
        (%mode-hole-count (find-mode-descriptor key) seen)))

  (defun %one-of-element-options (element &optional seen)
    "((KEY . HOLE-COUNT)...) for ELEMENT's alternatives in declaration order.
An alternative with no keyed ONE-OF is keyed by its name; one with keyed
ONE-OFs contributes one key (NAME SUBKEY...) per combination of options of
those elements, one subkey for each, in pattern order."
    (loop for name in (%one-of-alternatives element)
          for alt = (find-mode-descriptor name)
          for keyed = (%mode-keyed-elements alt)
          append (if keyed
                     (loop for subs in (%cartesian
                                        (mapcar (lambda (inner)
                                                  (mapcar #'car (%one-of-element-options inner seen)))
                                                keyed))
                           collect (let ((full (cons name subs)))
                                     (cons full (%option-hole-count full seen))))
                     (list (cons name (%mode-hole-count alt seen))))))

  (defun %pattern-varying-one-of-elements (pattern &optional seen)
    "The ONE-OF elements of PATTERN whose options disagree on hole count."
    (remove-if-not (lambda (element)
                     (and (eq (first element) :one-of)
                          (> (length (remove-duplicates
                                      (mapcar #'cdr (%one-of-element-options element seen))))
                             1)))
                   pattern))

  (defun %pattern-varying-one-of-element (pattern &optional seen)
    (first (%pattern-varying-one-of-elements pattern seen)))

  (defun %option-signature (key)
    "What a hole can differ in across options: the hole count and each per-hole attribute."
    (cons (%option-hole-count key)
          (mapcar (lambda (attribute) (%option-hole-attributes key attribute))
                  '(:signed :relative :width :strict))))

  (defun %one-of-element-keyed-p (element &optional seen)
    "T if ELEMENT's options differ in hole count or in a per-hole attribute, so its
pick must be recorded in a key tree."
    (> (length (remove-duplicates
                (mapcar (lambda (option) (%option-signature (car option)))
                        (%one-of-element-options element seen))
                :test #'equal))
       1))

  (defun %pattern-keyed-one-of-elements (pattern &optional seen)
    (remove-if-not (lambda (element)
                     (and (eq (first element) :one-of) (%one-of-element-keyed-p element seen)))
                   pattern))

  (defun %mode-shape (mode)
    "(VARYING KEYED STRICTP) for MODE: the ONE-OF elements of its pattern whose
options disagree on hole count, those that disagree on any per-hole attribute or
hole count (the ones an option key names a subkey for), and whether a hole of it
can be strict. Recomputed when a DEFMODE has run since the last call, so
redefining an inner mode is seen by its dependents."
    (let ((cache (mode-descriptor-shape-cache mode))
          (name (mode-descriptor-name mode)))
      (if (and cache (eql (car cache) *mode-generation*) (eq (cadr cache) *mode-scope*))
          (cddr cache)
          (progn
            (when (member name *shape-in-progress*)
              (%defmode-error "DEFMODE ~S: ONE-OF cycle -- ~{~S~^ -> ~} -> ~S references itself"
                              name (reverse *shape-in-progress*) name))
            (let* ((*shape-in-progress* (cons name *shape-in-progress*))
                   (pattern (mode-descriptor-pattern mode))
                   (shape (list (%pattern-varying-one-of-elements pattern)
                                (%pattern-keyed-one-of-elements pattern)
                                (or (mode-descriptor-strictp mode)
                                    (loop for element in pattern
                                          thereis (and (eq (first element) :one-of)
                                                       (some (lambda (alt)
                                                               (%mode-strict-reachable-p (find-mode-descriptor alt)))
                                                             (%one-of-alternatives element))))))))
              ;; TODO: one cache entry per mode thrashes when scopes alternate, keep one per scope if it shows up (#281)
              (setf (mode-descriptor-shape-cache mode) (list* *mode-generation* *mode-scope* shape))
              shape)))))

  (defun %mode-varying-elements (mode) (first (%mode-shape mode)))
  (defun %mode-keyed-elements (mode) (second (%mode-shape mode)))
  (defun %mode-strict-reachable-p (mode) (and (third (%mode-shape mode)) t))

  (defun mode-descriptor-varyingp (mode)
    "T if MODE's pattern has a varying ONE-OF element: one whose options
(%ONE-OF-ELEMENT-OPTIONS) disagree on hole count."
    (and (%mode-varying-elements mode) t))

  (defun mode-descriptor-keyedp (mode)
    "T if an option key for MODE is a tree: a ONE-OF of its pattern has options
that differ in hole count or in a per-hole attribute."
    (and (%mode-keyed-elements mode) t))

  (defun %choice-entry-key (entry)
    "The option key for a matcher CHOICES ENTRY: a descriptor, or a tree
(DESCRIPTOR ENTRY...) for a varying alternative."
    (if (consp entry)
        (cons (mode-descriptor-name (first entry)) (mapcar #'%choice-entry-key (rest entry)))
        (mode-descriptor-name entry)))

  (defun %choice-key-descriptor (key)
    (find-mode-descriptor (%key-head key)))

  (defun %key-declares-signed-p (key)
    "T if KEY's alternative, or any alternative in its tree, makes a hole signed."
    (or (mode-descriptor-signedp (%choice-key-descriptor key))
        (some #'identity (%option-hole-attributes key :signed))))

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
        (%defmode-error "DEFMODE ~S: ONE-OF cycle -- ~{~S~^ -> ~} -> ~S references itself"
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
                    (:one-of (let* ((options (%one-of-element-options element seen))
                                    (holes (reduce #'min (mapcar #'cdr options))))
                              (make-list holes :initial-element (mapcar #'car options)))))))

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
        (%defmode-error "DEFMODE ~S: a relative EXPR hole cannot be unsigned"
               (mode-descriptor-name mode)))
      (ecase attribute
        (:relative relative)
        (:signed (or relative signed))
        (:width (mode-descriptor-width mode))
        (:strict (mode-descriptor-strictp mode)))))

  (defun %option-hole-attributes (key attribute)
    (if (consp key)
        (%mode-hole-attributes (find-mode-descriptor (first key)) attribute (rest key))
        (%mode-hole-attributes (find-mode-descriptor key) attribute)))

  (defun %mode-hole-attributes (mode attribute &optional subkeys)
    "Return default attributes in pattern order. SUBKEYS names the option taken
by each of MODE's keyed ONE-OF elements, in pattern order. A hole a ONE-OF
contributes takes MODE's own :WIDTH or :STRICT when its alternative has none."
    (let ((keyed (%mode-keyed-elements mode))
          (fallback (case attribute
                      (:width (mode-descriptor-width mode))
                      (:strict (mode-descriptor-strictp mode)))))
      (loop for element in (mode-descriptor-pattern mode)
            append (ecase (first element)
                     (:literal nil)
                     (:expr (list (%expr-hole-attribute element mode attribute)))
                     (:one-of
                      (let* ((position (position element keyed :test #'eq))
                             (holes (%option-hole-attributes
                                     (if (and subkeys position)
                                         (nth position subkeys)
                                         (car (first (%one-of-element-options element))))
                                     attribute)))
                        (if fallback
                            (mapcar (lambda (value) (or value fallback)) holes)
                            holes)))))))

  (defun %check-one-of-elements! (name pattern)
    "Validate alternative syntax and supported ONE-OF nesting.
Alternatives may vary in arity and operand attributes; DEFINSTRUCTION
validates encoding support. Ambiguous syntax is rejected, as is a hole-less
inner option of an unnamed ONE-OF, or wrapper options on an alternative selected by a tree."
    (dolist (element pattern)
      (when (eq (first element) :one-of)
         (let* ((alt-names (%one-of-alternatives element))
               (alts (mapcar #'find-mode-descriptor alt-names)))
          (when (< (length alts) 2)
            (%defmode-error "DEFMODE ~S: ONE-OF needs at least two alternative modes, got ~S"
                   name alt-names))
          (dolist (alt alts)
            (when (and (mode-descriptor-varyingp alt)
                       (some (lambda (inner)
                               (some (lambda (option) (zerop (cdr option)))
                                     (%one-of-element-options inner)))
                             (%mode-varying-elements alt))
                       (null (%one-of-slot element)))
              (%defmode-error "DEFMODE ~S: ONE-OF alternative ~S nests a varying ONE-OF with an ~
alternative that has no operand hole -- name the outer ONE-OF, (one-of (slot alternative...)), so ~
that pick can be selected"
                     name (mode-descriptor-name alt)))
            (when (and (mode-descriptor-keyedp alt)
                       (or (mode-descriptor-width alt) (mode-descriptor-signedp alt)
                           (mode-descriptor-relativep alt) (mode-descriptor-suffix alt)
                           (mode-descriptor-strictp alt)))
              (%defmode-error "DEFMODE ~S: ONE-OF alternative ~S is selected by a tree, so it cannot ~
declare :WIDTH, :SIGNED, :RELATIVE, :SUFFIX or :STRICT -- declare them on its holes or inner ~
alternatives instead"
                     name (mode-descriptor-name alt))))
          (loop for (alt . later) on alts
                do (dolist (other later)
                     (when (and (equalp (mode-descriptor-pattern alt) (mode-descriptor-pattern other))
                                (not (and (mode-descriptor-suffix alt) (mode-descriptor-suffix other))))
                       (%defmode-error "DEFMODE ~S: ONE-OF alternatives ~S and ~S have identical syntax ~
-- nothing could ever choose between them (give both a :SUFFIX to select by prefix)"
                              name (mode-descriptor-name alt) (mode-descriptor-name other)))))))))

  (defstruct mode-hole-group
    base-start
    start
    base-count
    count
    alternatives
    options
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
                          (options (and alts (%one-of-element-options element seen)))
                          (keys (mapcar #'car options))
                          (base-count (ecase (first element)
                                        (:literal 0)
                                        (:expr 1)
                                        (:one-of (reduce #'min (mapcar #'cdr options)))))
                          (minimum (loop for (key . count) in options
                                         when (= count base-count) collect key))
                          (over (loop for (key . count) in options
                                      unless (= count base-count) collect key))
                          (split-minimum-p (and (%one-of-slot element)
                                                (> (length minimum) 1))))
                     (loop for alt in (if split-minimum-p
                                          (append minimum over)
                                          (cons nil over))
                           for count = (if alt
                                           (cdr (assoc alt options :test #'equal))
                                           base-count)
                           append (walk (rest pattern) (+ base-start base-count) (+ start count)
                                        (if (or over split-minimum-p)
                                            (cons (make-mode-hole-group
                                                   :base-start base-start :start start
                                                   :base-count base-count :count count
                                                   :alternatives alts :options keys
                                                   :alt-name alt :slot (%one-of-slot element))
                                                  groups)
                                            groups)
                                        (append holes (make-list count :initial-element keys))
                                        (append sources
                                                (ecase (first element)
                                                  (:literal nil)
                                                  (:expr (list (list :expr element)))
                                                  (:one-of
                                                   (loop for index below count
                                                         collect (list :one-of keys index alt)))))))))))
      (walk (mode-descriptor-pattern mode) 0 0 nil nil nil)))

  (defun %hole-source-attribute (mode source attribute &optional choice)
    (ecase (first source)
      (:expr (%expr-hole-attribute (second source) mode attribute))
      (:one-of
       (nth (third source)
            (%option-hole-attributes (or choice (fourth source) (first (second source)))
                                     attribute)))))

  (defun %mode-hole-sources (mode)
    (mode-hole-tuple-hole-sources (first (%mode-hole-tuples mode))))

  (defun %mode-hole-count-range (mode &optional seen)
    "(VALUES MIN MAX) hole count across every one of MODE's alternative-tuples
(#120, %MODE-HOLE-TUPLES) -- MIN equals %MODE-HOLE-COUNT; both equal it for
a mode with no varying :ONE-OF element."
    (let ((counts (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple)))
                           (%mode-hole-tuples mode seen))))
      (values (reduce #'min counts) (reduce #'max counts))))

  (defun build-mode-descriptor (name body &optional machine)
  (%with-definition (name mode-definition-error)
      (multiple-value-bind (pattern-elements options) (%split-mode-clause body)
        (when (null pattern-elements)
          (%defmode-error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
        (let ((pattern (%parse-mode-pattern pattern-elements)))
          (%check-one-of-elements! name pattern)
          ;; A literal-only mode is useful as a fixed alternative in a ONE-OF.
          ;; It contributes no operand value; the enclosing instruction can
          ;; attach its encoding with a named choice slot.
          (%definition-bind (&key width relative signed suffix strict) options
            (when (and relative (not (eq signed t)) (member :signed options))
              (%defmode-error "DEFMODE ~S: :RELATIVE T implies :SIGNED T -- do not pass ~
:SIGNED NIL alongside it" name))
            (%check-suffix-collision name suffix machine)
            (let ((descriptor
                    (make-mode-descriptor :name name :machine machine :pattern pattern :width width
                                          :relativep relative :signedp (or relative signed)
                                          :suffix suffix :strictp strict)))
              (dolist (element pattern)
                (when (eq (first element) :expr)
                  (%expr-hole-attribute element descriptor :signed)))
              descriptor)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %mode-references (mode)
    (loop for element in (mode-descriptor-pattern mode)
          when (eq (first element) :one-of)
            append (%one-of-alternatives element)))

  (defun %mode-dependents (name &optional (scope *mode-scope*))
    "Modes visible from SCOPE that reference NAME through ONE-OF, transitively, innermost first."
    (let ((depths (make-hash-table :test 'eq)))
      (labels ((depth (mode-name visiting)
                 (multiple-value-bind (known foundp) (gethash mode-name depths)
                   (cond (foundp known)
                         ((eq mode-name name) 0)
                         ((member mode-name visiting) nil)
                         (t (let* ((mode (%lookup-mode mode-name scope))
                                   (inner (and mode
                                               (loop for ref in (%mode-references mode)
                                                     for d = (depth ref (cons mode-name visiting))
                                                     when d collect d))))
                              (setf (gethash mode-name depths)
                                    (and inner (1+ (reduce #'max inner))))))))))
        (let (result)
          (dolist (mode (%visible-modes scope))
            (let ((mode-name (mode-descriptor-name mode)))
              (unless (eq mode-name name)
                (let ((d (depth mode-name nil)))
                  (when d (cl:push (cons d mode-name) result))))))
          (mapcar #'cdr (stable-sort (sort result #'string< :key (lambda (e) (symbol-name (cdr e))))
                                     #'< :key #'car))))))

  (defun %mode-signature (mode)
    (list (mode-descriptor-pattern mode) (mode-descriptor-width mode)
          (mode-descriptor-relativep mode) (mode-descriptor-signedp mode)
          (mode-descriptor-strictp mode) (mode-descriptor-suffix mode)))

  (defun %instructions-using-modes (mode-names machines)
    "((MACHINE . MNEMONIC)...) of instructions defined on MACHINES whose mode is in MODE-NAMES."
    (let (result)
      (maphash (lambda (machine md)
                 (when (member machine machines)
                   (maphash (lambda (mnemonic descriptors)
                              (dolist (d descriptors)
                                (let ((mode (instruction-descriptor-mode d)))
                                  (when (and mode (member (mode-descriptor-name mode) mode-names))
                                    (pushnew (cons (instruction-descriptor-machine d) mnemonic) result
                                             :test #'equal)))))
                            (machine-descriptor-instructions md))))
               *machines*)
      (nreverse result)))

  (defun %machines-seeing-mode (mode)
    "Machines whose scope resolves MODE's name to MODE itself."
    (loop for machine being the hash-keys of *machines*
          when (eq (%lookup-mode (mode-descriptor-name mode) machine) mode)
            collect machine))

  (defun %recheck-mode-dependents! (mode)
    "Warn (STALE-MODE) about modes that reference the redefined MODE and no
longer validate, and about instructions compiled against MODE or its dependents.
A machine-local MODE is checked for its machine and descendants; a global one
for the global scope and every machine that does not shadow it."
    (let* ((name (mode-descriptor-name mode))
           (machines (%machines-seeing-mode mode))
           (scopes (if (mode-descriptor-machine mode) machines (cons nil machines)))
           (warned nil)
           (all-dependents nil))
      (dolist (scope scopes)
        (let ((*mode-scope* scope))
          (dolist (dependent (%mode-dependents name scope))
            (pushnew dependent all-dependents)
            (let ((failure (handler-case
                               (progn (%check-one-of-elements!
                                       dependent (mode-descriptor-pattern (find-mode-descriptor dependent)))
                                      nil)
                             (lasm-error (c) c))))
              (when (and failure (not (member dependent warned)))
                (cl:push dependent warned)
                (warn 'stale-mode :mode name :dependents (list dependent)
                                  :message (format nil "Redefining mode ~S invalidates mode ~S: ~A"
                                                   name dependent failure)))))))
      (let* ((dependents (nreverse all-dependents))
             (instructions (%instructions-using-modes (cons name dependents) machines)))
        (when instructions
          (warn 'stale-mode :mode name :dependents dependents :instructions instructions
                            :message (format nil "Redefining mode ~S leaves instructions built against its old shape: ~
~{~A~^, ~} -- re-evaluate their DEFINSTRUCTIONs"
                                             name (mapcar (lambda (i) (format nil "~A ~A" (car i) (cdr i)))
                                                          instructions)))))))

  (defun %split-defmode-head (head)
    "(VALUES NAME MACHINE) for DEFMODE's NAME or (NAME (:MACHINE M))."
    (cond ((symbolp head) (values head nil))
          ((and (consp head) (symbolp (first head)) (first head)
                (equal (length head) 2) (consp (second head))
                (eq (first (second head)) :machine)
                (equal (length (second head)) 2) (symbolp (second (second head)))
                (second (second head)))
           (values (first head) (second (second head))))
          (t (%defmode-error "DEFMODE: the name must be a symbol or (NAME (:MACHINE M)), got ~S" head))))

  (defun %register-mode (head body)
    (multiple-value-bind (name machine) (%split-defmode-head head)
      (when (and machine (not (gethash machine *machines*)))
        (%defmode-error "DEFMODE ~S: no machine named ~S has been defined with DEFMACHINE" name machine))
      (let* ((*mode-scope* machine)
             (old (%lookup-mode name machine))
             (new (build-mode-descriptor name body machine)))
        (setf (gethash name (%mode-table machine)) new)
        (incf *mode-generation*)
        (when (and old (not (equalp (%mode-signature old) (%mode-signature new))))
          (%recheck-mode-dependents! new))
        name))))

(defmacro defmode (name &body pattern)
  "Define a mode from literal tokens, EXPR holes, and ONE-OF alternatives.
NAME may be (NAME (:MACHINE M)) to define a mode only machine M and its
descendants see, shadowing a global mode of the same name.
A hole may use (EXPR :REGISTER name :SIGNED boolean :RELATIVE boolean).
Hole options override mode-wide :SIGNED and :RELATIVE defaults. A relative
hole is signed, and any number of holes may be relative. :WIDTH supplies
the default operand width; :SUFFIX forces a mode at assembly time; :STRICT
checks ordinary operand ranges. See docs/modes.md."
  (%definition-toplevel-form `(%register-mode ',name ',pattern)
                             `',(if (consp name) (first name) name)))

;;; Pattern matching

(defun %score> (a b)
  "True when match score A, (LITERALS . REGISTER-HOLES), beats B, or B is NIL.
Literal count decides first; register-qualified hole count breaks its ties."
  (or (null b)
      (> (car a) (car b))
      (and (= (car a) (car b)) (> (cdr a) (cdr b)))))

(defvar *recorded-elements* nil
  "ONE-OF elements whose pick an enclosing keyed alternative will read back
from the matcher's selections (%NESTED-CHOICE-ENTRY).")

(defun %nested-choice-entry (alt selections)
  "The entry for ALT matched at a ONE-OF: ALT itself, or for a keyed ALT the
tree (ALT ENTRY...), one entry per keyed ONE-OF of ALT in pattern order. Each
inner pick comes from SELECTIONS, where the element records its own entry under
itself (%MATCH-MODE-ELEMENTS, when listed in *RECORDED-ELEMENTS*), so an inner
option with no hole is found too."
  (if (mode-descriptor-keyedp alt)
      (cons alt (mapcar (lambda (element) (cdr (assoc element selections :test #'eq)))
                        (%mode-keyed-elements alt)))
      alt))

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
%MODE-HOLE-ALTERNATIVES, the pattern-only version of this same walk). A
keyed nested alternative is the exception: its entry is a tree, the
descriptor followed by one entry for each of its keyed ONE-OFs
(%NESTED-CHOICE-ENTRY), matching an option key of %ONE-OF-ELEMENT-OPTIONS.

A hand-written DEFMODE cycle -- redefining a mode that some :ONE-OF already
references so the reference loops back to it -- is guarded against
elsewhere: %MODE-HOLE-COUNT (#115) signals rather than recursing forever
when a mode name reappears on its own recursion path. A plain file reload
can't create a cycle, since it replays the same patterns in the same order.

Alternatives match with the remaining pattern and input. The successful path
with the most literal tokens wins, then the one with the most matched
register-qualified holes; declaration order breaks remaining ties. The eighth
return value is that path's score, (LITERALS . REGISTER-HOLES). The tenth
lists a tie record (HOLES-FROM-END SLOT CHOSEN . RUNNERS-UP) per :ONE-OF
element on the winning path whose pick was decided by declaration order."
  (if (null elements)
       (if (and require-end (< i end))
           (values nil nil nil nil (%tok tokens i end) "Unexpected trailing token in operand")
           (values nil nil i t nil nil nil (cons 0 0) nil nil))
      (let ((element (first elements)) (rest-elements (rest elements)))
        (ecase (first element)
          (:literal
           (let ((tok (%tok tokens i end)))
             (if (and tok (string-equal (token-text tok) (second element)))
            (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
                (%match-mode-elements tokens rest-elements (1+ i) end require-end)
                (if okp
                        (values asts choices next-i t nil nil selections
                                (cons (1+ (car score)) (cdr score)) suffixes ties)
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
                  (best-score nil))
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
                                (when (and okp register)
                                  (setf score (cons (car score) (1+ (cdr score)))))
                                (if okp
                                    (when (%score> score best-score)
                                      (setf best (list (cons ast asts) (cons nil choices) next-i
                                                       t nil nil selections score (cons prefix suffixes) ties)
                                            best-score score))
                                   (setf last-failure-token failure-token
                                         last-message message)))
                             (setf last-failure-token (%tok tokens i end)
                                   last-message (format nil "Expected an alias of register bank ~S"
                                                        register)))))
                (flet ((retry-shorter (predicate upto)
                         ;; Retry shorter expressions ending before a "+", "<" or ">"
                         ;; that a delimiter literal may own rather than the operator.
                         (loop for split from (1+ start) below upto
                               when (funcall predicate (%punct-value (%tok tokens split end)))
                               do (handler-case
                                      (multiple-value-bind (short short-next)
                                          (parse-expression tokens :start start :end split)
                                        (when (= short-next split)
                                          (try short short-next)))
                                    (parse-failure () nil)))))
                  (handler-case
                      (multiple-value-bind (ast next-i-hole)
                          (handler-case (parse-expression tokens :start start :end end)
                            (parse-failure (c)
                              ;; A trailing "<"/">" delimiter parses as a comparison
                              ;; with no right operand.
                              (retry-shorter (lambda (p) (member p '(:lt :gt))) end)
                              (if best (return-from %match-mode-elements (values-list best)) (error c))))
                        (try ast next-i-hole)
                        ;; The expression parser intentionally remains greedy. A literal
                        ;; plus, less-than or greater-than immediately following this
                        ;; hole is a contextual separator for which mode matching
                        ;; retries a shorter prefix.
                        (when (and (first rest-elements)
                                   (eq (first (first rest-elements)) :literal))
                          (let ((literal (second (first rest-elements))))
                            (cond ((string= literal "+")
                                   (retry-shorter (lambda (p) (eq p :plus)) next-i-hole))
                                  ((member literal '("<" ">") :test #'string=)
                                   (retry-shorter (lambda (p) (member p '(:lt :gt))) next-i-hole)))))
                        (if best
                            (values-list best)
                            (values nil nil nil nil last-failure-token last-message)))
                    (parse-failure (c)
                      (values nil nil nil nil
                              (make-token :line (lasm-syntax-error-line c)
                                          :column (lasm-syntax-error-column c))
                              (lasm-syntax-error-message c))))))))
          (:one-of
           (let* ((tok (%tok tokens i end))
                  (alt-prefix (and tok (eq (token-type tok) :hole-prefix)
                                   (find-if (lambda (n) (let ((sfx (mode-descriptor-suffix (find-mode-descriptor n))))
                                                          (and sfx (string-equal sfx (token-value tok)))))
                                            (%one-of-alternatives element))
                                   (token-value tok)))
                  (start (if alt-prefix (1+ i) i))
                  last-failure-token last-message best (best-score nil) tied)
             (dolist (alt-name (%one-of-alternatives element))
               (let ((alt (find-mode-descriptor alt-name)))
                 (when (or (null alt-prefix)
                           (and (mode-descriptor-suffix alt)
                                (string-equal (mode-descriptor-suffix alt) alt-prefix)))
                  (multiple-value-bind (asts choices next-i okp failure-token message selections score suffixes ties)
                      (let ((*recorded-elements*
                              (if (mode-descriptor-keyedp alt)
                                  (append (%mode-keyed-elements alt) *recorded-elements*)
                                  *recorded-elements*)))
                        (%match-mode-elements tokens
                                              (append (mode-descriptor-pattern alt) rest-elements)
                                              start end require-end))
                    (cond
                      ((not okp) (setf last-failure-token failure-token last-message message))
                      ((equal score best-score) (cl:push alt tied))
                      ((%score> score best-score)
                       (let* ((slot (%one-of-slot element))
                                (entry (%nested-choice-entry alt selections))
                                (key (%choice-entry-key entry))
                                (count (%option-hole-count key))
                                (inner (%mode-keyed-elements alt))
                                (recordedp (member element *recorded-elements* :test #'eq))
                                (selections (let ((rest (if inner
                                                            (remove-if (lambda (selection)
                                                                         (member (car selection) inner :test #'eq))
                                                                       selections)
                                                            selections)))
                                              (if recordedp
                                                  (cons (cons element entry) rest)
                                                  rest))))
                         (setf best-score score
                               tied (list alt)
                               best
                               (list asts
                                     (append (make-list count :initial-element entry)
                                             (nthcdr count choices))
                                     next-i t nil nil
                                     (if (and slot (not recordedp))
                                         (cons (cons slot key)
                                               (remove slot selections :key #'car :test #'eq))
                                         selections)
                                     score suffixes
                                     (cons (length asts) ties))))))))))
             (when best
               ;; The tenth entry holds (holes-from-end . ties) until the
               ;; tied set is final.
               (destructuring-bind (holes-from-end . ties) (tenth best)
                 (setf (tenth best)
                       (if (and (null alt-prefix) (rest tied))
                           (cons (list* holes-from-end (%one-of-slot element) (reverse tied)) ties)
                           ties))))
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
      (declare (ignore next-i))
      (cond
         ((not okp) (values nil nil failure-token message nil nil nil nil (cons 0 0)))
         ;; The sixth value is intentionally new. Existing callers only bind
         ;; the hole-aligned CHOICES value; named ONE-OF slots use this
         ;; additional selection metadata, including zero-hole alternatives.
          (t (values asts t nil nil choices selections suffixes
                     (loop for (holes-from-end . rest) in ties
                           collect (cons (- (length asts) holes-from-end) rest))
                     score))))))

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
declaration order alone, HOLE being the element's first hole index. The
seventh is the match score, (LITERALS . REGISTER-HOLES), (0 . 0) on a
mismatch -- see %MATCH-MODE-ELEMENTS."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message choices selections suffixes ties score)
        (%match-mode-pattern tokens mode)
      (declare (ignore failure-token message))
       (if okp
           (values asts t choices selections suffixes ties score)
           (values nil nil nil nil nil nil (cons 0 0))))))

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
