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
               ; RELATIVE): the emulator sign-extends the fetched operand
               ; (emulator.lisp) before handing it to semantics, and the
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
    strictp)   ; T if an operand encoded through this mode that doesn't fit
               ; its own width is an ASSEMBLY-ERROR rather than silently
               ; wrapping (#74, absorbing #28/#43) -- checked at encode time
               ; by %ENCODE's :INSTRUCTION branch (assembler.lisp), alongside
               ; the *STRICT-OPERAND-RANGE* global switch (diagnostic.lisp)
               ; that makes every mode strict, including a mode-less
               ; instruction's bare operand. Default NIL preserves #28/#43's
               ; original wrap-on-overflow behavior.
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

(defun %mode-hole-count (mode)
  (count :expr (mode-descriptor-pattern mode) :key #'first))

;;; DEFMODE pattern parsing
;;;
;;; These run inside an EVAL-WHEN, not just plain DEFUNs, because this same
;;; file's built-in DEFMODE forms (bottom of this file) call BUILD-MODE-
;;; DESCRIPTOR at :COMPILE-TOPLEVEL time -- a plain DEFUN's body is only
;;; guaranteed callable at load time, which is too late within one
;;; compilation unit (see the DEFVAR comment above for the same issue).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-mode-pattern (elements)
    "ELEMENTS is DEFMODE's pattern spine: a mix of string literals and the
symbol EXPR. Returns a list of (:literal string) | (:expr) pattern elements,
plus any trailing keyword options untouched by this function (the caller
splits those off first)."
    (mapcar (lambda (el)
              (cond
                ((stringp el) (list :literal el))
                ((and (symbolp el) (string-equal (symbol-name el) "EXPR")) (list :expr))
                (t (error "Malformed DEFMODE pattern element ~S -- expected a string ~
literal or the symbol EXPR" el))))
            elements))

  (defun %split-mode-clause (body)
    "Split DEFMODE's BODY into (VALUES pattern-elements keyword-plist). Keyword
options start at the first keyword symbol; everything before it is pattern."
    (let ((pos (position-if #'keywordp body)))
      (if pos
          (values (subseq body 0 pos) (subseq body pos))
          (values body nil))))

  (defun build-mode-descriptor (name body)
    (multiple-value-bind (pattern-elements options) (%split-mode-clause body)
      (when (null pattern-elements)
        (error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
      (let ((pattern (%parse-mode-pattern pattern-elements)))
        (unless (find :expr pattern :key #'first)
          (error "DEFMODE ~S: pattern must include at least one EXPR hole" name))
        (destructuring-bind (&key width relative signed suffix strict) options
          (when (and relative (not (eq signed t)) (member :signed options))
            (error "DEFMODE ~S: :RELATIVE T implies :SIGNED T -- do not pass ~
:SIGNED NIL alongside it" name))
          (%check-suffix-collision name suffix)
          (make-mode-descriptor :name name :pattern pattern :width width
                                 :relativep relative
                                 :signedp (or relative signed)
                                 :suffix suffix
                                 :strictp strict))))))

(defmacro defmode (name &body pattern)
  "Define an addressing mode named NAME matching PATTERN, a sequence of
string literals and the symbol EXPR (one per operand hole), optionally
followed by :WIDTH n (a default operand byte width instructions using this
mode may omit from their own encoding), :SIGNED t (this mode's operand is a
signed quantity -- the emulator sign-extends it and the assembler
range-checks candidate values against the signed range; #30), :RELATIVE t
(this mode's operand is a PC-relative offset rather than an absolute value
-- see RELATIVE below and #23; implies :SIGNED t, so passing :SIGNED NIL
alongside :RELATIVE T is an error), and/or :SUFFIX \"s\" (a gas-style
mnemonic suffix, e.g. \"w\"/\"z\" -- a program can append SEPARATOR ++ s to
a mnemonic, e.g. \"lda.w\", to force this mode regardless of what the
operand's value folds to, bypassing relaxation entirely; #40, see
docs/modes.md), and/or :STRICT t (an operand that doesn't fit this mode's
own width is an ASSEMBLY-ERROR at encode time rather than silently wrapping
via WRAP-VALUE; #74, see docs/diagnostics.md -- *STRICT-OPERAND-RANGE*
makes every mode behave this way without marking any one of them). Signals
an error if SUFFIX is already claimed by a different mode. E.g.:

  (defmode immediate  \"#\" expr        :width 1)
  (defmode zero-page  expr            :width 1)
  (defmode absolute   expr)
  (defmode indexed-x  expr \",\" \"X\")
  (defmode indirect-y \"(\" expr \")\" \",\" \"Y\")
  (defmode signed-imm \"#\" expr        :width 1 :signed t)
  (defmode relative   expr            :width 1 :relative t)

Registers the resulting MODE-DESCRIPTOR under NAME in *MODES*, inside an
EVAL-WHEN so it is available at macroexpansion time like DEFMACHINE
(machine.lisp) -- DEFINSTRUCTION resolves mode names against this registry
when its own form is compiled."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *modes*) (build-mode-descriptor ',name ',pattern))
     ',name))

;;; Pattern matching

(defun %match-mode-pattern (tokens mode)
  "Match the SIMPLE-VECTOR TOKENS against MODE's pattern from the start.
Returns (VALUES asts okp failure-token message): on success ASTS is the list
of EXPR-* ASTs parsed from each :EXPR hole (in pattern order) and OKP is T;
on failure OKP is NIL and FAILURE-TOKEN/MESSAGE describe why -- FAILURE-TOKEN
is NIL only when the underlying PARSE-FAILURE (an :EXPR hole's own malformed
expression) itself carried no token to point at (e.g. an empty expression at
end of input), never as a way of discarding a position that was available."
  (let ((end (length tokens))
        (i 0)
        asts)
    (dolist (element (mode-descriptor-pattern mode))
      (ecase (first element)
        (:literal
         (let ((tok (%tok tokens i end)))
           (unless (and tok (string-equal (token-text tok) (second element)))
             (return-from %match-mode-pattern
               (values nil nil tok
                       (format nil "Operand does not match ~(~A~) addressing mode ~
(expected ~S~@[, found ~S~])"
                               (mode-descriptor-name mode) (second element)
                               (and tok (token-text tok))))))
           (incf i)))
        (:expr
         (handler-case
             (multiple-value-bind (ast next-i) (parse-expression tokens :start i :end end)
               (cl:push ast asts)
               (setf i next-i))
           ;; #74: keep the inner PARSE-FAILURE's own line/column instead of
           ;; only its message -- %TOK below can't reconstruct a token from
           ;; a bare string, so the failure token this returns is a
           ;; synthetic one carrying just enough (line/column) for
           ;; MATCH-OPERAND-MODE's %PARSE-ERROR to preserve position.
           (parse-failure (c)
             (return-from %match-mode-pattern
               (values nil nil
                       (make-token :line (lasm-syntax-error-line c)
                                   :column (lasm-syntax-error-column c))
                       (lasm-syntax-error-message c))))))))
    (if (< i end)
        (values nil nil (%tok tokens i end) "Unexpected trailing token in operand")
        (values (nreverse asts) t nil nil))))

(defun try-match-operand-mode (tokens mode)
  "Like MATCH-OPERAND-MODE, but returns (VALUES asts T) on a match or (VALUES
NIL NIL) on a mismatch instead of signalling -- the assembler's mode
candidate filter (assembler.lisp) uses this to try several modes in turn.
MODE, like MATCH-OPERAND-MODE's, may be a MODE-DESCRIPTOR or a symbol
naming one."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp) (%match-mode-pattern tokens mode)
      (if okp (values asts t) (values nil nil)))))

(defun match-operand-mode (tokens mode)
  "Match TOKENS (a SIMPLE-VECTOR of raw tokens, e.g. an OPERAND's TOKENS or a
STATEMENT's OPERAND-TOKENS) against addressing MODE's pattern (a
MODE-DESCRIPTOR, or a symbol naming one): consume MODE's literal tokens in
order and parse each :EXPR hole as an expression. Returns (VALUES first-ast
all-asts) -- FIRST-AST alone is what every current single-hole mode needs.
Signals PARSE-FAILURE if TOKENS don't match MODE or leave a trailing token
unconsumed -- with the failing token's own line/column (#74), not just its
message, even when the failure came from a nested :EXPR hole's own
PARSE-FAILURE rather than a literal mismatch."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message) (%match-mode-pattern tokens mode)
      (unless okp
        (%parse-error failure-token message))
      (values (first asts) asts))))

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
