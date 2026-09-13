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
    signedp))  ; T if this mode's operand is a signed quantity (#30, split off
               ; RELATIVE): the emulator sign-extends the fetched operand
               ; (emulator.lisp) before handing it to semantics, and the
               ; assembler's mode selector range-checks candidate values
               ; against the signed range rather than the unsigned one
               ; (assembler.lisp's %CHOOSE-VARIANT).

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
        (destructuring-bind (&key width relative signed) options
          (when (and relative (not (eq signed t)) (member :signed options))
            (error "DEFMODE ~S: :RELATIVE T implies :SIGNED T -- do not pass ~
:SIGNED NIL alongside it" name))
          (make-mode-descriptor :name name :pattern pattern :width width
                                 :relativep relative
                                 :signedp (or relative signed)))))))

(defmacro defmode (name &body pattern)
  "Define an addressing mode named NAME matching PATTERN, a sequence of
string literals and the symbol EXPR (one per operand hole), optionally
followed by :WIDTH n (a default operand byte width instructions using this
mode may omit from their own encoding), :SIGNED t (this mode's operand is a
signed quantity -- the emulator sign-extends it and the assembler
range-checks candidate values against the signed range; #30), and/or
:RELATIVE t (this mode's operand is a PC-relative offset rather than an
absolute value -- see RELATIVE below and #23; implies :SIGNED t, so passing
:SIGNED NIL alongside :RELATIVE T is an error). E.g.:

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
on failure OKP is NIL and FAILURE-TOKEN/MESSAGE describe why."
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
                       (format nil "Operand does not match ~(~A~) addressing mode"
                               (mode-descriptor-name mode)))))
           (incf i)))
        (:expr
         (handler-case
             (multiple-value-bind (ast next-i) (parse-expression tokens :start i :end end)
               (cl:push ast asts)
               (setf i next-i))
           (parse-failure (c)
             (return-from %match-mode-pattern
               (values nil nil nil (lasm-syntax-error-message c))))))))
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
unconsumed."
  (let ((mode (if (mode-descriptor-p mode) mode (find-mode-descriptor mode))))
    (multiple-value-bind (asts okp failure-token message) (%match-mode-pattern tokens mode)
      (unless okp
        (%parse-error failure-token message))
      (values (first asts) asts))))

;;; Built-in modes -- M1's *BUILTIN-MODE-PREFIXES* table, expressed as
;;; ordinary DEFMODE forms. "#" already lexes to :HASH (lexer.lisp) for
;;; exactly this purpose.

(defmode immediate "#" expr :width 1)
(defmode zero-page expr :width 1)
(defmode absolute expr)
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
