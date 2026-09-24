;;;; directive.lisp
;;;; DEFDIRECTIVE: a declarative assembler-directive grammar (LASM-plan.md
;;;; sec. 3.6, #14). A directive is a named parameter list plus exactly one
;;;; action form from a fixed vocabulary (SET-ORIGIN!, EMIT, RESERVE, ASSIGN,
;;;; REASSIGN)
;;;; -- restricting the body to one action, rather than arbitrary Lisp, is
;;;; what lets the assembler (assembler.lisp) derive a directive statement's
;;;; layout size *statically*, the same way it already knows an instruction
;;;; statement's size from its chosen INSTRUCTION-DESCRIPTOR without running
;;;; any semantics. ASSIGN (#35's .EQU) is zero-size like SET-ORIGIN!, but
;;;; binds a name in the symbol table instead of moving the address counter
;;;; -- see assembler.lisp for the layout-time binding path. REASSIGN is the
;;;; corresponding rebinding action used by .SET.
;;;;
;;;; Unlike DEFMODE (mode.lisp), this registers with a plain top-level SETF,
;;;; not an EVAL-WHEN: DEFMODE needs compile-time registration because
;;;; DEFINSTRUCTION resolves mode names at macroexpansion time. Nothing
;;;; resolves a directive name at macroexpansion -- the assembler looks a
;;;; directive up by name at ordinary runtime (like DEFLEXER's *LEXERS*,
;;;; lexer.lisp) -- so there is no need to import that complexity here.
;;;;
;;;; Scope: this file only defines and registers directives and evaluates
;;;; their arguments to a (VALUES size effect) shape. Statement-level
;;;; dispatch -- recognizing a statement's mnemonic as a directive, and
;;;; acting on its size/effect during layout and encode -- is
;;;; assembler.lisp's job, mirroring how mode.lisp only matches operand
;;;; syntax and assembler.lisp/instruction.lisp do the choosing and encoding.

(in-package #:lasm)

;;; Directive descriptor

(defstruct directive-descriptor
  name        ; string, upcased, prefix included (e.g. ".ORG")
  arity       ; (:fixed n) | :variadic
  action      ; :set-origin | :select-bank | :emit | :reserve | :assign | :reassign
  width       ; element width, in cells (#53) -- 1 for .byte, 2 for .word;
              ; NIL for :set-origin / :select-bank / :reserve / :assign / :reassign
  endian)     ; :emit only: overrides the machine's endian order, or NIL

;; Registry of defined directives, keyed by upcased name string -- mirrors
;; *LEXERS* (lexer.lisp), a plain runtime hash table with no EVAL-WHEN (see
;; the file header comment for why this differs from *MODES*, mode.lisp).
(defvar *directives* (make-hash-table :test 'equal))

(defun find-directive-descriptor (name)
  "Look up the DIRECTIVE-DESCRIPTOR registered under NAME (a string,
matched case-insensitively) with DEFDIRECTIVE. Returns NIL if none is
registered -- unlike FIND-MODE-DESCRIPTOR/FIND-LEXER-DESCRIPTOR, this is a
lookup the assembler uses to distinguish a directive statement from an
instruction one, so a miss is an ordinary outcome, not a caller error."
  (gethash (string-upcase name) *directives*))

;;; DEFDIRECTIVE

(defun %parse-directive-params (params)
  "PARAMS is DEFDIRECTIVE's parameter list: (name) for a fixed single
argument, (name value) for a fixed two-argument directive (#35's .EQU), or
(&rest name) for a variadic directive. Returns (VALUES arity param-names)
where ARITY is (:FIXED 1), (:FIXED 2), or :VARIADIC and PARAM-NAMES is a
list of the parameter symbols in order -- checked for &REST first since
(&rest name) and (name value) are both length 2."
  (cond
    ((and (= (length params) 2) (eq (first params) '&rest))
     (values :variadic (list (second params))))
    ((= (length params) 1)
     (values '(:fixed 1) params))
    ((= (length params) 2)
     (values '(:fixed 2) params))
    (t (error "Malformed DEFDIRECTIVE parameter list ~S -- expected (name), ~
(name value), or (&rest name)" params))))

(defun %parse-directive-action (action-form param-names)
  "ACTION-FORM is DEFDIRECTIVE's single body form. PARAM-NAMES are the
symbols bound by the parameter list (%PARSE-DIRECTIVE-PARAMS) -- the action
form must reference exactly these symbols, in order, as its arguments, so
DEFDIRECTIVE can compile the action without evaluating arbitrary Lisp.
Returns (VALUES action width). Handles the one-argument actions
(SET-ORIGIN!, SELECT-BANK!, RESERVE); EMIT and ASSIGN take two arguments and are parsed
by %PARSE-EMIT-ACTION / %PARSE-ASSIGN-ACTION instead."
  (unless (and (consp action-form) (= (length action-form) 2)
               (equal (rest action-form) param-names))
    (error "Malformed DEFDIRECTIVE action ~S -- expected one of (set-origin! ~
~S), (select-bank! ~S), (reserve ~S) referencing this directive's own parameter"
           action-form (first param-names) (first param-names) (first param-names)))
  (destructuring-bind (head arg) action-form
    (declare (ignore arg))
    (case head
      (set-origin! (values :set-origin nil))
      (select-bank! (values :select-bank nil))
      (reserve (values :reserve nil))
      (t (error "Unknown DEFDIRECTIVE action head ~S -- expected SET-ORIGIN!, ~
SELECT-BANK!, EMIT, RESERVE, or ASSIGN" head)))))

(defun %parse-emit-action (action-form param-names)
  "EMIT is one of the two actions taking two arguments (a literal width,
then the variadic values), so it doesn't fit %PARSE-DIRECTIVE-ACTION's
one-argument shape -- handled separately. An optional trailing :ENDIAN spec
overrides the machine's endian order. Returns (VALUES :emit width endian)."
  (destructuring-bind (head width-form values-sym &rest options) action-form
    (unless (and (eq head 'emit) (integerp width-form) (equal (list values-sym) param-names)
                 (or (null options) (and (eq (first options) :endian) (= (length options) 2))))
      (error "Malformed DEFDIRECTIVE action ~S -- expected (emit width ~S [:endian ORDER])"
             action-form (first param-names)))
    (values :emit width-form
            (and options (%check-endian (second options) (first param-names))))))

(defun %parse-assign-action (action-form param-names)
  "Validate ASSIGN or REASSIGN with the directive's two parameters, in order."
  (unless (and (consp action-form) (member (first action-form) '(assign reassign))
               (equal (rest action-form) param-names))
    (error "Malformed DEFDIRECTIVE action ~S -- expected an assignment with (~{~S~^ ~})"
           action-form param-names))
  (if (eq (first action-form) 'assign) :assign :reassign))

(defun build-directive-descriptor (name params action-form)
  (multiple-value-bind (arity param-names) (%parse-directive-params params)
    (multiple-value-bind (action width endian)
        (cond
          ((and (consp action-form) (eq (first action-form) 'emit))
           (%parse-emit-action action-form param-names))
          ((and (consp action-form) (member (first action-form) '(assign reassign)))
           (values (%parse-assign-action action-form param-names) nil))
          (t (%parse-directive-action action-form param-names)))
      (make-directive-descriptor :name (string-upcase name) :arity arity
                                  :action action :width width :endian endian))))

(defmacro defdirective (name params &body body)
  "Define a directive named NAME (a string, e.g. \".org\") taking PARAMS --
(VALUE-NAME) for a directive with exactly one argument, (NAME-NAME
VALUE-NAME) for one with exactly two (#35's .EQU), or (&rest VALUES-NAME)
for a variadic one (e.g. \".byte\"). BODY must be exactly one action form
referencing PARAMS' own parameter name(s), in order:

  (set-origin! address)   -- ADDRESS becomes the assembler's new address
                              counter; must fold to a label-free constant at
                              layout time (the assembler has no symbol table
                              yet in pass 1). Zero layout size.
  (select-bank! n)         -- N is the bank that later output landing in a
                              banked region is placed in; must fold to a
                              label-free constant. Zero layout size.
  (reserve count)          -- advance the address counter by COUNT cells
                              (#53 -- a machine's own addressable unit, not
                              necessarily 8 bits), zero-filled; COUNT must
                              also fold label-free.
  (emit width values [:endian order])
                           -- lay down (length VALUES) WIDTH-cell fields,
                              one per value in VALUES, in the machine's own
                              endian order (#66), or in ORDER (:little, :big
                              or (outer inner group)) when :endian is given;
                              layout size is WIDTH *
                              (length VALUES); each value may reference a
                              label (resolved in pass 2, like an ordinary
                              instruction operand).
  (assign name value)      -- bind NAME (an identifier operand, not an
                              expression) to VALUE in the symbol table,
                              without occupying any address -- .EQU.
                              VALUE must fold at layout time, against labels
                              and assignments already bound above it (a forward
                              reference is an ASSEMBLY-ERROR); zero layout
                              size.
  (reassign name value)    -- bind or replace an assignment name at layout
                              time, with the same operands and zero size.

E.g.:
  (defdirective \".org\"  (address)      (set-origin! address))
  (defdirective \".byte\" (&rest values) (emit 1 values))
  (defdirective \".word\" (&rest values) (emit 2 values))
  (defdirective \".res\"  (count)        (reserve count))
  (defdirective \".bank\" (n)            (select-bank! n))
  (defdirective \".equ\"  (name value)   (assign name value))
  (defdirective \".set\"  (name value)   (reassign name value))

Registers the resulting DIRECTIVE-DESCRIPTOR under NAME (upcased) in
*DIRECTIVES*, retrievable with FIND-DIRECTIVE-DESCRIPTOR. Restricting BODY
to one recognized action (rather than arbitrary Lisp) is what lets the
assembler compute a directive statement's layout size without evaluating
anything -- see this file's header comment."
  (unless (= (length body) 1)
    (error "DEFDIRECTIVE ~S: body must be exactly one action form" name))
  `(progn
     (setf (gethash (string-upcase ,name) *directives*)
           (build-directive-descriptor ,name ',params ',(first body)))
     ,name))

;;; Built-in directives

;; .BYTE/.WORD's widths are in cells (#53), not bits -- 1 and 2 cells
;; respectively, same as they always meant 1 and 2 8-bit bytes on every
;; byte-addressed machine so far. On a word-addressed machine (:CELL-WIDTH
;; 16), ".byte 1, 2" lays down two 16-bit cells, not two 8-bit bytes -- the
;; name is a misnomer there (DCPU-16 assemblers call the equivalent "dat").
(defdirective ".org"  (address)      (set-origin! address))
(defdirective ".byte" (&rest values) (emit 1 values))
(defdirective ".word" (&rest values) (emit 2 values))
;; .RES's count is also in cells (#53) -- on a word-addressed machine
;; ".res 4" reserves 4 cells, not 4 bytes. .ORG's operand was always an
;; address, and addresses were always cell-indexed, so .ORG itself needs no
;; change at all.
(defdirective ".res"  (count)        (reserve count))
(defdirective ".bank" (n)            (select-bank! n))
(defdirective ".equ"  (name value)   (assign name value))
(defdirective ".set"  (name value)   (reassign name value))
;; .CELL/.DAT (#65): plain aliases for .BYTE -- same width-1 :EMIT action, so
;; a word-addressed machine's source can name "one of the machine's own
;; cells" without reading the byte-addressed-flavored ".byte". Both spellings
;; are offered since DCPU-16 assemblers call this "dat" while other
;; word-addressed machines may prefer the more generic "cell". The registry
;; is global, so both are also visible (as .BYTE synonyms) on byte-addressed
;; machines, and -- like every other directive -- take the mnemonic
;; namespace, so no machine may define an instruction named .CELL or .DAT.
(defdirective ".cell" (&rest values) (emit 1 values))
(defdirective ".dat"  (&rest values) (emit 1 values))
