;;;; directive.lisp
;;;; DEFDIRECTIVE: a declarative assembler-directive grammar (LASM-plan.md
;;;; sec. 3.6, #14). A directive is a named parameter list plus exactly one
;;;; action form from a fixed vocabulary (SET-ORIGIN!, EMIT, RESERVE) --
;;;; restricting the body to one action, rather than arbitrary Lisp, is what
;;;; lets the assembler (assembler.lisp) derive a directive statement's
;;;; layout size *statically*, the same way it already knows an instruction
;;;; statement's size from its chosen INSTRUCTION-DESCRIPTOR without running
;;;; any semantics.
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
  action      ; :set-origin | :emit | :reserve
  width)      ; element byte width for :emit (1 for .byte, 2 for .word);
              ; NIL for :set-origin / :reserve

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
  "PARAMS is DEFDIRECTIVE's parameter list: either (name) for a fixed
single argument, or (&rest name) for a variadic directive. Returns (VALUES
arity name) where ARITY is (:FIXED 1) or :VARIADIC."
  (cond
    ((and (= (length params) 2) (eq (first params) '&rest))
     (values :variadic (second params)))
    ((= (length params) 1)
     (values '(:fixed 1) (first params)))
    (t (error "Malformed DEFDIRECTIVE parameter list ~S -- expected (name) ~
or (&rest name)" params))))

(defun %parse-directive-action (action-form param-name)
  "ACTION-FORM is DEFDIRECTIVE's single body form. PARAM-NAME is the symbol
bound by the parameter list (%PARSE-DIRECTIVE-PARAMS) -- the action form
must reference exactly this symbol as its argument, so DEFDIRECTIVE can
compile the action without evaluating arbitrary Lisp. Returns (VALUES
action width)."
  (unless (and (consp action-form) (= (length action-form) 2)
               (eq (second action-form) param-name))
    (error "Malformed DEFDIRECTIVE action ~S -- expected one of (set-origin! ~
~S), (emit width ~S), (reserve ~S) referencing this directive's own ~
parameter" action-form param-name param-name param-name))
  (destructuring-bind (head arg) action-form
    (declare (ignore arg))
    (case head
      (set-origin! (values :set-origin nil))
      (reserve (values :reserve nil))
      (t (error "Unknown DEFDIRECTIVE action head ~S -- expected SET-ORIGIN!, ~
EMIT, or RESERVE" head)))))

(defun %parse-emit-action (action-form param-name)
  "EMIT is the one action taking two arguments (a literal width, then the
variadic values), so it doesn't fit %PARSE-DIRECTIVE-ACTION's one-argument
shape -- handled separately. Returns (VALUES :emit width)."
  (destructuring-bind (head width-form values-sym) action-form
    (unless (and (eq head 'emit) (integerp width-form) (eq values-sym param-name))
      (error "Malformed DEFDIRECTIVE action ~S -- expected (emit width ~S)"
             action-form param-name))
    (values :emit width-form)))

(defun build-directive-descriptor (name params action-form)
  (multiple-value-bind (arity param-name) (%parse-directive-params params)
    (multiple-value-bind (action width)
        (if (and (consp action-form) (eq (first action-form) 'emit))
            (%parse-emit-action action-form param-name)
            (%parse-directive-action action-form param-name))
      (make-directive-descriptor :name (string-upcase name) :arity arity
                                  :action action :width width))))

(defmacro defdirective (name params &body body)
  "Define a directive named NAME (a string, e.g. \".org\") taking PARAMS --
either (VALUE-NAME) for a directive with exactly one argument, or (&rest
VALUES-NAME) for a variadic one (e.g. \".byte\"). BODY must be exactly one
action form referencing PARAMS' own parameter name:

  (set-origin! address)   -- ADDRESS becomes the assembler's new address
                              counter; must fold to a label-free constant at
                              layout time (the assembler has no symbol table
                              yet in pass 1). Zero layout size.
  (reserve count)          -- advance the address counter by COUNT bytes,
                              zero-filled; COUNT must also fold label-free.
  (emit width values)      -- lay down (length VALUES) little-endian
                              WIDTH-byte fields, one per value in VALUES;
                              layout size is WIDTH * (length VALUES); each
                              value may reference a label (resolved in
                              pass 2, like an ordinary instruction operand).

E.g.:
  (defdirective \".org\"  (address)      (set-origin! address))
  (defdirective \".byte\" (&rest values) (emit 1 values))
  (defdirective \".word\" (&rest values) (emit 2 values))
  (defdirective \".res\"  (count)        (reserve count))

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

(defdirective ".org"  (address)      (set-origin! address))
(defdirective ".byte" (&rest values) (emit 1 values))
(defdirective ".word" (&rest values) (emit 2 values))
(defdirective ".res"  (count)        (reserve count))
