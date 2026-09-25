;;;; conditional.lisp
;;;; .if/.elseif/.else/.endif conditional assembly helpers. Like .macro
;;;; (macro.lisp) and .include (include.lisp) these span a range of
;;;; statements, so they are recognized by mnemonic rather than registered in
;;;; *DIRECTIVES*. PREPROCESS (preprocess.lisp) resolves them.

(in-package #:lasm)

(define-condition conditional-error (lasm-syntax-error) ()
  (:documentation "Signalled by PREPROCESS on a malformed or
unbalanced .if/.elseif/.else/.endif, or a condition that is not a constant."))

(defun %conditional-error (line fmt &rest args)
  (error 'conditional-error :message (apply #'format nil fmt args) :line line))

(defstruct if-block
  parent-active-p ; the region enclosing the whole .if is emitting
  active-p        ; the current branch is emitting
  taken-p         ; some branch has already been selected
  else-seen-p
  line
  unit)

(defun %ast-layout-dependent-p (ast)
  (etypecase ast
    ((or expr-number expr-string) nil)
    (expr-label nil)
    (expr-location t)
    (expr-unary (or (eq (expr-unary-op ast) :bank)
                    (%ast-layout-dependent-p (expr-unary-operand ast))))
    (expr-binary (or (%ast-layout-dependent-p (expr-binary-left ast))
                     (%ast-layout-dependent-p (expr-binary-right ast))))))

(defun %conditional-label-names (statements)
  "Qualified names of every label in STATEMENTS, skipped regions included."
  (let ((names (make-hash-table :test 'equal)) scope)
    (dolist (statement statements names)
      (let ((label (statement-label statement)))
        (when (and label (or scope (not (statement-label-localp statement))))
          (setf (gethash (if (statement-label-localp statement)
                             (%qualify-local scope label (statement-line statement))
                             label)
                         names)
                t)
          (unless (statement-label-localp statement)
            (setf scope label)))))))

(defun %label-only-statement (statement)
  (make-statement :label (statement-label statement)
                  :label-localp (statement-label-localp statement)
                  :line (statement-line statement)
                  :source-unit (statement-source-unit statement)
                  :definition-line (statement-definition-line statement)
                  :definition-unit (statement-definition-unit statement)))

