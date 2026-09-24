;;;; conditional.lisp
;;;; .if/.elseif/.else/.endif conditional assembly. Like .macro (macro.lisp)
;;;; and .include (include.lisp) these span a range of statements, so they are
;;;; recognized by mnemonic here rather than registered in *DIRECTIVES*.
;;;;
;;;; EXPAND-CONDITIONALS runs after macro expansion and before layout. A
;;;; condition folds against the constants (.equ, .set, name = value) defined
;;;; above it; it cannot depend on a label or the location counter, which have
;;;; no value until layout.
;;;;
;;;; TODO: .include expands before this pass, so a skipped branch's include is
;;;; still read; lazy include expansion would make it truly conditional.

(in-package #:lasm)

(define-condition conditional-error (lasm-syntax-error) ()
  (:documentation "Signalled by EXPAND-CONDITIONALS on a malformed or
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
    (expr-number nil)
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
        (when label
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

(defun expand-conditionals (statements)
  "Resolve .if/.elseif/.else/.endif blocks in STATEMENTS, keeping the
statements of each selected branch and dropping the conditional lines
themselves. A label on a conditional line stays as a label-only statement.
Signals CONDITIONAL-ERROR on a malformed or unbalanced block or a condition
that does not fold to a constant."
  (unless (some #'%conditional-mnemonic statements)
    (return-from expand-conditionals statements))
  (let ((constants (make-hash-table :test 'equal))
        (layout-names (%conditional-label-names statements))
        (stack nil)
        (scope nil)
        result)
    (labels ((active-p () (or (null stack) (if-block-active-p (first stack))))
             (fold-assignment (statement)
               (let* ((directive (find-directive-descriptor (statement-mnemonic statement)))
                      (asts (%directive-args statement directive))
                      (line (statement-line statement))
                      (name-ast (first asts)))
                 (when (and (expr-label-p name-ast) (= 2 (length asts)))
                   (let ((name (if (expr-label-localp name-ast)
                                   (%qualify-local scope (expr-label-name name-ast) line)
                                   (expr-label-name name-ast)))
                         (value-ast (%qualify-locals! (second asts) scope line)))
                     (remhash name constants)
                     (setf (gethash name layout-names) t)
                     (unless (%ast-layout-dependent-p value-ast)
                       (handler-case
                           (progn (setf (gethash name constants)
                                        (eval-expr value-ast :symbols constants))
                                  (remhash name layout-names))
                         (error () nil)))))))
             (condition-value (statement)
               (let ((line (statement-line statement))
                     (mnemonic (statement-mnemonic statement)))
                 (when (statement-mode-suffix statement)
                   (%conditional-error line "~A: a mode suffix is not valid here" mnemonic))
                 (unless (= 1 (length (statement-operands statement)))
                   (%conditional-error line "~A: expected one condition" mnemonic))
                 (let ((ast (%qualify-locals!
                             (%directive-operand-ast (first (statement-operands statement)))
                             scope line)))
                   (when (%ast-layout-dependent-p ast)
                     (%conditional-error line "~A: the condition depends on layout (a label, * or bank())"
                                         mnemonic))
                   (handler-case (/= 0 (eval-expr ast :symbols constants))
                     (unresolved-label (c)
                       (let ((name (unresolved-label-name c)))
                         (%conditional-error
                          line (if (gethash name layout-names)
                                   "~A: ~S depends on layout (a label or *)"
                                   "~A: ~S is not a constant defined above")
                          mnemonic (%display-symbol-key name))))))))
             (no-operands (statement)
               (when (or (statement-operands statement) (statement-mode-suffix statement))
                 (%conditional-error (statement-line statement) "~A: takes no operands"
                                     (statement-mnemonic statement)))))
      (dolist (statement statements)
        (let ((*current-invocation-line* (and (statement-definition-line statement)
                                              (statement-line statement)))
              (*current-definition-line* (statement-definition-line statement))
              (*current-source-unit* (statement-source-unit statement))
              (*current-definition-unit* (statement-definition-unit statement))
              (keyword (%conditional-mnemonic statement))
              (line (statement-line statement)))
          (with-source-unit (statement-source-unit statement)
            (cond
              ((null keyword)
               (when (active-p)
                 (cl:push statement result)
                 (when (statement-label statement)
                   (unless (statement-label-localp statement)
                     (setf scope (statement-label statement))))
                 (let ((directive (and (statement-mnemonic statement)
                                       (find-directive-descriptor (statement-mnemonic statement)))))
                   (when (and directive
                              (member (directive-descriptor-action directive) '(:assign :reassign)))
                     (fold-assignment statement)))))
              (t
               (let ((label (and (statement-label statement)
                                 (%label-only-statement statement))))
                 (when (and label (active-p))
                   (cl:push label result)
                   (unless (statement-label-localp statement)
                     (setf scope (statement-label statement))))
                 (ecase keyword
                   (:if
                    (let ((parent (active-p)))
                      (cl:push (make-if-block
                                :parent-active-p parent :line line
                                :unit (statement-source-unit statement))
                               stack)
                      (let ((blk (first stack)))
                        (when parent
                          (setf (if-block-active-p blk) (condition-value statement)
                                (if-block-taken-p blk) (if-block-active-p blk)))
                        (unless parent (setf (if-block-taken-p blk) t)))))
                   ((:elseif :else :endif)
                    (let ((blk (first stack)))
                      (unless blk
                        (%conditional-error line "~A without a matching .if"
                                            (statement-mnemonic statement)))
                      (unless (eq (if-block-unit blk) (statement-source-unit statement))
                        (%conditional-error line "~A closes a .if from a different source file"
                                            (statement-mnemonic statement)))
                      (ecase keyword
                        (:elseif
                         (when (if-block-else-seen-p blk)
                           (%conditional-error line ".elseif after .else"))
                         (setf (if-block-active-p blk) nil)
                         (unless (if-block-taken-p blk)
                           (let ((value (condition-value statement)))
                             (setf (if-block-active-p blk) value
                                   (if-block-taken-p blk) value))))
                        (:else
                         (no-operands statement)
                         (when (if-block-else-seen-p blk)
                           (%conditional-error line "duplicate .else"))
                         (setf (if-block-else-seen-p blk) t
                               (if-block-active-p blk)
                               (and (if-block-parent-active-p blk)
                                    (not (if-block-taken-p blk)))
                               (if-block-taken-p blk) t))
                        (:endif
                         (no-operands statement)
                         (cl:pop stack))))))))))))
      (when stack
        (with-source-unit (if-block-unit (first stack))
          (%conditional-error (if-block-line (first stack)) ".if has no matching .endif")))
      (nreverse result))))
