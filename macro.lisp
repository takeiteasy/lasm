;;;; macro.lisp
;;;; .macro/.endm collection and expansion. Each invocation receives its own
;;;; bindings for parameters and body-defined symbols before assembly layout.

(in-package #:lasm)

;;; Conditions

(define-condition macro-error (lasm-syntax-error) ()
  (:documentation "Signalled by EXPAND-MACROS on a malformed macro definition,
name collision, invocation, or non-converging expansion."))

(defun %macro-error (line fmt &rest args)
  (error 'macro-error :message (apply #'format nil fmt args) :line line))

;;; Macro descriptor

(defstruct macro-descriptor
  name
  params
  defaults
  body
  line)

(defparameter *max-macro-expansion-rounds* 32
  "Maximum rewrite rounds before EXPAND-MACROS reports recursion.")

;;; Phase 1: collect .macro...endm blocks, leaving the rest of the program

(defun %macro-directive-p (mnemonic)
  (and mnemonic (string-equal mnemonic ".macro")))

(defun %endm-directive-p (mnemonic)
  (and mnemonic (string-equal mnemonic ".endm")))

(defun %macro-plain-name (token line)
  (unless (and token (eq (token-type token) :identifier) (not (token-localp token)))
    (%macro-error line ".macro: expected a plain name"))
  (token-value token))

(defun %parse-macro-header (statement)
  (let* ((tokens (coerce (statement-operand-tokens statement) 'list))
         (line (statement-line statement))
         (name (%macro-plain-name (first tokens) line))
         (rest (rest tokens))
         (params nil)
         (defaults nil))
    (if (find :equals rest :key #'%punct-value)
        (let* ((groups (%split-operands tokens))
               (first-group (rest (first groups)))
               (parameter-groups (append (and first-group (list first-group))
                                         (rest groups))))
          (dolist (group parameter-groups)
            (let ((param (%macro-plain-name (first group) line))
                  (tail (rest group)))
              (when (and tail (not (eq (%punct-value (first tail)) :equals)))
                (%macro-error line ".macro: separate parameters with commas when defaults are used"))
              (when (and tail (null (rest tail)))
                (%macro-error line ".macro: missing default for ~A" param))
              (when (and (null tail) (some #'identity defaults))
                (%macro-error line ".macro: required parameter ~A follows an optional parameter" param))
              (cl:push param params)
              (cl:push (and tail (rest tail)) defaults))))
        (dolist (tok rest)
          (unless (eq (%punct-value tok) :comma)
            (cl:push (%macro-plain-name tok line) params)
            (cl:push nil defaults))))
    (setf params (nreverse params) defaults (nreverse defaults))
    (unless (= (length params) (length (remove-duplicates params :test #'string=)))
      (%macro-error line ".macro: duplicate parameter name"))
    (values name params defaults)))

(defun %macro-symbol-key (name localp)
  (cons name (and localp t)))

(defun %macro-symbol-definitions (body)
  (let (definitions)
    (dolist (statement body)
      (when (statement-label statement)
        (cl:push (%macro-symbol-key (statement-label statement)
                                    (statement-label-localp statement)) definitions))
      (when (and (statement-mnemonic statement)
                 (string-equal (statement-mnemonic statement) ".equ"))
        (let* ((operand (first (statement-operands statement)))
               (tokens (and operand (operand-tokens operand))))
          (when (and tokens (= (length tokens) 1)
                     (eq (token-type (aref tokens 0)) :identifier))
            (let ((token (aref tokens 0)))
              (cl:push (%macro-symbol-key (token-value token) (token-localp token))
                       definitions))))))
    (remove-duplicates definitions :test #'equal)))

(defun %collect-macros (statements machine)
  "Return macro descriptors and statements outside definition blocks.
Reject malformed definitions and names reserved by MACHINE or directives."
  (let ((macros (make-hash-table :test 'equal))
        remaining
        in-macro-p header-name header-key header-params header-defaults header-line header-unit body)
    (dolist (statement statements)
      (with-source-unit (statement-source-unit statement)
       (let ((mnemonic (statement-mnemonic statement)))
        (cond
          ((%macro-directive-p mnemonic)
           (when in-macro-p
             (%macro-error (statement-line statement) ".macro: nested inside another .macro"))
           (when (statement-label statement)
             (%macro-error (statement-line statement) ".macro: cannot itself carry a label"))
           (when (statement-mode-suffix statement)
             (%macro-error (statement-line statement) ".macro: a mode suffix is not valid here"))
           (multiple-value-bind (name params defaults) (%parse-macro-header statement)
             (let ((key (string-upcase name)))
               (when (nth-value 1 (gethash key macros))
                 (%macro-error (statement-line statement) "Duplicate macro ~S" name))
               (when (find-directive-descriptor name)
                 (%macro-error (statement-line statement)
                                "Macro name ~S collides with a directive" name))
               (when (gethash key (machine-descriptor-instructions
                                   (find-machine-descriptor machine)))
                 (%macro-error (statement-line statement)
                               "Macro name ~S collides with an instruction" name))
               (setf in-macro-p t header-name name header-key key header-params params
                     header-defaults defaults
                     header-line (statement-line statement)
                     header-unit (statement-source-unit statement) body nil))))
          ((%endm-directive-p mnemonic)
           (unless in-macro-p
             (%macro-error (statement-line statement) ".endm without a matching .macro"))
           (when (statement-label statement)
             (%macro-error (statement-line statement) ".endm: cannot itself carry a label"))
           (when (statement-mode-suffix statement)
             (%macro-error (statement-line statement) ".endm: a mode suffix is not valid here"))
           (dolist (key (%macro-symbol-definitions body))
             (when (and (not (cdr key))
                        (member (car key) header-params :test #'string=))
               (%macro-error header-line "Macro parameter ~S also names a body symbol"
                             (car key))))
           (setf (gethash header-key macros)
                 (make-macro-descriptor :name header-key :params header-params
                                         :defaults header-defaults
                                         :body (nreverse body) :line header-line))
           (setf in-macro-p nil header-key nil))
          (in-macro-p (cl:push statement body))
          (t (cl:push statement remaining))))))
    (when in-macro-p
      (with-source-unit header-unit
        (%macro-error header-line ".macro ~A has no matching .endm" header-name)))
    (values macros (nreverse remaining))))

;;; Phase 2: expand invocations against the collected macro table

(defun %macro-hygiene-map (body fresh-name)
  (let ((names (make-hash-table :test 'equal)))
    (dolist (key (%macro-symbol-definitions body))
      (setf (gethash key names) (funcall fresh-name (car key))))
    names))

(defun %substitute-tokens (tokens bindings names)
  (loop for tok in tokens
        for binding = (and (eq (token-type tok) :identifier)
                            (assoc (token-value tok) bindings :test #'string=))
        for renamed = (and (eq (token-type tok) :identifier)
                           (gethash (%macro-symbol-key (token-value tok)
                                                       (token-localp tok)) names))
        if binding append (copy-list (cdr binding))
        else if renamed collect (let ((copy (copy-token tok)))
                                  (setf (token-value copy) renamed
                                        (token-text copy) renamed)
                                  copy)
        else collect tok))

(defun %substitute-statement (statement bindings names call-line call-unit)
  (let* ((substituted (%substitute-tokens (coerce (statement-operand-tokens statement) 'list)
                                           bindings names))
         (operand-tokens (coerce substituted 'simple-vector))
         (sugar-p (and (statement-mnemonic statement)
                       (string-equal (statement-mnemonic statement) ".equ")
                       (>= (length (statement-operand-tokens statement)) 2)
                       (eq (%punct-value (aref (statement-operand-tokens statement) 1))
                           :equals)))
         (groups (if sugar-p
                     (mapcar (lambda (operand)
                               (%substitute-tokens (coerce (operand-tokens operand) 'list)
                                                   bindings names))
                             (statement-operands statement))
                     (and substituted (%split-operands substituted)))))
    (make-statement
     :label (or (gethash (%macro-symbol-key (statement-label statement)
                                            (statement-label-localp statement)) names)
                (statement-label statement))
     :label-localp (statement-label-localp statement)
     :mnemonic (statement-mnemonic statement)
     :operand-tokens operand-tokens
     :operands (mapcar (lambda (group) (make-operand :tokens (coerce group 'simple-vector)))
                       groups)
     :mode-suffix (statement-mode-suffix statement)
     :line call-line
     :source-unit call-unit
     :definition-line (or (statement-definition-line statement)
                          (statement-line statement))
     :definition-unit (or (statement-definition-unit statement)
                          (statement-source-unit statement)))))

(defun %macro-invocation-p (statement macros)
  "Return the descriptor invoked by STATEMENT, or NIL."
  (and (statement-mnemonic statement)
       (gethash (string-upcase (statement-mnemonic statement)) macros)))

(defun %expand-invocation (statement descriptor fresh-name)
  "Expand STATEMENT using DESCRIPTOR's parameters and fresh body symbol names."
  (let* ((params (macro-descriptor-params descriptor))
         (args (statement-operands statement))
         (line (statement-line statement)))
    ;; A forced addressing-mode suffix names a mode, which only means
    ;; something on an instruction statement -- a macro invocation expands
    ;; to zero or more statements of its own, so "NAME.w arg" has nothing
    ;; coherent to force.
    (when (statement-mode-suffix statement)
      (%macro-error line "~A.~A: a mode suffix is not meaningful on a macro invocation"
                    (macro-descriptor-name descriptor) (statement-mode-suffix statement)))
    (let ((required (count nil (macro-descriptor-defaults descriptor))))
      (unless (<= required (length args) (length params))
        (%macro-error line "~A: expected ~D to ~D arguments, got ~D"
                      (macro-descriptor-name descriptor) required
                      (length params) (length args))))
    (let ((bindings (loop for param in params
                         for default in (macro-descriptor-defaults descriptor)
                         for arg in (append args (make-list (- (length params) (length args))))
                         collect (cons param (if arg
                                                 (coerce (operand-tokens arg) 'list)
                                                 default))))
          (names (%macro-hygiene-map (macro-descriptor-body descriptor) fresh-name))
          (label-statement
            (when (statement-label statement)
              (make-statement :label (statement-label statement)
                               :label-localp (statement-label-localp statement)
                               :line line
                               :source-unit (statement-source-unit statement)
                               :definition-line (statement-definition-line statement)
                               :definition-unit (statement-definition-unit statement)))))
      (append (and label-statement (list label-statement))
              (mapcar (lambda (body-statement)
                        (%substitute-statement body-statement bindings names line
                                               (statement-source-unit statement)))
                      (macro-descriptor-body descriptor))))))

(defun expand-macros (statements machine)
  "Expand macro invocations for MACHINE until no invocations remain."
  (multiple-value-bind (macros remaining) (%collect-macros statements machine)
    (if (zerop (hash-table-count macros))
        remaining
        (let ((result remaining)
              (used (make-hash-table :test 'equal))
              (aliases (machine-descriptor-register-aliases
                        (find-machine-descriptor machine)))
              (serial 0))
          (dolist (statement statements)
            (when (statement-label statement)
              (setf (gethash (statement-label statement) used) t))
            (loop for token across (statement-operand-tokens statement)
                  when (eq (token-type token) :identifier)
                    do (setf (gethash (token-value token) used) t)))
          (dotimes (round *max-macro-expansion-rounds*)
            (if (some (lambda (s) (%macro-invocation-p s macros)) result)
                (setf result
                      (loop for statement in result
                            for descriptor = (%macro-invocation-p statement macros)
                            if descriptor
                              append (with-source-unit (statement-source-unit statement)
                               (%expand-invocation
                                      statement descriptor
                                      (lambda (name)
                                        (loop for candidate = (format nil "~A__LASM_~D" name
                                                                      (incf serial))
                                              unless (or (gethash candidate used)
                                                         (and aliases
                                                              (gethash (string-upcase candidate)
                                                                       aliases)))
                                                do (setf (gethash candidate used) t)
                                                   (return candidate)))))
                            else collect statement))
                (return-from expand-macros result)))
          (%macro-error nil "macro expansion did not converge after ~D rounds ~
(a macro invoking itself, directly or indirectly?)"
                        *max-macro-expansion-rounds*)))))
