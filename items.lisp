;;;; items.lisp
;;;; #113: structured assembly items -- a program as data rather than source
;;;; text. Each item becomes the tokens of one source line, which
;;;; %PARSE-LINE turns into a STATEMENT for ASSEMBLE-STATEMENTS. Operands are
;;;; built from addressing-mode patterns (mode.lisp), named through the
;;;; operand kinds of a backend (backend.lisp) or directly with (:mode ...).
;;;; The same tokens render as the ordinary source text an ASSEMBLY retains,
;;;; so listings, diagnostics and snapshots work unchanged.
;;;;
;;;; ITEMS-SIZE sizes a run of items without encoding it. An operation's
;;;; (:label NAME) forms are renamed per expansion (%FRESH-LABEL).
;;;;
;;;; Items:  (:label NAME)  (:directive NAME expr...)  (:op NAME arg...)
;;;;         (MNEMONIC operand...)  and the lowered (:function ...) (:call ...)
;;;;         (:return) (:push X) (:pop X) (:depth n)
;;;; Operands: an expression, (KIND value...), (:mode MODE value...), the
;;;;           frame slots (:arg i) and (:local i), or (:force OPERAND), which
;;;;           writes OPERAND's mode as the mnemonic's suffix.
;;;; Expressions: an integer, a name, or (OPERATOR expr...).
;;;; A symbol names by its downcased name, a string by itself.

(in-package #:lasm)

(define-condition items-error (lasm-error)
  ((detail :initarg :detail :reader items-error-detail)
   (item :initarg :item :initform nil :reader items-error-item))
  (:report (lambda (c s)
             (format s "Item error: ~A~@[ (in ~S)~]" (items-error-detail c) (items-error-item c)))))

(define-condition items-malformed (items-error) ())
(define-condition items-operand-mismatch (items-error) ())

(defun %items-fail (type item control &rest args)
  (error type :detail (apply #'format nil control args) :item item))

(defstruct items-program
  items backend machine origin memory lexer)

;;; Context

(defvar *items-lexer* 'default)
(defvar *items-lexer-descriptor* nil)
(defvar *items-machine* nil)
(defvar *items-frame* nil "The ITEMS-FRAME of the function being lowered, or NIL.")
(defvar *items-backend* nil)
(defvar *items-memory* nil "The memory element name items are assembled for, or NIL for the default.")
(defvar *items-literals* nil "Literal text -> its tokens, for the assembly in progress.")
(defvar *items-serial* 0 "Generated labels made so far, for the assembly in progress.")
(defvar *items-used-names* nil "Names in the items, which generated labels avoid.")
(defvar *items-global-label-seen* nil "True once an item has defined a non-local label.")
(defvar *items-pickables* nil "Mnemonic -> its (VARIANT-MODE-NAMES . ALTERNATIVE-NAMES), for the assembly in progress.")

(defun %source-name (designator item)
  "The source spelling of DESIGNATOR: a string as is, a symbol downcased unless it has mixed case."
  (typecase designator
    (string designator)
    (symbol (let ((name (symbol-name designator)))
              (if (string= name (string-upcase name)) (string-downcase name) name)))
    (t (%items-fail 'items-malformed item "~S is not a name" designator))))

(defun %same-name-p (a b)
  (let ((a (%designator-name a)) (b (%designator-name b)))
    (and a b (string= a b))))

(defun %local-name-p (name)
  (let ((prefix (lexer-descriptor-local-label-prefix *items-lexer-descriptor*)))
    (and prefix (plusp (length prefix))
         (>= (length name) (length prefix))
         (string= prefix name :end2 (length prefix)))))

;;; Tokens

(defun %name-token (name)
  (make-token :type :identifier :value name :text name :localp (%local-name-p name)))

(defun %number-token (n)
  (make-token :type :number :value n :text (format nil "~D" n)))

(defun %string-token (string)
  (make-token :type :string :value string :text string))

(defun %punct-token (keyword)
  (make-token :type :punctuation :value keyword :text (car (rassoc keyword *punctuators*))))

(defun %literal-tokens (text)
  (mapcar #'copy-token
          (or (gethash text *items-literals*)
              (setf (gethash text *items-literals*)
                    (remove-if (lambda (token) (member (token-type token) '(:eof :newline)))
                               (coerce (tokenize text :lexer *items-lexer*) 'list))))))

(defun %token-source (token)
  (if (eq (token-type token) :string)
      (with-output-to-string (out)
        (let ((delim (or (lexer-descriptor-string-delim *items-lexer-descriptor*) "\"")))
          (write-string delim out)
          (loop for char across (token-text token)
                do (when (or (char= char #\\) (string= (string char) delim))
                     (write-char #\\ out))
                   (write-char char out))
          (write-string delim out)))
      (token-text token)))

;;; Expressions

(defun %expression-tokens (value item)
  (typecase value
    (integer (if (minusp value)
                 (list (%punct-token :minus) (%number-token (- value)))
                 (list (%number-token value))))
    (string (list (%name-token value)))
    (symbol (if (keywordp value)
                (%items-fail 'items-malformed item "~S is not a value" value)
                (list (%name-token (%source-name value item)))))
    (cons (%operator-tokens value item))
    (t (%items-fail 'items-malformed item "~S is not a value" value))))

(defun %function-operator (head)
  (and (or (symbolp head) (stringp head))
       (cdr (assoc (string head) (lexer-descriptor-function-operators *items-lexer-descriptor*)
                   :test #'string-equal))))

(defun %expression-head-p (head)
  (or (%expression-operator head) (%function-operator head)))

(defun %operator-tokens (form item)
  (destructuring-bind (head &rest args) form
    (let ((operator (%expression-operator head))
          (function-keyword (%function-operator head)))
      (cond
        ((and operator args (null (rest args)) (assoc operator *unary-ops*))
         (append (list (%punct-token :lparen) (%punct-token operator))
                 (%expression-tokens (first args) item)
                 (list (%punct-token :rparen))))
        ((and operator (rest args) (assoc operator *binary-precedence*))
         (append (list (%punct-token :lparen))
                 (loop for (arg . more) on args
                       append (%expression-tokens arg item)
                       when more append (list (%punct-token operator)))
                 (list (%punct-token :rparen))))
        (function-keyword
         (append (list (make-token :type :function-operator :value function-keyword
                                   :text (string-downcase (string head)))
                       (%punct-token :lparen))
                 (loop for (arg . more) on args
                       append (%expression-tokens arg item)
                       when more append (list (%punct-token :comma)))
                 (list (%punct-token :rparen))))
        (t (%items-fail 'items-malformed item "~S is not an expression" form))))))

;;; Operands

(defun %fill-pattern (elements values item)
  "The tokens of a mode pattern with VALUES in its holes, and a claim
(ALTERNATIVE START END) for each ONE-OF alternative named in VALUES, its span
counted in tokens from the pattern's start."
  (let ((tokens '()) (claims '()))
    (loop while elements
          do (let ((element (cl:pop elements)))
               (ecase (first element)
                 (:literal (setf tokens (append tokens (%literal-tokens (second element)))))
                 (:expr (when (null values)
                          (%items-fail 'items-malformed item "too few values for the operand"))
                        (setf tokens (append tokens (%expression-tokens (cl:pop values) item))))
                 (:end-of (cl:push (list (second element) (third element) (length tokens)) claims))
                 (:one-of
                  (let* ((name (cl:pop values))
                         (alternative (find-if (lambda (alternative) (%same-name-p alternative name))
                                               (%one-of-alternatives element))))
                    (unless alternative
                      (%items-fail 'items-malformed item "~S is not one of ~{~A~^, ~}"
                                   name (%one-of-alternatives element)))
                    (setf elements (append (mode-descriptor-pattern
                                            (find-mode-descriptor alternative *items-machine*))
                                           (cons (list :end-of alternative (length tokens))
                                                 elements))))))))
    (when values
      (%items-fail 'items-malformed item "too many values for the operand: ~S" values))
    (values tokens (nreverse claims))))

(defun %force-operand-p (operand)
  (and (consp operand) (%keyword-named-p (first operand) "FORCE")))

(defun %operand-tokens (operand item)
  "The tokens of OPERAND, the addressing mode it names, if any, its claims
(MODE START END): the named mode over the whole operand, then its alternatives,
and whether it is a (:force ...) operand."
  (when (%force-operand-p operand)
    (let ((inner (second operand)))
      (unless (and (= (length operand) 2) (consp inner)
                   (not (%force-operand-p inner)) (not (%frame-operand-p inner)))
        (%items-fail 'items-malformed item "expected (:force (:mode MODE value...)) or (:force (KIND value...))"))
      (multiple-value-bind (tokens mode claims) (%operand-tokens inner item)
        (unless mode
          (%items-fail 'items-malformed item "~S names no addressing mode to force" inner))
        (return-from %operand-tokens (values tokens mode claims t)))))
  (when (%frame-operand-p operand)
    (setf operand (%frame-operand operand item)))
  (if (and (consp operand) (not (%expression-head-p (first operand))))
      (let* ((head (first operand))
             (mode (cond ((and (keywordp head) (string= (symbol-name head) "MODE"))
                          (or (%find-mode-by-name (second operand) *items-machine*)
                              (%items-fail 'items-malformed item "~S is not an addressing mode"
                                           (second operand))))
                         ((keywordp head)
                          (%items-fail 'items-malformed item "unknown operand form ~S" operand))
                         (t
                          (unless *items-backend*
                            (%items-fail 'items-malformed item "operand kind ~A needs a backend" head))
                          (let ((entry (assoc (%designator-name head)
                                              (backend-descriptor-operands *items-backend*)
                                              :test #'equal)))
                            (unless entry
                              (%items-fail 'items-malformed item "backend ~A has no operand kind ~A"
                                           (backend-descriptor-name *items-backend*) head))
                            (find-mode-descriptor (cdr entry) *items-machine*))))))
        (multiple-value-bind (tokens claims)
            (%fill-pattern (mode-descriptor-pattern mode)
                           (if (keywordp head) (cddr operand) (rest operand))
                           item)
          (values tokens mode
                  (cons (list (mode-descriptor-name mode) 0 (length tokens)) claims))))
      (values (%expression-tokens operand item) nil nil)))

;;; Operands that another alternative would win

(defun %check-operand-syntax (tokens mode item)
  (unless (nth-value 1 (try-match-operand-mode (coerce tokens 'simple-vector) mode))
    (%items-fail 'items-operand-mismatch item "the operand does not match its mode ~A"
                 (mode-descriptor-name mode))))

(defun %mode-pickables (variants)
  "The names of VARIANTS' modes, and of every ONE-OF alternative reachable from them."
  (let ((modes '()) (alternatives '()) (seen '()))
    (labels ((visit (mode)
               (unless (member mode seen)
                 (cl:push mode seen)
                 (dolist (element (mode-descriptor-pattern mode))
                   (when (eq (first element) :one-of)
                     (dolist (name (%one-of-alternatives element))
                       (cl:pushnew name alternatives :test #'%same-name-p)
                       (visit (find-mode-descriptor name *items-machine*))))))))
      (dolist (variant variants)
        (let ((mode (instruction-descriptor-mode variant)))
          (when mode
            (cl:pushnew (mode-descriptor-name mode) modes :test #'%same-name-p)
            (visit mode)))))
    (cons modes alternatives)))

(defun %mnemonic-pickables (mnemonic)
  (let ((key (string-upcase mnemonic)))
    (or (gethash key *items-pickables*)
        (setf (gethash key *items-pickables*)
              (%mode-pickables (find-instruction-variants *items-machine* mnemonic))))))

(defun %score-of (mode name tokens)
  (nth-value 6 (try-match-operand-mode tokens (or mode (find-mode-descriptor name *items-machine*)))))

(defun %claim-rival (claim line statement entry)
  "The mode the assembler chose over CLAIM's, or NIL when CLAIM holds. LINE is
the item line, STATEMENT its parsed statement and ENTRY its listing line.
A claim holds when the assembler picked that alternative for its span, or its
mode is the one the instruction's variant uses. Otherwise it loses to another
alternative picked for the same span, or, for the whole operand, to a variant
whose syntax matches more specifically."
  (destructuring-bind (name start end) claim
    (let* ((picks (listing-line-choices entry))
           (mode (instruction-descriptor-mode (listing-line-descriptor entry)))
           (tokens (statement-operand-tokens statement))
           (pickables (%mnemonic-pickables (item-line-mnemonic line))))
      (flet ((span-p (pick) (and (= (second pick) start) (= (third pick) end))))
        (cond
          ((find-if (lambda (pick) (and (span-p pick) (%same-name-p (first pick) name))) picks) nil)
          ((and mode (%same-name-p (mode-descriptor-name mode) name) (= start 0) (= end (length tokens))) nil)
          ((and (member name (cdr pickables) :test #'%same-name-p)
                (find-if #'span-p picks))
           (first (find-if #'span-p picks)))
          ((and mode (= start 0) (= end (length tokens))
                (member name (car pickables) :test #'%same-name-p)
                (%score> (%score-of mode nil tokens) (%score-of nil name tokens)))
           (mode-descriptor-name mode)))))))

(defun %check-choices (assembly lines statements unit)
  "Signal ITEMS-OPERAND-MISMATCH for an operand whose named mode lost to another
alternative of the assembled instruction (see %CLAIM-RIVAL)."
  (let ((lines (coerce lines 'simple-vector))
        (statements (coerce statements 'simple-vector)))
    (dolist (entry (assembly-listing assembly))
      (when (and (eq (listing-line-kind entry) :instruction)
                 (eq (listing-line-source-unit entry) unit)
                 (null (listing-line-definition-line entry)))
        (let* ((index (1- (listing-line-line entry)))
               (line (aref lines index)))
          (dolist (claim (item-line-claims line))
            (let ((rival (%claim-rival claim line (aref statements index) entry)))
              (when rival
                (%items-fail 'items-operand-mismatch (item-line-item line)
                             "the operand for mode ~A also matches ~A, which the assembler chose"
                             (first claim) rival)))))))))

;;; Backend operations

(defun %substitute-params (tree bindings)
  (cond ((and (symbolp tree) tree (not (keywordp tree)))
         (let ((binding (assoc (symbol-name tree) bindings :test #'string=)))
           (if binding (cdr binding) tree)))
        ((consp tree) (cons (%substitute-params (car tree) bindings)
                            (%substitute-params (cdr tree) bindings)))
        (t tree)))

(defun %rename-labels (tree renames)
  "TREE with each symbol RENAMES (an alist keyed by upcased name) maps replaced by
its string, except an instruction's mnemonic."
  (cond ((and (symbolp tree) tree (not (keywordp tree)))
         (or (cdr (assoc (symbol-name tree) renames :test #'string-equal)) tree))
        ((consp tree) (cons (%rename-labels (car tree) renames) (%rename-labels-tail (cdr tree) renames)))
        (t tree)))

(defun %rename-labels-tail (tree renames)
  (if (consp tree)
      (cons (%rename-labels (car tree) renames) (%rename-labels-tail (cdr tree) renames))
      tree))

(defun %rename-form-labels (form renames)
  (cond ((%label-form-p form) (list (first form) (or (cdr (assoc (%designator-name (second form)) renames :test #'string=))
                                                     (second form))))
        ((consp form) (cons (first form) (%rename-labels-tail (rest form) renames)))
        (t form)))

(defun %expand-op (backend name args &optional rename)
  "The forms of BACKEND's operation NAME with ARGS in place of its parameters.
The forms may include the operation's (:label NAME) forms; RENAME, when given,
maps each such NAME to the string that replaces it, before the arguments are
substituted so an argument is never mistaken for a label."
  (let* ((backend (find-backend backend))
         (item (list* :op name args))
         (entry (assoc (%designator-name name) (backend-descriptor-ops backend) :test #'equal)))
    (unless entry
      (%items-fail 'items-malformed item "backend ~A has no operation ~A" (backend-descriptor-name backend) name))
    (destructuring-bind (params &rest forms) (rest entry)
      (unless (= (length params) (length args))
        (%items-fail 'items-malformed item "operation ~A takes ~D argument~:P, got ~D"
                     name (length params) (length args)))
      (let ((bindings (mapcar #'cons params args))
            (renames (and rename
                          (loop for label in (%template-labels (first entry) params forms)
                                collect (cons label (funcall rename label))))))
        (mapcar (lambda (form)
                  (let ((form (%rename-form-labels form renames)))
                    (if (%label-form-p form)
                        form
                        (cons (first form)
                              (mapcar (lambda (operand) (%substitute-params operand bindings)) (rest form))))))
                forms)))))

(defun backend-expand-op (backend name args)
  "The forms of BACKEND's operation NAME with ARGS in place of its parameters.
A form (:label NAME) defines a label of the operation; it is not renamed here.
Signals ITEMS-MALFORMED for an unknown operation or a wrong argument count."
  (%expand-op backend name args))

(defun %fresh-label (label)
  "A name for the template LABEL, used by one expansion of an operation: local
to the enclosing label when one has been defined and the lexer has local labels."
  (let* ((base (string-downcase label))
         (descriptor *items-lexer-descriptor*)
         (prefix (lexer-descriptor-local-label-prefix descriptor))
         (local (and *items-global-label-seen* prefix (plusp (length prefix)))))
    (loop for candidate = (format nil "~@[~A~]~A" (and local prefix)
                                  (lexer-generated-name descriptor base (incf *items-serial*)))
          unless (gethash candidate *items-used-names*)
            do (setf (gethash candidate *items-used-names*) t)
               (return candidate))))

(defun %template-lines (forms item)
  "The item lines of an operation's expanded FORMS, ITEM being the item that expanded it."
  (loop for form in forms
        append (if (%label-form-p form)
                   (%item-lines form)
                   (list (%instruction-line form item)))))

(defun %op-lines (name args item)
  (%template-lines (%expand-op *items-backend* name args #'%fresh-label) item))

;;; Convention lowering (#320, #322)
;;;
;;; (:function NAME (:args n :locals n :save (reg...)) ITEM...), (:call F ARG... [:keep (reg...)]),
;;; (:return), (:push X), (:pop X) and the operands (:arg i) and (:local i) lower to
;;; the backend's reserved operations (+BACKEND-HOOK-ARITIES+), following its
;;; call and frame clauses. A frame slot is addressed by its distance from the
;;; top of the stack, or from the frame pointer when the frame has one (#321); a
;;; function body must keep the stack balanced between items, or use (:push)
;;; and (:pop), for a stack-pointer distance to hold.

;;; Without a frame pointer a label must be reached at the depth it is defined
;;; at by a branch instruction (the backend's BRANCHES), and a raw instruction
;;; that matches a stack operation is rejected unless the operation declares
;;; its :PUSHES and :POPS.

(defstruct items-frame
  nargs     ; declared arguments
  nlocals   ; declared locals
  locals    ; cells allocated for locals, padded to the frame alignment
  saves     ; registers saved on entry
  pointer   ; the frame pointer register, or NIL
  (depth 0) ; cells pushed since the prologue
  labels    ; (NAME . DEPTH) for each label defined in the body
  references) ; (NAME DEPTH ITEM) for each name an instruction of the body mentions

(defun %keyword-named-p (x name)
  (and (keywordp x) (string= (symbol-name x) name)))

(defun %backend-call-option (key)
  (getf (backend-descriptor-call *items-backend*) key))

(defun %arg-registers ()
  (let ((args (%backend-call-option :args)))
    (if (eq args :stack) '() args)))

(defun %hook-lines (name args item)
  "The lines of the backend operation NAME, which lowering ITEM needs."
  (unless (and *items-backend*
               (assoc (%designator-name name) (backend-descriptor-ops *items-backend*) :test #'equal))
    (%items-fail 'items-malformed item "~A needs the backend operation ~(~S~)~@[ (backend ~A)~]"
                 (first item) name (and *items-backend* (backend-descriptor-name *items-backend*))))
  (%op-lines name args item))

(defun %register-operand (register item)
  (let ((kind (and *items-backend* (backend-register *items-backend* :operand))))
    (unless kind
      (%items-fail 'items-malformed item "~A needs (registers :operand KIND) in the backend" (first item)))
    (list kind (string-downcase (string register)))))

(defun %slot-operand (distance item)
  (let* ((frame (backend-descriptor-frame *items-backend*))
         (stackp (and (getf frame :pointer) (null (items-frame-pointer *items-frame*))))
         (key (if stackp :stack-slot :slot))
         (kind (getf frame key)))
    (unless kind
      (%items-fail 'items-malformed item "~A needs (frame ~(~S~) KIND) in the backend" (first item) key))
    (list kind (if (eq (getf frame :grows) :up)
                   (- -1 distance)
                   distance))))

(defun %bump-depth (n)
  (when *items-frame*
    (incf (items-frame-depth *items-frame*) n)))

(defun %frame-overhead (frame)
  "Cells between a frame's locals and its return address: the saved registers and the saved frame pointer."
  (+ (length (items-frame-saves frame)) (if (items-frame-pointer frame) 1 0)))

(defun %frame-operand-p (operand)
  (and (consp operand) (or (%keyword-named-p (first operand) "ARG") (%keyword-named-p (first operand) "LOCAL"))))

(defun %frame-operand (operand item)
  "The operand (:arg i) or (:local i) addresses in the current function."
  (let ((frame *items-frame*) (index (second operand)))
    (unless frame
      (%items-fail 'items-malformed item "~S is only valid inside (:function ...)" operand))
    (unless (and (= (length operand) 2) (typep index '(integer 0)))
      (%items-fail 'items-malformed item "expected (~(~A~) INDEX), got ~S" (symbol-name (first operand)) operand))
    (let ((base (if (items-frame-pointer frame) (- (items-frame-locals frame)) (items-frame-depth frame))))
      (if (%keyword-named-p (first operand) "LOCAL")
          (progn
            (unless (< index (items-frame-nlocals frame))
              (%items-fail 'items-malformed item "~S: the function has ~D local~:P" operand (items-frame-nlocals frame)))
            (%slot-operand (+ base index) item))
          (let* ((registers (%arg-registers))
                 (nstack (max 0 (- (items-frame-nargs frame) (length registers))))
                 (stack-index (- index (length registers))))
            (unless (< index (items-frame-nargs frame))
              (%items-fail 'items-malformed item "~S: the function has ~D argument~:P" operand (items-frame-nargs frame)))
            (if (minusp stack-index)
                (%register-operand (nth index registers) item)
                (%slot-operand (+ base (items-frame-locals frame)
                                  (%frame-overhead frame)
                                  (%backend-call-option :return-address-slots)
                                  (if (eq (%backend-call-option :order) :right-to-left)
                                      stack-index
                                      (- nstack 1 stack-index)))
                               item)))))))

(defun %resolve-operand (operand item)
  (if (%frame-operand-p operand) (%frame-operand operand item) operand))

(defun %frame-register (designator role item)
  "The upcased name of DESIGNATOR, checked to be a register of ROLE."
  (let ((name (%designator-name designator)))
    (unless (and name (member name (backend-register *items-backend* role) :test #'string=))
      (%items-fail 'items-malformed item "~S is not a ~(~A~) register of backend ~A"
                   designator role (backend-descriptor-name *items-backend*)))
    name))

(defun %depth-tracked-p ()
  (and *items-frame* (null (items-frame-pointer *items-frame*))))

(defun %record-label (name)
  (when (%depth-tracked-p)
    (cl:push (cons (%designator-name name) (items-frame-depth *items-frame*))
             (items-frame-labels *items-frame*))))

(defun %branch-form-p (form)
  "True when FORM is an instruction whose operands the backend may take as branch targets."
  (let ((branches (if *items-backend* (backend-descriptor-branches *items-backend*) t)))
    (or (eq branches t)
        (member (%designator-name (first form)) branches :test #'equal))))

(defun %record-references (form item)
  "Note the names FORM's operand values mention, at the current depth, when FORM branches."
  (when (and (%depth-tracked-p) (consp form) (not (%label-form-p form)) (%branch-form-p form))
    (labels ((walk (operand)
               (typecase operand
                 (cons (dolist (value (if (%keyword-named-p (car operand) "MODE") (cddr operand) (cdr operand)))
                         (walk value)))
                 (keyword nil)
                 ((or string symbol)
                  (when operand
                    (cl:push (list (%designator-name operand) (items-frame-depth *items-frame*) item)
                             (items-frame-references *items-frame*)))))))
      (dolist (operand (rest form))
        (walk operand)))))

(defun %check-label-depths (frame)
  (dolist (reference (reverse (items-frame-references frame)))
    (destructuring-bind (name depth item) reference
      (let ((label (assoc name (items-frame-labels frame) :test #'string=)))
        (when (and label (/= depth (cdr label)))
          (%items-fail 'items-malformed item
                       "~A is reached at depth ~D but defined at depth ~D; use (:depth n) after a jump"
                       name depth (cdr label)))))))

(defun %template-matches-p (template form params)
  "True when FORM is TEMPLATE with each of PARAMS replaced by anything."
  (cond ((and (symbolp template) template (member (%designator-name template) params :test #'equal)) t)
        ((and (consp template) (consp form))
         (and (%template-matches-p (car template) (car form) params)
              (%template-matches-p (cdr template) (cdr form) params)))
        ((and (atom template) (atom form))
         (or (eql template form) (and (%designator-name template) (%same-name-p template form))))))

(defun %stack-operation-of (form)
  "The name of the backend's :push, :pop, :alloc or :free that the single instruction FORM is."
  (loop for hook in '("PUSH" "POP" "ALLOC" "FREE")
        for entry = (assoc hook (backend-descriptor-ops *items-backend*) :test #'string=)
        do (when (and entry (= (length entry) 3) (not (%label-form-p (third entry)))
                      (%template-matches-p (third entry) form (second entry)))
             (return hook))))

(defun %check-stack-forms (forms item)
  "Reject a raw FORM in a function without a frame pointer that does what the backend's stack operation does."
  (when (and (%depth-tracked-p) *items-backend*)
    (dolist (form forms)
      (when (and (consp form) (not (%label-form-p form)))
        (let ((hook (%stack-operation-of form)))
          (when hook
            (%items-fail 'items-malformed item
                         "~S does what the backend's ~(~S~) does, which the lowering cannot see; use (:push)/(:pop) or a frame pointer"
                         form (intern hook :keyword))))))))

(defun %line-operand-tokens (line)
  (coerce (loop for (operand . more) on (item-line-operands line)
                append operand
                when more collect (%punct-token :comma))
          'simple-vector))

(defun %line-variants (line)
  "The variants of LINE's instruction that its operands select best, as the assembler picks them."
  (let ((variants (handler-case (find-instruction-variants *items-machine* (item-line-mnemonic line))
                    (unknown-instruction () nil))))
    (when (item-line-forced line)
      (setf variants (%variants-in-mode variants (item-line-forced line))))
    (mapcar #'first (%best-scored (%syntax-candidates variants (%line-operand-tokens line))))))

(defun %check-stack-lines (lines item)
  "Reject, in a function without a frame pointer, an instruction line whose selected variant writes the stack pointer."
  (when (and (%depth-tracked-p) *items-backend*)
    (dolist (line lines)
      (when (and (item-line-mnemonic line)
                 (some (lambda (variant) (%stack-writer-p *items-backend* variant)) (%line-variants line)))
        (%items-fail 'items-malformed item
                     "~A writes the stack pointer, which the lowering cannot see; use (:push)/(:pop), an (:op) that declares :pushes/:pops, or a frame pointer"
                     (nth-value 1 (%layout-line line 0)))))))

(defun %op-stack-effect (name args item)
  "The net cells the backend's operation NAME puts on the stack, and true, when it declares an effect."
  (let ((declared (assoc (%designator-name name) (backend-descriptor-op-effects *items-backend*) :test #'equal)))
    (when declared
      (let ((params (second (assoc (%designator-name name) (backend-descriptor-ops *items-backend*) :test #'equal))))
        (flet ((cells (key)
                 (let ((value (getf (rest declared) key 0)))
                   (if (stringp value)
                       (let ((arg (nth (position value params :test #'equal) args)))
                         (unless (typep arg '(integer 0))
                           (%items-fail 'items-malformed item "~A: ~(~S~) ~A must be a non-negative integer, got ~S"
                                        (first item) key value arg))
                         arg)
                       value))))
          (values (- (cells :pushes) (cells :pops)) t))))))

(defun %function-lines (item)
  (unless (and (>= (length item) 3) (listp (third item)) (evenp (length (third item))))
    (%items-fail 'items-malformed item "expected (:function NAME (:args n :locals n :save (reg...)) ITEM...)"))
  (when *items-frame*
    (%items-fail 'items-malformed item "functions cannot nest"))
  (destructuring-bind (name options &rest body) (rest item)
    (loop for (key nil) on options by #'cddr
          do (unless (or (%keyword-named-p key "ARGS") (%keyword-named-p key "LOCALS") (%keyword-named-p key "SAVE")
                   (%keyword-named-p key "FRAME"))
               (%items-fail 'items-malformed item "unknown function option ~S" key)))
    (flet ((option (name default)
             (loop for (key value) on options by #'cddr
                   when (%keyword-named-p key name) return value
                   finally (return default))))
      (let ((nargs (option "ARGS" nil)) (nlocals (option "LOCALS" 0)) (saves (option "SAVE" '())))
        (unless (and (typep nlocals '(integer 0)) (or (null nargs) (typep nargs '(integer 0))) (listp saves))
          (%items-fail 'items-malformed item "expected :args and :locals to be non-negative integers and :save a list"))
        (unless (member (option "FRAME" t) '(t nil))
          (%items-fail 'items-malformed item "expected :frame to be t or nil"))
        (when (and (option "FRAME" nil) (null (getf (backend-descriptor-frame *items-backend*) :pointer)))
          (%items-fail 'items-malformed item ":frame t needs (frame :pointer REG) in the backend"))
        (when (and (null nargs)
                   (or (eq (%backend-call-option :cleanup) :callee)
                       (eq (%backend-call-option :order) :left-to-right)))
          (%items-fail 'items-malformed item "the backend's calling convention needs :args on a function"))
        (let* ((backend-pointer (getf (backend-descriptor-frame *items-backend*) :pointer))
               (framep (option "FRAME" t))
               (pointer (and framep backend-pointer))
               (saves (mapcar (lambda (register)
                                (when (equal backend-pointer (%designator-name register))
                                  (%items-fail 'items-malformed item "~A is the frame pointer; the prologue already saves it"
                                               backend-pointer))
                                (%frame-register register :callee-saved item))
                              saves))
               (alignment (getf (backend-descriptor-frame *items-backend*) :alignment))
               (locals (+ nlocals (mod (- (+ nlocals (length saves) (if pointer 1 0))) alignment)))
               (frame (make-items-frame :nargs (or nargs 0) :nlocals nlocals :locals locals :saves saves
                                        :pointer pointer))
               (lines (append (%item-lines (list :label name))
                              (loop for register in saves
                                    append (%hook-lines :push (list (%register-operand register item)) item))
                              (and pointer (%hook-lines :enter '() item))
                              (and (plusp locals) (%hook-lines :alloc (list locals) item)))))
          (let ((*items-frame* frame))
            (prog1 (append lines (loop for element in body append (%item-lines element)))
              (%check-label-depths frame))))))))

(defun %return-lines (item)
  (let ((frame *items-frame*))
    (unless (and frame (null (rest item)))
      (%items-fail 'items-malformed item "expected (:return) inside (:function ...)"))
    (unless (or (items-frame-pointer frame) (zerop (items-frame-depth frame)))
      (%items-fail 'items-malformed item "the stack is ~D cell~:P deeper than at the function's entry"
                   (items-frame-depth frame)))
    (let ((nstack (max 0 (- (items-frame-nargs frame) (length (%arg-registers))))))
      (append (if (items-frame-pointer frame)
                  (%hook-lines :leave '() item)
                  (and (plusp (items-frame-locals frame)) (%hook-lines :free (list (items-frame-locals frame)) item)))
              (loop for register in (reverse (items-frame-saves frame))
                    append (%hook-lines :pop (list (%register-operand register item)) item))
              (if (and (eq (%backend-call-option :cleanup) :callee) (plusp nstack))
                  (%hook-lines :return-pop (list nstack) item)
                  (%hook-lines :return '() item))))))

(defun %depth-lines (item)
  (unless (and *items-frame* (= (length item) 2) (typep (second item) '(integer 0)))
    (%items-fail 'items-malformed item "expected (:depth n), n a non-negative integer, inside (:function ...)"))
  (setf (items-frame-depth *items-frame*) (second item))
  '())

(defun %push-pop-lines (item)
  (unless (= (length item) 2)
    (%items-fail 'items-malformed item "expected (~(~A~) OPERAND)" (symbol-name (first item))))
  (let ((pushp (%keyword-named-p (first item) "PUSH")))
    (when (and (not pushp) *items-frame* (zerop (items-frame-depth *items-frame*)))
      (%items-fail 'items-malformed item "(:pop) has nothing pushed to pop"))
    (prog1 (%hook-lines (if pushp :push :pop) (list (second item)) item)
      (%bump-depth (if pushp 1 -1)))))

;;; Register holes

(defun %operand-shape (operand)
  "The addressing mode OPERAND names and the values it gives it, or NIL for an expression."
  (when (and (consp operand) (not (%expression-head-p (first operand))))
    (let ((head (first operand)))
      (cond ((%keyword-named-p head "MODE")
             (values (%find-mode-by-name (second operand) *items-machine*) (cddr operand) t))
            ((keywordp head) nil)
            (t (let ((entry (and *items-backend*
                                 (assoc (%designator-name head) (backend-descriptor-operands *items-backend*)
                                        :test #'equal))))
                 (and entry (values (find-mode-descriptor (cdr entry) *items-machine*) (rest operand)))))))))

(defun %map-register-holes (operand function)
  "OPERAND with FUNCTION applied to each value in a register hole of its mode."
  (multiple-value-bind (mode values namedp) (%operand-shape operand)
    (if (null mode)
        operand
        (let ((elements (copy-list (mode-descriptor-pattern mode)))
              (mapped '()))
          (loop while (and elements values)
                do (let ((element (cl:pop elements)))
                     (case (first element)
                       (:expr (let ((value (cl:pop values)))
                                (cl:push (if (and (second element) (atom value)) (funcall function value) value)
                                         mapped)))
                       (:one-of (let* ((name (cl:pop values))
                                       (alternative (find-if (lambda (candidate) (%same-name-p candidate name))
                                                             (%one-of-alternatives element))))
                                  (cl:push name mapped)
                                  (when alternative
                                    (setf elements (append (mode-descriptor-pattern
                                                            (find-mode-descriptor alternative *items-machine*))
                                                           elements))))))))
          (append (subseq operand 0 (if namedp 2 1)) (nreverse mapped) values)))))

(defun %reads-register-p (operand register)
  (let ((found nil))
    (%map-register-holes operand (lambda (value)
                                   (when (%same-name-p value register)
                                     (setf found t))
                                   value))
    found))

(defun %substitute-register (operand register replacement)
  (%map-register-holes operand (lambda (value) (if (%same-name-p value register) replacement value))))

(defun %free-scratch (moves pending target)
  "A :scratch register no move writes, no PENDING move reads and TARGET does not read."
  (find-if (lambda (register)
             (and (notany (lambda (move) (%same-name-p (first move) register)) moves)
                  (notany (lambda (move) (%reads-register-p (third move) register)) pending)
                  (not (%reads-register-p target register))))
           (backend-register *items-backend* :scratch)))

(defun %swap-registers (operand a b)
  "OPERAND with a read of register A turned into one of B and the reverse, in one pass."
  (%map-register-holes operand (lambda (value)
                                 (cond ((%same-name-p value a) (string-downcase (string b)))
                                       ((%same-name-p value b) (string-downcase (string a)))
                                       (t value)))))

(defun %self-move-p (move)
  (string-equal (princ-to-string (second move)) (princ-to-string (third move))))

(defun %has-op-p (name)
  (assoc (%designator-name name) (backend-descriptor-ops *items-backend*) :test #'equal))

(defun %exchange-candidate (pending)
  "A pending move that copies another pending move's destination register, and that register."
  (let ((kind (backend-register *items-backend* :operand)))
    (loop for move in pending
          for source = (third move)
          do (when (and kind (consp source) (= (length source) 2) (%same-name-p (first source) kind)
                        (find-if (lambda (other) (and (not (eq other move)) (%same-name-p (first other) (second source))))
                                 pending))
               (return (values move (second source)))))))

(defun %order-moves (moves target item)
  "The operations that perform MOVES, (REGISTER DESTINATION SOURCE) entries, as
(HOOK OPERAND OPERAND) entries ordered so none overwrites a register another
still to run reads. A cycle is broken by exchanging two of its registers, or
by first copying one of them into a free :scratch register."
  (let ((pending (remove-if #'%self-move-p moves))
        (ordered '()))
    (loop while pending
          do (let ((ready (find-if (lambda (move)
                                     (notany (lambda (other)
                                               (and (not (eq other move))
                                                    (%reads-register-p (third other) (first move))))
                                             pending))
                                   pending)))
               (if ready
                   (progn (cl:push (list :move (second ready) (third ready)) ordered)
                          (setf pending (remove ready pending)))
                   (multiple-value-bind (swapped other) (if (%has-op-p :exchange) (%exchange-candidate pending) nil)
                     (if swapped
                         (let ((register (first swapped)))
                           (cl:push (list :exchange (second swapped) (%register-operand other item)) ordered)
                           (setf pending (remove-if #'%self-move-p
                                                    (loop for move in (remove swapped pending)
                                                          collect (list (first move) (second move)
                                                                        (%swap-registers (third move) register other))))))
                         (let ((scratch (%free-scratch moves pending target))
                               (from (first (first pending))))
                           (unless scratch
                             (%items-fail 'items-malformed item
                                          "register arguments ~{~A~^, ~} form a cycle and the backend has no free :scratch register"
                                          (mapcar #'first pending)))
                           (cl:push (list :move (%register-operand scratch item) (%register-operand from item)) ordered)
                           (setf pending (loop for move in pending
                                               collect (list (first move) (second move)
                                                             (%substitute-register (third move) from
                                                                                   (string-downcase (string scratch))))))))))))
    (nreverse ordered)))

(defun %protect-target (moves target item)
  "The moves that copy each register TARGET reads and MOVES write into a free
:scratch register, and TARGET reading the copies."
  (let ((pending (remove-if #'%self-move-p moves))
        (copies '()))
    (when (consp target)
      (dolist (move moves)
        (let ((register (first move)))
          (when (and (%reads-register-p target register) (not (%self-move-p move)))
            (let ((scratch (%free-scratch moves pending target)))
              (unless scratch
                (%items-fail 'items-malformed item
                             "the call target reads argument register ~A and the backend has no free :scratch register"
                             register))
              (cl:push (list :move (%register-operand scratch item) (%register-operand register item)) copies)
              (setf target (%substitute-register target register (string-downcase (string scratch)))))))))
    (values (nreverse copies) target)))

(defun %keep-registers (keeps item)
  "The upcased names of the :caller-saved registers in KEEPS; :callee-saved ones
survive a call, and any other register cannot be kept."
  (loop for register in keeps
        for name = (%designator-name register)
        do (when (member name (backend-register *items-backend* :return) :test #'equal)
             (%items-fail 'items-malformed item "~A is a return register; a call overwrites it" register))
           (unless (or (member name (backend-register *items-backend* :caller-saved) :test #'equal)
                       (member name (backend-register *items-backend* :callee-saved) :test #'equal))
             (%items-fail 'items-malformed item "~A is neither a :caller-saved nor a :callee-saved register" register))
        when (member name (backend-register *items-backend* :caller-saved) :test #'equal)
          collect name))

(defun %call-lines (item)
  (unless (rest item)
    (%items-fail 'items-malformed item "expected (:call TARGET ARG... [:keep (reg...)])"))
  (destructuring-bind (target &rest all) (rest item)
    (let ((keep-position (position-if (lambda (x) (%keyword-named-p x "KEEP")) all)))
      (when (and keep-position
                 (not (and (= (+ keep-position 2) (length all)) (listp (nth (1+ keep-position) all)))))
        (%items-fail 'items-malformed item ":keep must end the call and take a list of registers"))
      (let* ((keeps (and keep-position (%keep-registers (nth (1+ keep-position) all) item)))
             (arguments (subseq all 0 keep-position))
             (registers (%arg-registers))
             (on-stack (nthcdr (length registers) arguments))
             (lines '()))
        (labels ((emit (new) (setf lines (append lines new)))
                 (push-cell (operand)
                   (emit (%hook-lines :push (list operand) item))
                   (%bump-depth 1)))
          (dolist (name keeps)
            (push-cell (%register-operand name item)))
          (dolist (argument (if (eq (%backend-call-option :order) :right-to-left) (reverse on-stack) on-stack))
            (push-cell argument))
          (let* ((moves (loop for register in registers
                              for argument in arguments
                              collect (list register (%register-operand register item) (%resolve-operand argument item))))
                 (target (%resolve-operand target item)))
            (multiple-value-bind (copies target) (%protect-target moves target item)
              (dolist (move (append copies (%order-moves moves target item)))
                (emit (%hook-lines (first move) (rest move) item)))
              (emit (%hook-lines :call (list target) item))))
          (when (and on-stack (eq (%backend-call-option :cleanup) :caller))
            (emit (%hook-lines :free (list (length on-stack)) item)))
          (%bump-depth (- (length on-stack)))
          (dolist (name (reverse keeps))
            (emit (%hook-lines :pop (list (%register-operand name item)) item))
            (%bump-depth -1)))
        lines))))

;;; Items to lines

(defstruct item-line
  label       ; string, or NIL
  mnemonic    ; string, or NIL
  suffix      ; the forced mode's suffix with its separator, or NIL
  forced      ; the forced mode descriptor, or NIL
  operands    ; list of token lists
  item        ; the item it came from
  claims)     ; (MODE START END) per named mode, as token indices into the operands

(defun %forced-suffix (mode mnemonic form item)
  "The suffix text that makes the assembler use MODE for MNEMONIC."
  (let ((separator (lexer-descriptor-mode-suffix-separator *items-lexer-descriptor*)))
    (unless (= (length form) 2)
      (%items-fail 'items-malformed item "a (:force ...) operand must be the instruction's only operand"))
    (unless (mode-descriptor-suffix mode)
      (%items-fail 'items-malformed item "mode ~A has no suffix to force it with" (mode-descriptor-name mode)))
    (unless separator
      (%items-fail 'items-malformed item "lexer ~A has no mode suffix syntax" *items-lexer*))
    (unless (member (mode-descriptor-name mode) (car (%mnemonic-pickables mnemonic)) :test #'%same-name-p)
      (%items-fail 'items-malformed item "~A is not a mode of ~A" (mode-descriptor-name mode) mnemonic))
    (concatenate 'string separator (mode-descriptor-suffix mode))))

(defun %check-forced-range (mode tokens mnemonic item)
  "Signal ITEMS-OPERAND-MISMATCH for a constant in TOKENS, the operand forced into MODE, that does not fit its field.
A value that depends on a label is checked when the assembler encodes it."
  (let ((variant (find mode (find-instruction-variants *items-machine* mnemonic)
                       :key #'instruction-descriptor-mode))
        (cell-width (%machine-cell-width *items-machine* *items-memory*)))
    (when variant
      (loop for ast in (try-match-operand-mode (coerce tokens 'simple-vector) mode)
            for i from 0
            for value = (handler-case (eval-expr-constant ast) (error () nil))
            do (when (and value (not (nth i (instruction-descriptor-relative-holes variant))))
                 (multiple-value-bind (low high) (%operand-hole-bounds variant i cell-width)
                   (unless (<= low value high)
                     (%items-fail 'items-operand-mismatch item
                                  "~D does not fit the forced mode ~A, which takes ~D to ~D"
                                  value (mode-descriptor-name mode) low high))))))))

(defun %instruction-line (form item)
  (unless (and (consp form) (or (stringp (first form)) (and (symbolp (first form)) (not (keywordp (first form))))))
    (%items-fail 'items-malformed item "~S is not an instruction" form))
  (let ((offset 0) (claims '()) (operands '()) (forced nil))
    (dolist (operand (rest form))
      (multiple-value-bind (tokens mode operand-claims forcedp) (%operand-tokens operand item)
        (when mode
          (%check-operand-syntax tokens mode item))
        (when forcedp
          (setf forced mode))
        (dolist (claim operand-claims)
          (cl:push (list (first claim) (+ offset (second claim)) (+ offset (third claim))) claims))
        (cl:push tokens operands)
        (incf offset (1+ (length tokens)))))
    (let* ((mnemonic (%source-name (first form) item))
           (suffix (and forced (%forced-suffix forced mnemonic form item))))
      (when forced
        (%check-forced-range forced (first operands) mnemonic item))
      (make-item-line :mnemonic mnemonic :operands (nreverse operands) :suffix suffix :forced forced
                      :item item :claims (nreverse claims)))))

(defun %directive-line (item)
  (destructuring-bind (name &rest args) (rest item)
    (let ((name (%source-name name item)))
      (make-item-line :mnemonic (if (and (plusp (length name)) (char= (char name 0) #\.))
                                    name
                                    (concatenate 'string "." name))
                      :operands (mapcar (lambda (arg)
                                          (if (stringp arg)
                                              (list (%string-token arg))
                                              (%expression-tokens arg item)))
                                        args)))))

(defun %item-lines (item)
  (unless (and (consp item) (listp (cdr item)) (or (symbolp (first item)) (stringp (first item))))
    (%items-fail 'items-malformed item "~S is not an item" item))
  (let ((head (first item)))
    (cond
      ((and (keywordp head) (string= (symbol-name head) "LABEL"))
       (unless (and (= (length item) 2) (or (stringp (second item)) (symbolp (second item))))
         (%items-fail 'items-malformed item "expected (:label NAME)"))
       (let ((name (%source-name (second item) item)))
         (unless (%local-name-p name)
           (setf *items-global-label-seen* t))
         (%record-label name)
         (list (make-item-line :label name))))
      ((and (keywordp head) (string= (symbol-name head) "DIRECTIVE"))
       (unless (rest item)
         (%items-fail 'items-malformed item "expected (:directive NAME expr...)"))
       (list (%directive-line item)))
      ((and (keywordp head) (string= (symbol-name head) "OP"))
       (unless (and (rest item) *items-backend*)
         (%items-fail 'items-malformed item "(:op NAME arg...) needs a backend"))
       (when (and (%depth-tracked-p) (member (%designator-name (second item)) '("PUSH" "POP" "ALLOC" "FREE") :test #'equal))
         (%items-fail 'items-malformed item "(:op ~(~S~) ...) changes the stack depth; use (:push)/(:pop) or a frame pointer"
                      (second item)))
       (let ((forms (%expand-op *items-backend* (second item) (cddr item))))
         (multiple-value-bind (effect declaredp) (%op-stack-effect (second item) (cddr item) item)
           (unless declaredp
             (%check-stack-forms forms item))
           (dolist (form forms)
             (%record-references form item))
           (let ((lines (%op-lines (second item) (cddr item) item)))
             (if declaredp
                 (%bump-depth effect)
                 (%check-stack-lines lines item))
             lines))))
      ((%keyword-named-p head "FUNCTION") (%function-lines item))
      ((%keyword-named-p head "CALL") (%call-lines item))
      ((%keyword-named-p head "RETURN") (%return-lines item))
      ((or (%keyword-named-p head "PUSH") (%keyword-named-p head "POP")) (%push-pop-lines item))
      ((%keyword-named-p head "DEPTH") (%depth-lines item))
      ((keywordp head)
       (%items-fail 'items-malformed item "unknown item ~S" head))
      (t (%check-stack-forms (list item) item)
         (%record-references item item)
         (let ((lines (list (%instruction-line item item))))
           (%check-stack-lines lines item)
           lines)))))

(defun %layout-line (line number)
  "The tokens of LINE, placed on source line NUMBER, and that line's text."
  (let ((tokens '()) (column 1) (out (make-string-output-stream)) (start t))
    (labels ((emit (token &optional (space t))
               (when (and space (not start))
                 (write-char #\Space out)
                 (incf column))
               (setf start nil
                     (token-line token) number
                     (token-column token) column)
               (let ((text (%token-source token)))
                 (write-string text out)
                 (incf column (length text)))
               (cl:push token tokens)))
      (when (item-line-label line)
        (let ((suffix (lexer-descriptor-label-suffix *items-lexer-descriptor*)))
          (unless suffix
            (%items-fail 'items-malformed nil "lexer ~A has no label syntax" *items-lexer*))
          (emit (%name-token (item-line-label line)))
          (emit (make-token :type :label-suffix :value :label-suffix :text suffix) nil)))
      (when (item-line-mnemonic line)
        (emit (%name-token (concatenate 'string (item-line-mnemonic line) (or (item-line-suffix line) "")))))
      (loop for (operand . more) on (item-line-operands line)
            do (loop for token in operand
                     do (emit token))
               (when more
                 (emit (%punct-token :comma) nil))))
    (values (nreverse tokens) (get-output-stream-string out))))

;;; Assembling

(defun %find-by-name (designator table)
  "The key of hash TABLE named DESIGNATOR."
  (and designator
       (loop for key being the hash-keys of table
             when (%same-name-p key designator) return key)))

(defun %items-context (backend machine lexer)
  "Values BACKEND, MACHINE and LEXER resolved."
  (let* ((backend (and backend (find-backend backend)))
         (machine (cond ((and backend machine
                              (not (%same-name-p (backend-descriptor-machine backend) machine)))
                         (%signal-usage-error 'usage-error "backend ~A targets machine ~A, not ~A"
                                              (backend-descriptor-name backend)
                                              (backend-descriptor-machine backend) machine))
                        (backend (backend-descriptor-machine backend))
                        (machine (or (%find-machine-name machine)
                                     (%lookup-error 'unknown-machine machine "No machine named ~S" machine)))
                        (t (%signal-usage-error 'usage-error "items need a :machine or a :backend"))))
         (lexer (or (%find-by-name lexer *lexers*)
                    (%lookup-error 'unknown-lexer lexer "No lexer named ~S" lexer))))
    (values backend machine lexer)))

(defmacro %with-items-context ((backend machine lexer &optional memory) &body body)
  `(multiple-value-bind (backend* machine* lexer*) (%items-context ,backend ,machine ,lexer)
     (let* ((*items-backend* backend*)
            (*items-machine* machine*)
            (*items-memory* (and ,memory
                                 (or (%find-element-name machine* ,memory)
                                     (%signal-usage-error 'usage-error "No memory named ~S" ,memory))))
            (*items-lexer* lexer*)
            (*items-lexer-descriptor* (find-lexer-descriptor lexer*))
            (*items-literals* (make-hash-table :test 'equal))
            (*items-pickables* (make-hash-table :test 'equal))
            (*items-serial* 0)
            (*items-used-names* (make-hash-table :test 'equal))
            (*items-global-label-seen* nil)
            (*mode-scope* machine*)
            (*register-alias-elements*
              (machine-descriptor-register-alias-elements (find-machine-descriptor machine*))))
       ,@body)))

(defun %seed-used-names (tree)
  "Record every name TREE mentions, so generated labels differ from all of them."
  (typecase tree
    (cons (%seed-used-names (car tree)) (%seed-used-names (cdr tree)))
    (string (setf (gethash tree *items-used-names*) t))
    (symbol (when tree
              (setf (gethash (%source-name tree nil) *items-used-names*) t)))))

(defun %items-source (items)
  "The statements ITEMS make, the source text they render as, its unit and the item lines."
  (%seed-used-names items)
  (let* ((lines (loop for item in items append (%item-lines item)))
         (descriptor *items-lexer-descriptor*)
         (text (make-string-output-stream))
         (token-lines (loop for line in lines
                            for number from 1
                            collect (multiple-value-bind (tokens source) (%layout-line line number)
                                      (write-line source text)
                                      tokens))))
    (let* ((text (get-output-stream-string text))
           (unit (make-source-unit :text text))
           (statements (mapcar (lambda (tokens)
                                 (let ((statement (%parse-line tokens
                                                               :mode-suffix-separator
                                                               (lexer-descriptor-mode-suffix-separator descriptor)
                                                               :hole-prefix-separator
                                                               (lexer-descriptor-hole-prefix-separator descriptor))))
                                   (setf (statement-source-unit statement) unit)
                                   statement))
                               token-lines)))
      (values statements text unit lines))))

(defun render-items (items &key backend machine (lexer 'default) memory)
  "The assembly source text ITEMS render as. Assembling it gives the cells
ASSEMBLE-ITEMS gives. MEMORY sizes the range check of a forced mode's operand."
  (%with-items-context (backend machine lexer memory)
    (nth-value 1 (%items-source items))))

(defun assemble-items (items &key backend machine (lexer 'default) (origin 0) memory file)
  "Assemble ITEMS, a list of items, for the machine of BACKEND (a name or
BACKEND-DESCRIPTOR) or for MACHINE. Returns an ASSEMBLY like ASSEMBLE, whose
source is the text RENDER-ITEMS gives; LEXER, ORIGIN and MEMORY are ASSEMBLE's.
FILE names the program in diagnostics. Signals ITEMS-MALFORMED for an item that
is not well formed and ITEMS-OPERAND-MISMATCH for an operand that does not
match its mode, or that the assembler read as another alternative of the
instruction's modes; the assembler's own conditions otherwise."
  (%with-items-context (backend machine lexer memory)
    (multiple-value-bind (statements text unit lines) (%items-source items)
      (setf (source-unit-file unit) (and file (namestring (pathname file))))
      (let ((assembly (with-source-unit unit
                        (assemble-statements statements
                                             :machine *items-machine* :lexer *items-lexer* :origin origin
                                             :memory *items-memory*
                                             :source text :source-unit unit))))
        (%check-choices assembly lines statements unit)
        assembly))))

(defun items-size (items &key backend machine (lexer 'default) (origin 0) memory (assume :widest))
  "The cells ITEMS occupy, from their first cell to the end of their last, laid
out as ASSEMBLE-ITEMS would but without encoding, so a label ITEMS never define
is allowed. An operand naming such a label is sized at its :WIDEST or :NARROWEST
variant, as ASSUME says; every other choice is the assembler's. The keys are
ASSEMBLE-ITEMS's."
  (unless (member assume '(:widest :narrowest))
    (%signal-usage-error 'usage-error ":assume must be :widest or :narrowest, not ~S" assume))
  (%with-items-context (backend machine lexer memory)
    (multiple-value-bind (statements text unit) (%items-source items)
      (let ((*unresolved-width* assume))
        (with-source-unit unit
          (%with-laid-out-statements (statements *items-machine* *items-lexer* origin *items-memory* text)
              (symbols sized final-address asm-origin info label-banks cell-width endian)
            (- final-address asm-origin)))))))

(defun %find-element-name (machine designator)
  (let ((element (find-if (lambda (element) (%same-name-p (storage-element-name element) designator))
                          (machine-descriptor-elements (find-machine-descriptor machine)))))
    (and element (storage-element-name element))))

;;; Files

(defun %program-fail (control &rest args)
  (%items-fail 'items-malformed nil "~?" control args))

(defun %parse-program (form)
  (unless (and (consp form) (listp (cdr form)) (keywordp (first form)) (string= (symbol-name (first form)) "PROGRAM")
               (listp (second form)))
    (%program-fail "expected (:program (option...) item...)"))
  (let ((program (make-items-program :items (cddr form))) (options (second form)))
    (unless (evenp (length options))
      (%program-fail "the :program options must be keyword/value pairs"))
    (loop for (key value) on options by #'cddr
          do (let ((name (and (keywordp key) (symbol-name key))))
               (cond ((equal name "BACKEND") (setf (items-program-backend program) value))
                     ((equal name "MACHINE") (setf (items-program-machine program) value))
                     ((equal name "MEMORY") (setf (items-program-memory program) value))
                     ((equal name "LEXER") (setf (items-program-lexer program) value))
                     ((equal name "ORIGIN")
                      (unless (typep value '(integer 0))
                        (%program-fail ":origin must be a non-negative integer, got ~S" value))
                      (setf (items-program-origin program) value))
                     (t (%program-fail "unknown :program option ~S" key)))))
    program))

(defun read-items (path)
  "The ITEMS-PROGRAM in the file PATH: one (:program (option...) item...) form,
with options :backend, :machine, :memory, :lexer and :origin. The file is
untrusted: it is read without evaluation and without interning symbols.
Signals ITEMS-MALFORMED for an unreadable or malformed file."
  (%parse-program
   (with-open-file (in path)
     (read-restricted-form in (lambda (control &rest args)
                                (apply #'%program-fail control args))
                           path :bare :uninterned))))

(defun read-items-from-string (string)
  "Like READ-ITEMS, for the text STRING."
  (%parse-program
   (with-input-from-string (in string)
     (read-restricted-form in (lambda (control &rest args)
                                (apply #'%program-fail control args))
                           "items" :bare :uninterned))))

(defun assemble-items-file (path &key backend machine lexer origin memory)
  "Read the items program at PATH and assemble it. The keys override the
program's own options."
  (let* ((program (read-items path))
         (truename (truename path))
         (*include-directory* (%file-directory truename))
         (*include-chain* (list truename))
         (assembly (assemble-items (items-program-items program)
                                   :backend (or backend (items-program-backend program))
                                   :machine (or machine (items-program-machine program))
                                   :lexer (or lexer (items-program-lexer program) 'default)
                                   :origin (or origin (items-program-origin program) 0)
                                   :memory (or memory (items-program-memory program))
                                   :file path)))
    (setf (source-unit-path (assembly-source-unit assembly)) (namestring truename))
    assembly))
