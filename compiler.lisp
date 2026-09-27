;;;; compiler.lisp
;;;; #319: a small Lisp-like source language compiled to items through a
;;;; backend (backend.lisp, items.lisp). Values are plain machine words. Every
;;;; expression leaves its value in the backend's first :RETURN register, the
;;;; accumulator, and binary operators combine it with a second register
;;;; through the backend's operations (+BACKEND-LANGUAGE-OP-ARITIES+).
;;;;
;;;; Forms:  (defun NAME (PARAM...) BODY...)  (defvar NAME [INT])  (defconstant NAME INT)
;;;; Expressions: an integer or name, (set N E), (let ((V E)...) BODY...),
;;;;   (if C A [B]), (while C BODY...), (progn E...), (and E...), (or E...),
;;;;   (not E), an operator, (peek A), (poke A V), (return [E]), (asm ITEM...),
;;;;   (F ARG...)
;;;;
;;;; Symbols are compared by name: source is read without interning.

(in-package #:lasm)

;; #362: a LASM-SYNTAX-ERROR so DIAGNOSTIC-TEXT renders FILE:LINE:COLUMN and,
;; when the program's source text is known (READ-SOURCE, READ-SOURCE-FROM-
;; STRING), the offending line with a caret. DETAIL, FORM and FUNCTION are
;; the plain message parts; MESSAGE (on LASM-SYNTAX-ERROR) is all of them
;; combined, for the :REPORT method to print.
(define-condition program-compile-error (lasm-syntax-error)
  ((detail :initarg :detail :reader program-compile-error-detail)
   (form :initarg :form :initform nil :reader program-compile-error-form)
   (function :initarg :function :initform nil :reader program-compile-error-function)))

(defparameter *cc-source-keywords* '(:var)
  "Keywords that source text uses, which the reader accepts only once they exist.")

(defvar *cc-backend* nil)
(defvar *cc-acc* nil "The accumulator register operand.")
(defvar *cc-temp* nil "The register operand that holds a right operand.")
(defvar *cc-acc-name* nil "The accumulator's name, for the operations that take register names.")
(defvar *cc-temp-name* nil "The temporary register's name.")
(defvar *cc-functions* nil "Upcased name -> (LABEL . ARITY).")
(defvar *cc-globals* nil "Upcased name -> label symbol.")
(defvar *cc-constants* nil "Upcased name -> integer.")
(defvar *cc-function* nil "The source name of the function being compiled.")
(defvar *cc-form* nil "The innermost expression being compiled.")
(defvar *cc-out* nil "The items of the current function or stub, reversed.")
(defvar *cc-env* nil "(KEY . LOCATION) for each parameter and let variable in scope.")
(defvar *cc-next* 0 "The next free local slot.")
(defvar *cc-max* 0 "Local slots the function needs.")
(defvar *cc-labels* 0 "Control labels made so far.")
(defvar *cc-depth* 0 "Temporaries the compiler has pushed since the function's entry.")
(defvar *cc-positions* nil "EQ hash table, form -> character offset, or NIL without one (#362).")
(defvar *cc-source* nil "The program's source text, or NIL.")
(defvar *cc-file* nil "The program's path, or NIL.")

(defun %cc-line-column (form)
  "(VALUES LINE COLUMN) of FORM, when its position and the source text are known."
  (let ((offset (and *cc-positions* form (gethash form *cc-positions*))))
    (and offset *cc-source* (%offset-line-column *cc-source* offset))))

(defun %cc-fail (form control &rest args)
  (let* ((detail (apply #'format nil control args))
         (message (let ((*print-gensym* nil) (*print-case* :downcase) (*print-length* 8) (*print-level* 4))
                    (format nil "~A~@[ (in ~S)~]~@[ (function ~A)~]" detail form *cc-function*))))
    (multiple-value-bind (line column) (%cc-line-column form)
      (error 'program-compile-error :detail detail :form form :function *cc-function*
                                     :message message :line line :column column
                                     :file *cc-file* :source *cc-source*))))

;;; Names

(defun %cc-name-p (x)
  (and x (symbolp x) (not (keywordp x))))

(defun %cc-key (x form)
  (unless (%cc-name-p x)
    (%cc-fail form "expected a name, got ~S" x))
  (%designator-name x))

(defun %cc-symbol (string)
  "A symbol that names STRING when items are rendered, and prints readably as it."
  (make-symbol (if (string= string (string-downcase string)) (string-upcase string) string)))

(defun %cc-mangle (prefix name)
  "The label for source NAME: alphanumeric so any lexer reads it, and prefixed so it
cannot be a register alias or a generated label."
  (%cc-symbol (with-output-to-string (out)
                (write-string prefix out)
                (loop for char across (%source-name name nil)
                      do (if (and (< (char-code char) 128) (alphanumericp char))
                             (write-char char out)
                             (format out "z~(~X~)z" (char-code char)))))))

(defun %cc-new-label ()
  (%cc-symbol (format nil "lbl~D" (incf *cc-labels*))))

;;; Emission

(defun %cc-emit (item)
  (cl:push item *cc-out*))

(defun %cc-op (name &rest args)
  "Emit the backend operation NAME, which must exist."
  (unless (assoc (string name) (backend-descriptor-ops *cc-backend*) :test #'string=)
    (%cc-fail *cc-form* "the backend ~A needs the operation ~(~S~)"
              (backend-descriptor-name *cc-backend*) name))
  (%cc-emit (list* :op name args)))

(defun %cc-alloc ()
  (prog1 (list :local *cc-next*)
    (incf *cc-next*)
    (setf *cc-max* (max *cc-max* *cc-next*))))

(defun %cc-free ()
  (decf *cc-next*))

(defun %cc-const (value)
  (%cc-op :const *cc-acc* value))

;;; Expressions

(defun %cc-lookup (symbol)
  "The location of the variable SYMBOL: (:LOCAL i), (:ARG i), (:GLOBAL LABEL) or (:CONSTANT n)."
  (let ((key (%cc-key symbol *cc-form*)))
    (or (cdr (assoc key *cc-env* :test #'string=))
        (let ((label (gethash key *cc-globals*)))
          (and label (list :global label)))
        (let ((value (gethash key *cc-constants*)))
          (and value (list :constant value)))
        (%cc-fail *cc-form* "unknown variable ~A" (%source-name symbol nil)))))

;; A leaf (an integer or a variable name) loads straight into any register
;; with :const/:get/:peek (#364), instead of always going through the
;; accumulator and the stack.
(defun %cc-leaf-p (form)
  (or (integerp form) (%cc-name-p form)))

(defun %cc-load-leaf (form register register-name)
  "Load leaf FORM into REGISTER; REGISTER-NAME names it, for :peek."
  (if (integerp form)
      (%cc-op :const register form)
      (let ((location (%cc-lookup form)))
        (ecase (first location)
          ((:local :arg) (%cc-op :get register location))
          (:global (%cc-op :const register (second location))
           (%cc-op :peek register-name register-name))
          (:constant (%cc-op :const register (second location)))))))

(defun %cc-variable (symbol)
  (%cc-load-leaf symbol *cc-acc* *cc-acc-name*))

(defun %cc-progn (forms)
  (if forms
      (dolist (form forms) (%cc-expr form))
      (%cc-const 0)))

;; TODO: a non-leaf operand still goes through the stack, one push per
;; operand; a register allocator over the backend's scratch registers would
;; avoid it when one is free (#373).
(defun %cc-to-temp (form)
  "FORM's value into the temp register, leaving the accumulator as it is."
  (if (%cc-leaf-p form)
      (%cc-load-leaf form *cc-temp* *cc-temp-name*)
      (progn (%cc-emit (list :push *cc-acc*))
             (incf *cc-depth*)
             (%cc-expr form)
             (%cc-op :move *cc-temp* *cc-acc*)
             (%cc-emit (list :pop *cc-acc*))
             (decf *cc-depth*))))

(defun %cc-value-to-temp (form)
  "FORM's value into the temp register, when the accumulator holds nothing to keep."
  (if (%cc-leaf-p form)
      (%cc-load-leaf form *cc-temp* *cc-temp-name*)
      (progn (%cc-expr form)
             (%cc-op :move *cc-temp* *cc-acc*))))

(defun %cc-affects-p (key tree)
  "T when TREE, an operand's source form, might write KEY (a variable's
%DESIGNATOR-NAME): a (set KEY ...), or any (asm ...), which can reach it
through (:var KEY)."
  (and (consp tree)
       (let ((head (and (%cc-name-p (first tree)) (%designator-name (first tree)))))
         (or (equal head "ASM")
             (and (equal head "SET")
                  (equal key (and (%cc-name-p (second tree)) (%designator-name (second tree)))))
             (some (lambda (element) (%cc-affects-p key element)) tree)))))

(defun %cc-swappable-p (left right)
  "T when LEFT, an operator's left operand, can be loaded after RIGHT is
evaluated: an integer or a constant always can; a local or argument can when
RIGHT does not (set) it or reach it through an (asm ...) block. A global
never swaps -- a call or poke in RIGHT could change it."
  (or (integerp left)
      (and (%cc-name-p left)
           (let ((location (%cc-lookup left)))
             (case (first location)
               (:constant t)
               ((:local :arg) (not (%cc-affects-p (%designator-name left) right)))
               (t nil))))))

(defun %cc-operands (left right)
  "Compile LEFT into the accumulator and RIGHT into the temp register, in
whichever order avoids the stack (#364): RIGHT first, when LEFT is safe to
load afterwards (%CC-SWAPPABLE-P); otherwise LEFT then %CC-TO-TEMP."
  (if (and (not (%cc-leaf-p right)) (%cc-swappable-p left right))
      (progn (%cc-expr right)
             (%cc-op :move *cc-temp* *cc-acc*)
             (%cc-load-leaf left *cc-acc* *cc-acc-name*))
      (progn (%cc-expr left)
             (%cc-to-temp right))))

(defparameter *cc-operators*
  '(("+" :add 2 nil) ("-" :sub 1 nil) ("*" :mul 2 nil) ("/" :div 2 2) ("MOD" :mod 2 2)
    ("LOGAND" :and 2 nil) ("LOGIOR" :or 2 nil) ("LOGXOR" :xor 2 nil) ("SHL" :shl 2 2) ("SHR" :shr 2 2)
    ("=" :eq 2 2) ("/=" :ne 2 2) ("<" :lt 2 2) (">" :gt 2 2) ("<=" :le 2 2) (">=" :ge 2 2))
  "Operator name, the backend operation, and the fewest and most operands.")

(defun %cc-operator (entry form)
  (destructuring-bind (name op least most) entry
    (let ((args (rest form)))
      (unless (and (>= (length args) least) (or (null most) (<= (length args) most)))
        (%cc-fail form "~(~A~) takes ~D~@[ to ~D~] operand~:P, got ~D"
                  name least (and most (/= most least) most) (length args)))
      (cond ((and (eq op :sub) (null (rest args)))
             (%cc-operands 0 (first args))
             (%cc-op :sub *cc-acc* *cc-temp*))
            (t (%cc-operands (first args) (second args))
               (%cc-op op *cc-acc* *cc-temp*)
               (dolist (arg (cddr args))
                 (%cc-to-temp arg)
                 (%cc-op op *cc-acc* *cc-temp*)))))))

(defun %cc-not (form)
  (%cc-check-length form 2 2)
  (%cc-operands 0 (second form))
  (%cc-op :eq *cc-acc* *cc-temp*))

(defun %cc-check-length (form least most)
  (unless (and (listp (cdr form)) (null (cdr (last form)))
               (>= (length form) least) (or (null most) (<= (length form) most)))
    (%cc-fail form "~(~A~) is malformed" (%source-name (first form) nil))))

(defun %cc-progn-form (form)
  (%cc-check-length form 1 nil)
  (%cc-progn (rest form)))

(defun %cc-if (form)
  (%cc-check-length form 3 4)
  (let ((else (%cc-new-label)) (end (%cc-new-label)))
    (%cc-expr (second form))
    (%cc-op :branch-zero *cc-acc* else)
    (%cc-expr (third form))
    (%cc-op :jump end)
    (%cc-emit (list :label else))
    (if (fourth form) (%cc-expr (fourth form)) (%cc-const 0))
    (%cc-emit (list :label end))))

(defun %cc-while (form)
  (%cc-check-length form 3 nil)
  (let ((top (%cc-new-label)) (end (%cc-new-label)))
    (%cc-emit (list :label top))
    (%cc-expr (second form))
    (%cc-op :branch-zero *cc-acc* end)
    (dolist (body (cddr form)) (%cc-expr body))
    (%cc-op :jump top)
    (%cc-emit (list :label end))
    (%cc-const 0)))

(defun %cc-and (form)
  (%cc-check-length form 1 nil)
  (if (null (rest form))
      (%cc-const 1)
      (let ((end (%cc-new-label)))
        (loop for (arg . more) on (rest form)
              do (%cc-expr arg)
                 (when more
                   (%cc-op :branch-zero *cc-acc* end)))
        (%cc-emit (list :label end)))))

(defun %cc-or (form)
  (%cc-check-length form 1 nil)
  (if (null (rest form))
      (%cc-const 0)
      (let ((end (%cc-new-label)))
        (loop for (arg . more) on (rest form)
              do (%cc-expr arg)
                 (when more
                   (let ((next (%cc-new-label)))
                     (%cc-op :branch-zero *cc-acc* next)
                     (%cc-op :jump end)
                     (%cc-emit (list :label next)))))
        (%cc-emit (list :label end)))))

(defun %cc-let (form)
  (%cc-check-length form 2 nil)
  (unless (and (listp (second form)) (null (cdr (last (second form)))))
    (%cc-fail form "let needs a list of (NAME VALUE) bindings"))
  (let ((*cc-env* *cc-env*) (slots 0))
    (dolist (binding (second form))
      (unless (and (consp binding) (= (length binding) 2))
        (%cc-fail form "let binding ~S is not (NAME VALUE)" binding))
      (%cc-expr (second binding))
      (let ((slot (%cc-alloc)))
        (%cc-op :set slot *cc-acc*)
        (incf slots)
        (cl:push (cons (%cc-key (first binding) form) slot) *cc-env*)))
    (%cc-progn (cddr form))
    (dotimes (i slots) (%cc-free))))

(defun %cc-set (form)
  (%cc-check-length form 3 3)
  (let ((location (%cc-lookup (second form))))
    (ecase (first location)
      ((:local :arg) (%cc-expr (third form))
       (%cc-op :set location *cc-acc*))
      (:global (%cc-value-to-temp (third form))
       (%cc-op :const *cc-acc* (second location))
       (%cc-op :poke *cc-acc-name* *cc-temp-name*)
       (%cc-op :move *cc-acc* *cc-temp*))
      (:constant (%cc-fail form "~A is a constant" (%source-name (second form) nil))))))

(defun %cc-peek (form)
  (%cc-check-length form 2 2)
  (%cc-expr (second form))
  (%cc-op :peek *cc-acc-name* *cc-acc-name*))

(defun %cc-poke (form)
  (%cc-check-length form 3 3)
  (%cc-operands (second form) (third form))
  (%cc-op :poke *cc-acc-name* *cc-temp-name*)
  (%cc-op :move *cc-acc* *cc-temp*))

;; #363: an early return. Pending temporaries (each binary operator's left
;; operand, or a POKE's address) sit on the stack above the frame's own
;; locals, which (:return) cannot see -- pop them back off first, then leave
;; the rest of the body's depth tracking (ITEMS-FRAME-DEPTH) where it was
;; with (:depth n).
(defun %cc-return (form)
  (%cc-check-length form 1 2)
  (if (second form) (%cc-expr (second form)) (%cc-const 0))
  (dotimes (i *cc-depth*) (%cc-emit (list :pop *cc-temp*)))
  (%cc-emit (list :return))
  (when (plusp *cc-depth*) (%cc-emit (list :depth *cc-depth*))))

(defun %cc-substitute-variables (tree form)
  "TREE with each (:VAR NAME) replaced by the operand or label of that variable."
  (cond ((and (consp tree) (%keyword-named-p (first tree) "VAR"))
         (unless (and (= (length tree) 2) (%cc-name-p (second tree)))
           (%cc-fail form "expected (:var NAME), got ~S" tree))
         (let ((location (let ((*cc-form* form)) (%cc-lookup (second tree)))))
           (ecase (first location)
             ((:local :arg) location)
             (:global (second location))
             (:constant (second location)))))
        ((and (consp tree) (null (cdr (last tree))))
         (mapcar (lambda (element) (%cc-substitute-variables element form)) tree))
        (t tree)))

(defun %cc-asm (form)
  (%cc-check-length form 1 nil)
  (dolist (item (rest form))
    (%cc-emit (%cc-substitute-variables item form))))

(defun %cc-call (form)
  (let* ((entry (gethash (%cc-key (first form) form) *cc-functions*))
         (args (rest form)))
    (unless entry
      (%cc-fail form "unknown function ~A" (%source-name (first form) nil)))
    (%cc-check-length form 1 nil)
    (unless (= (length args) (cdr entry))
      (%cc-fail form "~A takes ~D argument~:P, got ~D"
                (%source-name (first form) nil) (cdr entry) (length args)))
    (let ((slots '()))
      (dolist (arg args)
        (%cc-expr arg)
        (let ((slot (%cc-alloc)))
          (%cc-op :set slot *cc-acc*)
          (cl:push slot slots)))
      (%cc-emit (list* :call (car entry) (reverse slots)))
      (dolist (slot slots)
        (declare (ignore slot))
        (%cc-free)))))

(defparameter *cc-forms*
  '(("PROGN" . %cc-progn-form) ("IF" . %cc-if) ("WHILE" . %cc-while) ("AND" . %cc-and) ("OR" . %cc-or)
    ("NOT" . %cc-not) ("LET" . %cc-let) ("SET" . %cc-set) ("PEEK" . %cc-peek) ("POKE" . %cc-poke)
    ("ASM" . %cc-asm) ("RETURN" . %cc-return)))

(defun %cc-expr (form)
  (typecase form
    (integer (%cc-const form))
    (null (%cc-fail form "expected an expression, got nil"))
    (symbol (if (keywordp form)
                (%cc-fail form "expected an expression, got ~S" form)
                (%cc-variable form)))
    (cons (let ((*cc-form* form))
            (unless (and (%cc-name-p (first form)) (listp (cdr form)))
              (%cc-fail form "expected (NAME ARG...)"))
            (let* ((key (%designator-name (first form)))
                   (special (cdr (assoc key *cc-forms* :test #'string=)))
                   (operator (assoc key *cc-operators* :test #'string=)))
              (cond (special (funcall special form))
                    (operator (%cc-operator operator form))
                    (t (%cc-call form))))))
    (t (%cc-fail form "expected an expression, got ~S" form))))

;;; Program

(defun %cc-function (definition)
  (destructuring-bind (name params body label) definition
    (let* ((*cc-function* (%source-name name nil))
           (*cc-out* '()) (*cc-env* '()) (*cc-next* 0) (*cc-max* 0) (*cc-depth* 0)
           (arg-registers (let ((args (getf (backend-descriptor-call *cc-backend*) :args)))
                            (if (eq args :stack) 0 (length args)))))
      (loop for param in params
            for index from 0
            do (let ((key (%cc-key param name)))
                 (when (assoc key *cc-env* :test #'string=)
                   (%cc-fail name "~A is a parameter twice" (%source-name param nil)))
                 (if (< index arg-registers)
                     (let ((slot (%cc-alloc)))
                       (let ((*cc-form* name))
                         (%cc-op :set slot (list :arg index)))
                       (cl:push (cons key slot) *cc-env*))
                     (cl:push (cons key (list :arg index)) *cc-env*))))
      (%cc-progn body)
      (list* :function label (list :args (length params) :locals *cc-max*)
             (nreverse (cl:push (list :return) *cc-out*))))))

(defun %cc-register-operand (name)
  (list (%cc-symbol (string-downcase (getf (backend-descriptor-registers *cc-backend*) :operand)))
        (%cc-symbol (string-downcase name))))

(defun %cc-registers ()
  "Set the accumulator and the temporary register from the backend."
  (let* ((registers (backend-descriptor-registers *cc-backend*))
         (acc (first (getf registers :return)))
         (pointer (getf (backend-descriptor-frame *cc-backend*) :pointer))
         (temp (and acc
                    (find-if (lambda (name) (and (string/= name acc) (not (equal name pointer))))
                             (append (getf registers :scratch) (getf registers :caller-saved))))))
    (unless (getf registers :operand)
      (%cc-fail nil "the backend needs (registers :operand KIND)"))
    (unless acc
      (%cc-fail nil "the backend needs (registers :return (REGISTER))"))
    (unless temp
      (%cc-fail nil "the backend needs a :scratch or :caller-saved register besides ~(~A~)" acc))
    (setf *cc-acc* (%cc-register-operand acc)
          *cc-temp* (%cc-register-operand temp)
          *cc-acc-name* (%cc-symbol (string-downcase acc))
          *cc-temp-name* (%cc-symbol (string-downcase temp)))))

(defun %cc-collect (forms)
  "(VALUES DEFINITIONS GLOBALS), registering functions, globals and constants."
  (let ((definitions '()) (globals '()) (seen (make-hash-table :test 'equal)))
    (flet ((claim (name form label)
             (let ((existing (gethash (symbol-name label) seen)))
               (when (and existing (string/= existing (%designator-name name)))
                 (%cc-fail form "~A and ~A both make the label ~A"
                           (%source-name name nil) existing (symbol-name label)))
               (setf (gethash (symbol-name label) seen) (%designator-name name)))))
      (dolist (form forms)
        (unless (and (consp form) (%cc-name-p (first form)) (listp (cdr form)) (null (cdr (last form))))
          (%cc-fail form "expected (defun ...), (defvar ...) or (defconstant ...)"))
        (let ((head (%designator-name (first form))))
          (cond
            ((string= head "DEFUN")
             (unless (and (>= (length form) 3) (listp (third form)) (null (cdr (last (third form)))))
               (%cc-fail form "expected (defun NAME (PARAM...) BODY...)"))
             (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "fn" name)))
               (when (or (assoc key *cc-forms* :test #'string=) (assoc key *cc-operators* :test #'string=))
                 (%cc-fail form "~A is a built-in form" (%source-name name nil)))
               (when (gethash key *cc-functions*)
                 (%cc-fail form "~A is defined twice" (%source-name name nil)))
               (claim name form label)
               (setf (gethash key *cc-functions*) (cons label (length (third form))))
               (cl:push (list name (third form) (cdddr form) label) definitions)))
            ((string= head "DEFVAR")
             (unless (and (<= 2 (length form) 3) (or (null (cddr form)) (integerp (third form))))
               (%cc-fail form "expected (defvar NAME [INTEGER])"))
             (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "gv" name)))
               (when (or (gethash key *cc-globals*) (gethash key *cc-constants*))
                 (%cc-fail form "~A is defined twice" (%source-name name nil)))
               (claim name form label)
               (setf (gethash key *cc-globals*) label)
               (cl:push (list label (or (third form) 0)) globals)))
            ((string= head "DEFCONSTANT")
             (unless (and (= (length form) 3) (integerp (third form)))
               (%cc-fail form "expected (defconstant NAME INTEGER)"))
             (let ((key (%cc-key (second form) form)))
               (when (or (gethash key *cc-globals*) (gethash key *cc-constants*))
                 (%cc-fail form "~A is defined twice" (%source-name (second form) nil)))
               (setf (gethash key *cc-constants*) (third form))))
            (t (%cc-fail form "expected (defun ...), (defvar ...) or (defconstant ...)"))))))
    (values (nreverse definitions) (nreverse globals))))

(defun compile-program (forms &key backend positions source file)
  "The items that compile FORMS, a list of (defun ...), (defvar ...) and
(defconstant ...) forms, for BACKEND. A stub at the start stores the
globals' initial values, calls main and halts. Signals PROGRAM-COMPILE-ERROR.
POSITIONS, SOURCE and FILE, as READ-SOURCE and READ-SOURCE-FROM-STRING set
them on an ITEMS-PROGRAM, let errors report FILE:LINE:COLUMN (#362)."
  (unless backend
    (%cc-fail nil "compiling needs a backend"))
  (let ((*cc-backend* (find-backend backend))
        (*cc-functions* (make-hash-table :test 'equal))
        (*cc-globals* (make-hash-table :test 'equal))
        (*cc-constants* (make-hash-table :test 'equal))
        (*cc-function* nil) (*cc-form* nil) (*cc-labels* 0) (*cc-depth* 0) (*cc-out* '())
        (*cc-acc* nil) (*cc-temp* nil) (*cc-acc-name* nil) (*cc-temp-name* nil)
        (*cc-positions* positions) (*cc-source* source) (*cc-file* file))
    (%cc-registers)
    (multiple-value-bind (definitions globals) (%cc-collect forms)
      (let ((main (gethash "MAIN" *cc-functions*)))
        (unless (and main (zerop (cdr main)))
          (%cc-fail nil "the program needs (defun main () ...)"))
        (loop for (label value) in globals
              do (unless (zerop value)
                   (%cc-op :const *cc-acc* label)
                   (%cc-op :const *cc-temp* value)
                   (%cc-op :poke *cc-acc-name* *cc-temp-name*)))
        (%cc-emit (list :call (car main)))
        (%cc-op :halt)
        (append (nreverse *cc-out*)
                (mapcar #'%cc-function definitions)
                (loop for (label) in globals
                      append (list (list :label label)
                                   (list :directive (%cc-symbol "res") 1))))))))

;;; Source files

(defun %source-fail (control &rest args)
  (let ((detail (apply #'format nil control args)))
    (error 'program-compile-error :detail detail :message detail)))

(defun %parse-source (forms)
  "The ITEMS-PROGRAM whose items are FORMS, less a leading (:program (option...))."
  (let ((head (first forms)))
    (if (and (consp head) (%keyword-named-p (first head) "PROGRAM"))
        (let ((program (%parse-program head)))
          (setf (items-program-items program) (rest forms))
          program)
        (make-items-program :items forms))))

(defun %read-source-forms (stream path)
  "(VALUES FORMS POSITIONS) for STREAM, as READ-SOURCE and READ-SOURCE-FROM-
STRING read it. POSITIONS maps each form to a character offset into the text
STREAM reads from, for #362."
  (let ((positions (make-hash-table :test 'eq)))
    (values (read-restricted-forms stream #'%source-fail path :bare :uninterned :positions positions)
            positions)))

(defun read-source-from-string (string)
  "The ITEMS-PROGRAM whose items are the source forms in STRING. The text is
read without evaluation and without interning symbols."
  (with-input-from-string (in string)
    (multiple-value-bind (forms positions) (%read-source-forms in "source")
      (let ((program (%parse-source forms)))
        (setf (items-program-source program) string
              (items-program-positions program) positions)
        program))))

(defun read-source (path)
  "The ITEMS-PROGRAM whose items are the source forms in the file PATH, less an
optional leading (:program (option...)) as in a .lasm file."
  (let ((text (%slurp-file path)))
    (with-input-from-string (in text)
      (multiple-value-bind (forms positions) (%read-source-forms in path)
        (let ((program (%parse-source forms)))
          (setf (items-program-source program) text
                (items-program-file program) (namestring path)
                (items-program-positions program) positions)
          program)))))

(defun compile-source (program &key backend)
  "An ITEMS-PROGRAM of the items that compile the source PROGRAM, with its
options. BACKEND overrides the program's."
  (let ((backend (or backend (items-program-backend program))))
    (unless backend
      (%source-fail "no backend: name one in (:program (:backend NAME)) or pass one"))
    (let ((compiled (copy-items-program program)))
      (setf (items-program-items compiled)
            (compile-program (items-program-items program) :backend backend
                              :positions (items-program-positions program)
                              :source (items-program-source program)
                              :file (items-program-file program))
            (items-program-backend compiled) backend)
      compiled)))

(defun compile-source-file (path &key backend)
  "Compile the source file PATH to an ITEMS-PROGRAM."
  (compile-source (read-source path) :backend backend))

(defun assemble-source-file (path &key backend machine lexer origin memory)
  "Compile the source file PATH and assemble it as ASSEMBLE-ITEMS-FILE does."
  (%assemble-items-program (compile-source-file path :backend backend) path
                           :backend backend :machine machine :lexer lexer :origin origin :memory memory))

;;; Writing

(defun %printable (tree)
  "TREE with every symbol but a keyword replaced by an uninterned one of its name."
  (typecase tree
    (cons (mapcar #'%printable tree))
    (keyword tree)
    (null tree)
    (symbol (%cc-symbol (%source-name tree nil)))
    (t tree)))

(defun %write-item (item stream depth)
  (format stream "~&~vT" (* 2 depth))
  (if (and (consp item) (%keyword-named-p (first item) "FUNCTION"))
      (destructuring-bind (name options &rest body) (rest item)
        (format stream "(:function ~S ~S" name options)
        (dolist (element body) (%write-item element stream (1+ depth)))
        (write-string ")" stream))
      (prin1 item stream)))

(defun write-items-program (program stream)
  "Write the ITEMS-PROGRAM as a .lasm file READ-ITEMS reads back."
  (let ((*print-gensym* nil) (*print-case* :downcase) (*print-pretty* nil) (*print-readably* nil)
        (options (loop for (key value) on (list :backend (items-program-backend program)
                                                :machine (items-program-machine program)
                                                :origin (items-program-origin program)
                                                :memory (items-program-memory program)
                                                :lexer (items-program-lexer program))
                         by #'cddr
                       when value append (list key (if (or (symbolp value) (stringp value)) (%cc-symbol (%source-name value nil)) value)))))
    (format stream "(:program ~S" options)
    (dolist (item (%printable (items-program-items program)))
      (%write-item item stream 1))
    (format stream ")~%")))
