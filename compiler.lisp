;;;; compiler.lisp
;;;; #319: a small Lisp-like source language compiled to items through a
;;;; backend (backend.lisp, items.lisp). Values are plain machine words. Every
;;;; expression leaves its value in the backend's first :RETURN register, the
;;;; accumulator, and binary operators combine it with a second register
;;;; through the backend's operations (+BACKEND-LANGUAGE-OP-ARITIES+).
;;;;
;;;; Forms:  (defun NAME (PARAM...) BODY...)  (defvar NAME [INT])  (defconstant NAME INT)
;;;;   (defarray NAME SIZE)  (defarray NAME (VALUE...))  (defstring NAME "TEXT")
;;;;   (defmacro NAME (PARAM... [&rest R]) TEMPLATE)
;;;; Expressions: an integer or name, (set N E), (let ((V E)...) BODY...),
;;;;   (if C A [B]), (while C BODY...), (progn E...), (and E...), (or E...),
;;;;   (not E), an operator, (peek A), (poke A V), (peek-byte A), (poke-byte A V),
;;;;   (aref A I), (aset A I V), (return [E]), (asm ITEM...), (function F),
;;;;   (funcall E ARG...), (F ARG...)
;;;;
;;;; #365: (function F) is F's address, a value; (funcall E ARG...) calls
;;;; through any expression, going straight to F's label when E is literally
;;;; (function F). #366: (defarray ...) and (defstring ...) are initialised,
;;;; addressed data; the name is its address, never peeked through like a
;;;; global. (aref A I)/(aset A I V) index by cell; (peek-byte A)/(poke-byte A V)
;;;; are the backend's optional :peek-byte/:poke-byte, for machines narrower
;;;; than a cell.
;;;;
;;;; Symbols are compared by name: source is read without interning.
;;;;
;;;; #367: a defmacro call, in an expression or at top level, is replaced by
;;;; its TEMPLATE with each parameter substituted for the call's argument and
;;;; each &rest parameter spliced in as a list; nothing computes at compile
;;;; time. %CC-INSTANTIATE does the substitution; %CC-EXPAND-ALL expands a
;;;; function body, and %CC-COLLECT a top-level call, once every macro (from
;;;; anywhere in the file) is registered.

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
(defvar *cc-volatile* nil "Upcased names the register allocator (#373) may hold a value in across ordinary code, least-preferred last.")
(defvar *cc-preserved* nil "Upcased names the allocator may hold a value in across a call, saving it in the function's prologue.")
(defvar *cc-saves* nil "Upcased names from *CC-PRESERVED* the function being compiled has used, for its :save option.")
(defvar *cc-functions* nil "Upcased name -> (LABEL . ARITY).")
(defvar *cc-globals* nil "Upcased name -> label symbol.")
(defvar *cc-constants* nil "Upcased name -> integer.")
(defvar *cc-data* nil "Upcased name -> label symbol, for a DEFARRAY or DEFSTRING (#366).")
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
(defvar *cc-macros* nil "Upcased name -> (NAMES REST TEMPLATE): REST is a &rest parameter's key, or NIL (#367).")
(defvar *cc-expansions* 0 "Macro expansions performed so far in this program (#367).")
(defparameter +cc-expansion-limit+ 10000
  "Total macro expansions a program may perform; a budget, not a nesting-depth
limit, so it also catches a macro that expands into a call to itself (#367).")
(defvar *cc-rename-serial* 0 "Fresh names handed out so far, for a macro template's own let bindings (#367).")
(defvar *cc-expand-position* nil "The *CC-POSITIONS* offset a macro expansion's fresh conses are attributed to (#367).")

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

(defun %cc-proper-list-p (x)
  (and (listp x) (null (cdr (last x)))))

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

(defun %cc-register-operand (name)
  (list (%cc-symbol (string-downcase (getf (backend-descriptor-registers *cc-backend*) :operand)))
        (%cc-symbol (string-downcase name))))

;;; Expressions

(defun %cc-lookup (symbol)
  "The location of the variable SYMBOL: (:LOCAL i), (:ARG i), (:GLOBAL LABEL),
(:CONSTANT n) or (:ADDRESS LABEL), a DEFARRAY or DEFSTRING (#366)."
  (let ((key (%cc-key symbol *cc-form*)))
    (or (cdr (assoc key *cc-env* :test #'string=))
        (let ((label (gethash key *cc-globals*)))
          (and label (list :global label)))
        (let ((value (gethash key *cc-constants*)))
          (and value (list :constant value)))
        (let ((label (gethash key *cc-data*)))
          (and label (list :address label)))
        (%cc-fail *cc-form* "unknown variable ~A" (%source-name symbol nil)))))

(defun %cc-function-form-p (form)
  "T when FORM is (function NAME) (#365)."
  (and (consp form) (%cc-name-p (first form)) (equal (%designator-name (first form)) "FUNCTION")))

(defun %cc-function-label (form)
  "The label of the function (function NAME) names, checked to exist."
  (let* ((key (%cc-key (second form) form))
         (entry (gethash key *cc-functions*)))
    (unless entry
      (%cc-fail form "unknown function ~A" (%source-name (second form) nil)))
    (car entry)))

(defun %cc-function-expr (form)
  (%cc-check-length form 2 2)
  (%cc-const (%cc-function-label form)))

;; A leaf (an integer, a variable name, or (function NAME)) loads straight
;; into any register with :const/:get/:peek (#364), instead of always going
;; through the accumulator and the stack.
(defun %cc-leaf-p (form)
  (or (integerp form) (%cc-name-p form) (%cc-function-form-p form)))

(defun %cc-load-leaf (form register register-name)
  "Load leaf FORM into REGISTER; REGISTER-NAME names it, for :peek."
  (cond
    ((integerp form) (%cc-op :const register form))
    ((%cc-function-form-p form)
     (%cc-check-length form 2 2)
     (%cc-op :const register (%cc-function-label form)))
    (t (let ((location (%cc-lookup form)))
         (ecase (first location)
           ((:local :arg) (%cc-op :get register location))
           (:global (%cc-op :const register (second location))
            (%cc-op :peek register-name register-name))
           (:constant (%cc-op :const register (second location)))
           (:address (%cc-op :const register (second location))))))))

(defun %cc-variable (symbol)
  (%cc-load-leaf symbol *cc-acc* *cc-acc-name*))

(defun %cc-progn (forms)
  (if forms
      (dolist (form forms) (%cc-expr form))
      (%cc-const 0)))

(defun %cc-hazards (tree)
  "(VALUES ASM-P CALL-P): whether TREE, an operand's source form, reaches an
(asm ...) block, which can target any register directly, or calls a
function, which clobbers the volatile pool (#373) -- a call's target may
not preserve a :caller-saved register the way it preserves :callee-saved
ones."
  (if (not (consp tree))
      (values nil nil)
      (let* ((head (and (%cc-name-p (first tree)) (%designator-name (first tree))))
             (asm (equal head "ASM"))
             (call (and head (or (equal head "FUNCALL") (and (gethash head *cc-functions*) t)))))
        (dolist (element tree)
          (multiple-value-bind (a c) (%cc-hazards element)
            (when a (setf asm t))
            (when c (setf call t))))
        (values asm call))))

(defun %cc-take (form)
  "A register from the pool to hold a value across compiling FORM, an
operand not yet compiled, or NIL to fall back to the stack (#373): NIL when
FORM reaches an (asm ...), which could target the held register directly;
a free *CC-PRESERVED* register, recorded in *CC-SAVES* for the function's
prologue and epilogue to save and restore, when FORM calls a function; a
free *CC-VOLATILE* register otherwise, since a call is the only thing a
compiled operand can do that a :caller-saved register does not survive."
  (multiple-value-bind (asm call) (%cc-hazards form)
    (cond (asm nil)
          (call (let ((register (first *cc-preserved*)))
                  (when register (pushnew register *cc-saves* :test #'string=))
                  register))
          (t (first *cc-volatile*)))))

;; TODO: a register %CC-TAKE picks from *CC-PRESERVED* for a single call
;; site costs a prologue push and an epilogue pop, about the same as the
;; stack path it replaces. A use-count or loop-nesting heuristic that
;; prefers the stack for a one-off use would remove that cost (#376).
;;
;; TODO: an (asm ...) in the operand always falls back to the stack (%CC-
;; HAZARDS), because asm can target any register. A declared clobber list on
;; asm would let most inline asm keep the register path (#377).
(defun %cc-to-temp (form)
  "FORM's value into the temp register, leaving the accumulator as it is.
A non-leaf FORM holds the accumulator's current value in a register from
the pool (%CC-TAKE) while FORM computes, instead of the stack, when one is
free and safe; otherwise the accumulator is pushed and popped as before."
  (if (%cc-leaf-p form)
      (%cc-load-leaf form *cc-temp* *cc-temp-name*)
      (let ((register (%cc-take form)))
        (if register
            (let ((operand (%cc-register-operand register))
                  (*cc-volatile* (remove register *cc-volatile* :test #'string=))
                  (*cc-preserved* (remove register *cc-preserved* :test #'string=)))
              (%cc-op :move operand *cc-acc*)
              (%cc-expr form)
              (%cc-op :move *cc-temp* *cc-acc*)
              (%cc-op :move *cc-acc* operand))
            (progn (%cc-emit (list :push *cc-acc*))
                   (incf *cc-depth*)
                   (%cc-expr form)
                   (%cc-op :move *cc-temp* *cc-acc*)
                   (%cc-emit (list :pop *cc-acc*))
                   (decf *cc-depth*))))))

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
evaluated: an integer, a constant, an array/string address, or (function F)
always can; a local or argument can when RIGHT does not (set) it or reach it
through an (asm ...) block. A global never swaps -- a call or poke in RIGHT
could change it."
  (or (integerp left)
      (%cc-function-form-p left)
      (and (%cc-name-p left)
           (let ((location (%cc-lookup left)))
             (case (first location)
               ((:constant :address) t)
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
      (:constant (%cc-fail form "~A is a constant" (%source-name (second form) nil)))
      (:address (%cc-fail form "~A is an array or string" (%source-name (second form) nil))))))

(defun %cc-peek (form)
  (%cc-check-length form 2 2)
  (%cc-expr (second form))
  (%cc-op :peek *cc-acc-name* *cc-acc-name*))

(defun %cc-poke (form)
  (%cc-check-length form 3 3)
  (%cc-operands (second form) (third form))
  (%cc-op :poke *cc-acc-name* *cc-temp-name*)
  (%cc-op :move *cc-acc* *cc-temp*))

;; #366: (peek-byte A)/(poke-byte A V) mirror (peek A)/(poke A V) through the
;; backend's optional :peek-byte/:poke-byte, for a machine whose registers are
;; wider than its cells; a machine byte-addresses A as it defines those ops.
(defun %cc-peek-byte (form)
  (%cc-check-length form 2 2)
  (%cc-expr (second form))
  (%cc-op :peek-byte *cc-acc-name* *cc-acc-name*))

(defun %cc-poke-byte (form)
  (%cc-check-length form 3 3)
  (%cc-operands (second form) (third form))
  (%cc-op :poke-byte *cc-acc-name* *cc-temp-name*)
  (%cc-op :move *cc-acc* *cc-temp*))

;; #366: (aref A I)/(aset A I V) index by cell, sugar for (peek (+ A I)) and
;; (poke (+ A I) V) -- %CC-PEEK/%CC-POKE already give the address expression
;; the same leaf and operand-ordering treatment as any other (#364).
(defun %cc-aref (form)
  (%cc-check-length form 3 3)
  (%cc-peek (list 'peek (list '+ (second form) (third form)))))

(defun %cc-aset (form)
  (%cc-check-length form 4 4)
  (%cc-poke (list 'poke (list '+ (second form) (third form)) (fourth form))))

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
             ((:global :address) (second location))
             (:constant (second location)))))
        ((and (consp tree) (null (cdr (last tree))))
         (mapcar (lambda (element) (%cc-substitute-variables element form)) tree))
        (t tree)))

(defun %cc-asm (form)
  (%cc-check-length form 1 nil)
  (dolist (item (rest form))
    (%cc-emit (%cc-substitute-variables item form))))

(defun %cc-args-to-slots (args)
  "Each of ARGS compiled into a fresh frame slot, in order, returned least
recently allocated first; a call's arguments each need their own slot, since
computing a later one may otherwise clobber an earlier one held in the
accumulator or a register (docs/language.md#backend-requirements)."
  (let ((slots '()))
    (dolist (arg args)
      (%cc-expr arg)
      (let ((slot (%cc-alloc)))
        (%cc-op :set slot *cc-acc*)
        (cl:push slot slots)))
    (nreverse slots)))

(defun %cc-free-slots (slots)
  (dotimes (i (length slots)) (%cc-free)))

(defun %cc-call (form)
  (let* ((entry (gethash (%cc-key (first form) form) *cc-functions*))
         (args (rest form)))
    (unless entry
      (%cc-fail form "unknown function ~A" (%source-name (first form) nil)))
    (%cc-check-length form 1 nil)
    (unless (= (length args) (cdr entry))
      (%cc-fail form "~A takes ~D argument~:P, got ~D"
                (%source-name (first form) nil) (cdr entry) (length args)))
    (let ((slots (%cc-args-to-slots args)))
      (%cc-emit (list* :call (car entry) slots))
      (%cc-free-slots slots))))

;; #365: (function F) is a value, F's label; (funcall E ARG...) calls through
;; any expression. A literal (function F) target compiles the same direct
;; (:call LABEL ...) a plain (F ARG...) call does, arity-checked; any other E
;; is held in a frame slot across compiling ARG..., as each of those is
;; (%CC-ARGS-TO-SLOTS), then loaded into a register the call goes through.
(defun %cc-funcall (form)
  (%cc-check-length form 2 nil)
  (let ((callee (second form)) (args (cddr form)))
    (if (%cc-function-form-p callee)
        (%cc-direct-funcall form callee args)
        (%cc-indirect-funcall callee args))))

(defun %cc-direct-funcall (form callee args)
  (%cc-check-length callee 2 2)
  (let* ((key (%cc-key (second callee) callee))
         (entry (gethash key *cc-functions*)))
    (unless entry
      (%cc-fail callee "unknown function ~A" (%source-name (second callee) nil)))
    (unless (= (length args) (cdr entry))
      (%cc-fail form "~A takes ~D argument~:P, got ~D"
                (%source-name (second callee) nil) (cdr entry) (length args)))
    (let ((slots (%cc-args-to-slots args)))
      (%cc-emit (list* :call (car entry) slots))
      (%cc-free-slots slots))))

(defun %cc-call-arg-registers ()
  (let ((args (getf (backend-descriptor-call *cc-backend*) :args)))
    (if (eq args :stack) '() args)))

(defun %cc-call-target-register ()
  "An upcased register name to hold FUNCALL's computed target: the
accumulator or temp register when neither is a call argument register, since
the callee's value already ends up in the accumulator; otherwise the first of
the volatile pool that is not one. With none free, the accumulator anyway --
a target in an argument register is copied to a free :scratch register, or is
items-malformed, as any call target is (#335, docs/conventions.md)."
  (let ((args (%cc-call-arg-registers))
        (candidates (list* (symbol-name *cc-acc-name*) (symbol-name *cc-temp-name*) *cc-volatile*)))
    (or (find-if (lambda (name) (not (member name args :test #'string=))) candidates)
        (symbol-name *cc-acc-name*))))

(defun %cc-indirect-funcall (callee args)
  (let ((callee-slot (unless (%cc-leaf-p callee)
                        (%cc-expr callee)
                        (let ((slot (%cc-alloc)))
                          (%cc-op :set slot *cc-acc*)
                          slot))))
    (let* ((slots (%cc-args-to-slots args))
           (target (%cc-call-target-register))
           (target-operand (%cc-register-operand target))
           (target-name (%cc-symbol (string-downcase target))))
      (if callee-slot
          (%cc-op :get target-operand callee-slot)
          (%cc-load-leaf callee target-operand target-name))
      (%cc-emit (list* :call target-operand slots))
      (%cc-free-slots slots))
    (when callee-slot (%cc-free))))

(defparameter *cc-forms*
  '(("PROGN" . %cc-progn-form) ("IF" . %cc-if) ("WHILE" . %cc-while) ("AND" . %cc-and) ("OR" . %cc-or)
    ("NOT" . %cc-not) ("LET" . %cc-let) ("SET" . %cc-set) ("PEEK" . %cc-peek) ("POKE" . %cc-poke)
    ("PEEK-BYTE" . %cc-peek-byte) ("POKE-BYTE" . %cc-poke-byte) ("AREF" . %cc-aref) ("ASET" . %cc-aset)
    ("ASM" . %cc-asm) ("RETURN" . %cc-return) ("FUNCTION" . %cc-function-expr) ("FUNCALL" . %cc-funcall)))

;;; Macros (#367)

(defun %cc-parse-macro-params (params form)
  "(VALUES NAMES REST) for a defmacro PARAM list: NAMES are the fixed
parameters' upcased keys, in order; REST is a trailing &rest parameter's
key, or NIL."
  (unless (%cc-proper-list-p params)
    (%cc-fail form "defmacro parameter list is malformed"))
  (let ((names '()) (rest nil) (seen (make-hash-table :test 'equal)))
    (labels ((claim (param)
               (let ((key (%cc-key param form)))
                 (when (gethash key seen)
                   (%cc-fail form "~A is a parameter twice" (%source-name param nil)))
                 (setf (gethash key seen) t)
                 key)))
      (loop for tail on params
            do (if (and (%cc-name-p (first tail)) (string= (%designator-name (first tail)) "&REST"))
                   (progn
                     (unless (and (consp (rest tail)) (null (cddr tail)))
                       (%cc-fail form "&rest needs exactly one parameter name, at the end"))
                     (setf rest (claim (second tail)))
                     (return))
                   (cl:push (claim (first tail)) names))))
    (values (nreverse names) rest)))

(defun %cc-parse-defmacro (form)
  "Register FORM, a (defmacro NAME (PARAM... [&rest R]) TEMPLATE), in *CC-MACROS*."
  (unless (and (%cc-proper-list-p form) (= (length form) 4))
    (%cc-fail form "expected (defmacro NAME (PARAM...) TEMPLATE)"))
  (let* ((name (second form)) (key (%cc-key name form)))
    (when (or (assoc key *cc-forms* :test #'string=) (assoc key *cc-operators* :test #'string=)
              (member key '("DEFUN" "DEFVAR" "DEFCONSTANT" "DEFMACRO") :test #'string=))
      (%cc-fail form "~A is a built-in form" (%source-name name nil)))
    (when (or (gethash key *cc-functions*) (gethash key *cc-macros*)
              (gethash key *cc-globals*) (gethash key *cc-constants*) (gethash key *cc-data*))
      (%cc-fail form "~A is defined twice" (%source-name name nil)))
    (multiple-value-bind (names rest) (%cc-parse-macro-params (third form) form)
      (setf (gethash key *cc-macros*) (list names rest (fourth form))))))

(defun %cc-fresh-name (template-name)
  "A fresh name standing for a macro TEMPLATE-NAME's own `let` binding: its
printed name has a space, which no source symbol can spell, so it can never
collide with a caller's variable of the same name (#367)."
  (%cc-symbol (format nil "~A ~D" (string-downcase (%source-name template-name nil)) (incf *cc-rename-serial*))))

(defun %cc-form-position (form)
  (and *cc-positions* (gethash form *cc-positions*)))

(defun %cc-remember-position (node)
  "NODE, a fresh cons a macro expansion built, attributed to the enclosing
call's position (*CC-EXPAND-POSITION*), so an error inside it reports the
call's line and column, not the template's own (#367, #362)."
  (when (and *cc-positions* *cc-expand-position* (consp node))
    (setf (gethash node *cc-positions*) *cc-expand-position*))
  node)

(defun %cc-rest-splice (element bindings renames)
  "The forms ELEMENT, a template list element, splices in as a &rest
parameter, or NIL when it is not one."
  (and (%cc-name-p element)
       (not (assoc (%designator-name element) renames :test #'string=))
       (let ((binding (assoc (%designator-name element) bindings :test #'string=)))
         (and binding (eq (second binding) :rest) (third binding)))))

(defun %cc-instantiate (node bindings renames form)
  "TEMPLATE node NODE with each macro parameter in BINDINGS (KEY :FIXED FORM)
or (KEY :REST FORMS) substituted, and each RENAMES (KEY . FRESH) applied. A
substituted argument keeps its own position; only a cons this rebuilds is
attributed to the call (%CC-REMEMBER-POSITION)."
  (cond
    ((%cc-name-p node)
     (let ((rename (assoc (%designator-name node) renames :test #'string=)))
       (if rename
           (cdr rename)
           (let ((binding (assoc (%designator-name node) bindings :test #'string=)))
             (cond ((null binding) node)
                   ((eq (second binding) :rest)
                    (%cc-fail form "~A (a &rest parameter) is used outside a list" (%source-name node nil)))
                   (t (third binding)))))))
    ((not (consp node)) node)
    ((and (%cc-name-p (first node)) (member (%designator-name (first node)) '("LET" "LET*") :test #'string=)
          (consp (rest node)) (listp (second node)))
     (%cc-instantiate-let node bindings renames form))
    ((and (%cc-name-p (first node)) (equal (%designator-name (first node)) "ASM"))
     (%cc-remember-position
      (list* (first node) (mapcar (lambda (item) (%cc-instantiate-asm-item item bindings renames form)) (rest node)))))
    (t (%cc-instantiate-list node bindings renames form))))

(defun %cc-instantiate-list (node bindings renames form)
  "NODE, a proper template list, instantiated element by element; a &rest
parameter occupying an element position splices its forms in."
  (unless (%cc-proper-list-p node)
    (%cc-fail form "macro template is malformed"))
  (%cc-remember-position
   (loop for element in node
         for forms = (%cc-rest-splice element bindings renames)
         append (if forms (copy-list forms) (list (%cc-instantiate element bindings renames form))))))

(defun %cc-instantiate-let (node bindings renames form)
  "A template `let`/`let*`'s own binding names are fresh (%CC-FRESH-NAME)
unless the name is itself a macro parameter, in which case it keeps the
caller's name; LET*'s later bindings and the body see each rename in turn."
  (unless (%cc-proper-list-p (second node))
    (%cc-fail form "let needs a list of (NAME VALUE) bindings"))
  (let* ((sequential (equal (%designator-name (first node)) "LET*"))
         (inner renames) (new-bindings '()))
    (dolist (binding (second node))
      (unless (and (consp binding) (= (length binding) 2) (%cc-name-p (first binding)))
        (%cc-fail form "let binding ~S is not (NAME VALUE)" binding))
      (let* ((key (%designator-name (first binding)))
             (value (%cc-instantiate (second binding) bindings (if sequential inner renames) form))
             (fixed (assoc key bindings :test #'string=)))
        (cond ((and fixed (eq (second fixed) :rest))
               (%cc-fail form "~A (a &rest parameter) is used outside a list" (%source-name (first binding) nil)))
              (fixed (cl:push (list (third fixed) value) new-bindings))
              (t (let ((fresh (%cc-fresh-name (first binding))))
                   (cl:push (list fresh value) new-bindings)
                   (setf inner (acons key fresh inner)))))))
    (%cc-remember-position
     (list* (first node) (%cc-remember-position (nreverse new-bindings))
            (mapcar (lambda (each) (%cc-instantiate each bindings inner form)) (cddr node))))))

(defun %cc-instantiate-asm-item (item bindings renames form)
  "An (asm ...) ITEM from a macro template, with each (:var PARAM) replaced by
(:var ARGUMENT-NAME) or (:var FRESH-NAME); other items, including register
operands such as (reg a), pass through untouched."
  (cond ((and (consp item) (%keyword-named-p (first item) "VAR") (consp (rest item)) (%cc-name-p (second item)))
         (let* ((key (%designator-name (second item)))
                (rename (assoc key renames :test #'string=))
                (binding (assoc key bindings :test #'string=)))
           (cond (rename (list :var (cdr rename)))
                 ((null binding) item)
                 ((eq (second binding) :rest)
                  (%cc-fail form "~A (a &rest parameter) is used outside a list" (%source-name (second item) nil)))
                 ((not (%cc-name-p (third binding)))
                  (%cc-fail form "(:var ~A) needs a name, got ~S" (%source-name (second item) nil) (third binding)))
                 (t (list :var (third binding))))))
        ((%cc-proper-list-p item)
         (mapcar (lambda (element) (%cc-instantiate-asm-item element bindings renames form)) item))
        (t item)))

(defun %cc-expand-call (name entry form)
  "FORM, a call (NAME ARG...) matching macro ENTRY = (NAMES REST TEMPLATE),
expanded once into a fresh copy of TEMPLATE."
  (destructuring-bind (names rest template) entry
    (unless (%cc-proper-list-p form)
      (%cc-fail form "expected (~A ARG...)" (%source-name name nil)))
    (let* ((args (rest form)) (fixed (length names)))
      (unless (and (>= (length args) fixed) (or rest (= (length args) fixed)))
        (%cc-fail form "~A takes ~:[exactly~;at least~] ~D argument~:P, got ~D"
                  (%source-name name nil) rest fixed (length args)))
      (when (> (incf *cc-expansions*) +cc-expansion-limit+)
        (%cc-fail form "macro ~A expands too many times (over ~D expansions)"
                  (%source-name name nil) +cc-expansion-limit+))
      (let ((bindings (append (loop for key in names for arg in args collect (list key :fixed arg))
                               (and rest (list (list rest :rest (nthcdr fixed args)))))))
        (%cc-instantiate template bindings nil form)))))

(defun %cc-expand-macro-form (name entry form)
  "FORM expanded (%CC-EXPAND-CALL) with *CC-EXPAND-POSITION* set to its own
position, or the enclosing expansion's when it has none of its own."
  (let ((*cc-expand-position* (or (%cc-form-position form) *cc-expand-position*)))
    (%cc-expand-call name entry form)))

(defun %cc-rebuild-if-changed (original elements)
  "ORIGINAL, a list, given its own ELEMENTS (mapped from it): ORIGINAL itself
when every element is EQ to its own, so an unexpanded form keeps its #362
position; otherwise a fresh list, carrying ORIGINAL's own position, since a
form with a macro call somewhere inside still needs one for the rest of it."
  (if (every #'eq original elements)
      original
      (let ((position (%cc-form-position original)))
        (when (and *cc-positions* position)
          (setf (gethash elements *cc-positions*) position))
        elements)))

(defun %cc-expand-let-elements (form)
  "FORM's (LET/LET* ((NAME VALUE)...) BODY...) elements, a binding's VALUE and
each BODY form macro-expanded; binding names are left alone."
  (list* (first form)
         (if (%cc-proper-list-p (second form))
             (let ((bindings (mapcar (lambda (binding)
                                        (if (and (consp binding) (consp (rest binding)))
                                            (let ((value (%cc-expand-all (second binding))))
                                              (if (eq value (second binding)) binding (list (first binding) value)))
                                            binding))
                                      (second form))))
               (if (every #'eq bindings (second form)) (second form) bindings))
             (second form))
         (mapcar #'%cc-expand-all (cddr form))))

(defun %cc-expand-set-elements (form)
  "FORM's (SET NAME VALUE) elements with only VALUE macro-expanded, so its
place name is left alone."
  (if (and (consp (rest form)) (consp (cddr form)) (null (cdddr form)))
      (let ((value (%cc-expand-all (third form))))
        (if (eq value (third form)) form (list (first form) (second form) value)))
      form))

(defun %cc-expand-all (form)
  "FORM with every macro call macro-expanded, keeping the original cons
wherever nothing inside changed, so #362 positions survive. A `let`'s
binding names, a `set`'s place, and an `asm` block are left alone: those
names must stay literal for the rest of the compiler to resolve."
  (if (not (and (consp form) (%cc-name-p (first form))))
      form
      (let* ((key (%designator-name (first form)))
             (entry (gethash key *cc-macros*)))
        (if entry
            (%cc-expand-all (%cc-expand-macro-form (first form) entry form))
            (%cc-rebuild-if-changed
             form
             (cond ((member key '("LET" "LET*") :test #'string=) (%cc-expand-let-elements form))
                   ((string= key "SET") (%cc-expand-set-elements form))
                   ((string= key "ASM") form)
                   ((%cc-proper-list-p form) (mapcar #'%cc-expand-all form))
                   (t form)))))))

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
           ;; Every macro in the file is registered by now (%CC-COLLECT ran
           ;; first), regardless of where NAME's DEFUN sits relative to them (#367).
           (body (mapcar #'%cc-expand-all body))
           (*cc-out* '()) (*cc-env* '()) (*cc-next* 0) (*cc-max* 0) (*cc-depth* 0) (*cc-saves* '())
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
      (list* :function label
             (append (list :args (length params) :locals *cc-max*)
                     ;; A preserved register %CC-TAKE used (#373); the backend's
                     ;; own :callee-saved convention pushes and pops it, which
                     ;; also restores it correctly across an early (return).
                     (and *cc-saves* (list :save (mapcar (lambda (name) (%cc-symbol (string-downcase name)))
                                                          (reverse *cc-saves*)))))
             (nreverse (cl:push (list :return) *cc-out*))))))

(defun %cc-registers ()
  "Set the accumulator, the temporary register, and the volatile and
preserved pools the register allocator (#373) holds operands in, all from
the backend's register roles. An operation writes only its destination
register, so any of these are free once nothing above still needs them."
  (let* ((registers (backend-descriptor-registers *cc-backend*))
         (acc (first (getf registers :return)))
         (pointer (getf (backend-descriptor-frame *cc-backend*) :pointer))
         (temp (and acc
                    (find-if (lambda (name) (and (string/= name acc) (not (equal name pointer))))
                             (append (getf registers :scratch) (getf registers :caller-saved)))))
         (reserved (list* acc temp pointer (getf registers :return))))
    (unless (getf registers :operand)
      (%cc-fail nil "the backend needs (registers :operand KIND)"))
    (unless acc
      (%cc-fail nil "the backend needs (registers :return (REGISTER))"))
    (unless temp
      (%cc-fail nil "the backend needs a :scratch or :caller-saved register besides ~(~A~)" acc))
    (setf *cc-acc* (%cc-register-operand acc)
          *cc-temp* (%cc-register-operand temp)
          *cc-acc-name* (%cc-symbol (string-downcase acc))
          *cc-temp-name* (%cc-symbol (string-downcase temp))
          *cc-volatile* (remove-if (lambda (name) (member name reserved :test #'string=))
                                    (append (getf registers :scratch) (getf registers :caller-saved)))
          *cc-preserved* (remove-if (lambda (name) (member name reserved :test #'string=))
                                     (getf registers :callee-saved)))))

(defun %cc-array-value (value form)
  "The integer VALUE, an element of a DEFARRAY's (VALUE...), resolves to: an
integer as is, (function F)'s label, or a DEFCONSTANT's, DEFARRAY's or
DEFSTRING's own name."
  (cond
    ((integerp value) value)
    ((%cc-function-form-p value) (%cc-function-label value))
    ((%cc-name-p value)
     (let ((key (%cc-key value form)))
       (or (gethash key *cc-constants*)
           (gethash key *cc-data*)
           (%cc-fail form "~A is not a constant, a function or an array/string"
                     (%source-name value nil)))))
    (t (%cc-fail form "~S is not a constant, a function or an array/string" value))))

(defun %cc-collect (forms)
  "(VALUES DEFINITIONS GLOBALS DATA), registering functions, globals,
constants, DEFARRAY/DEFSTRING data (#366) and macros (#367), in file order.
A top-level macro call expands in place, and a (progn DEF...) it (or the
source) produces flattens. A DEFUN's own body is expanded later, in
%CC-FUNCTION, once every macro here is registered. DATA's array/string
values are resolved only once every form is registered, so one may name a
function, macro or array/string defined later in FORMS."
  (let ((definitions '()) (globals '()) (arrays '()) (seen (make-hash-table :test 'equal)))
    (labels
        ((claim (name form label)
           (let ((existing (gethash (symbol-name label) seen)))
             (when (and existing (string/= existing (%designator-name name)))
               (%cc-fail form "~A and ~A both make the label ~A"
                         (%source-name name nil) existing (symbol-name label)))
             (setf (gethash (symbol-name label) seen) (%designator-name name))))
         (defined-p (key) (or (gethash key *cc-globals*) (gethash key *cc-constants*)
                               (gethash key *cc-data*) (gethash key *cc-macros*)))
         (process (form)
           (unless (and (consp form) (%cc-name-p (first form)) (%cc-proper-list-p form))
             (%cc-fail form "expected (defun ...), (defvar ...), (defconstant ...), (defarray ...), (defstring ...) or (defmacro ...)"))
           (let* ((head (%designator-name (first form)))
                  (macro (gethash head *cc-macros*)))
             (cond
               ((string= head "PROGN") (dolist (sub (rest form)) (process sub)))
               (macro (process (%cc-expand-macro-form (first form) macro form)))
               ((string= head "DEFMACRO") (%cc-parse-defmacro form))
               ((string= head "DEFUN")
                (unless (and (>= (length form) 3) (listp (third form)) (null (cdr (last (third form)))))
                  (%cc-fail form "expected (defun NAME (PARAM...) BODY...)"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "fn" name)))
                  (when (or (assoc key *cc-forms* :test #'string=) (assoc key *cc-operators* :test #'string=))
                    (%cc-fail form "~A is a built-in form" (%source-name name nil)))
                  (when (or (gethash key *cc-functions*) (gethash key *cc-macros*))
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-functions*) (cons label (length (third form))))
                  (cl:push (list name (third form) (cdddr form) label) definitions)))
               ((string= head "DEFVAR")
                (unless (and (<= 2 (length form) 3) (or (null (cddr form)) (integerp (third form))))
                  (%cc-fail form "expected (defvar NAME [INTEGER])"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "gv" name)))
                  (when (defined-p key)
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-globals*) label)
                  (cl:push (list label (or (third form) 0)) globals)))
               ((string= head "DEFCONSTANT")
                (unless (and (= (length form) 3) (integerp (third form)))
                  (%cc-fail form "expected (defconstant NAME INTEGER)"))
                (let ((key (%cc-key (second form) form)))
                  (when (defined-p key)
                    (%cc-fail form "~A is defined twice" (%source-name (second form) nil)))
                  (setf (gethash key *cc-constants*) (third form))))
               ((string= head "DEFARRAY")
                (unless (= (length form) 3)
                  (%cc-fail form "expected (defarray NAME size) or (defarray NAME (value...))"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "ar" name))
                       (spec (third form)))
                  (when (defined-p key)
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-data*) label)
                  (cond
                    ((and (integerp spec) (plusp spec)) (cl:push (list label :size spec form) arrays))
                    ((and (listp spec) (null (cdr (last spec)))) (cl:push (list label :values spec form) arrays))
                    (t (%cc-fail form "expected (defarray NAME size) or (defarray NAME (value...))")))))
               ((string= head "DEFSTRING")
                (unless (and (= (length form) 3) (stringp (third form)))
                  (%cc-fail form "expected (defstring NAME \"text\")"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "st" name)))
                  (when (defined-p key)
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-data*) label)
                  (cl:push (list label :string (third form) form) arrays)))
               (t (%cc-fail form "expected (defun ...), (defvar ...), (defconstant ...), (defarray ...), (defstring ...) or (defmacro ...)"))))))
      (dolist (form forms) (process form)))
    (values (nreverse definitions) (nreverse globals)
            (loop for (label kind payload form) in (nreverse arrays)
                  append (list (list :label label)
                               (ecase kind
                                 (:size (list :directive (%cc-symbol "res") payload))
                                 (:values (list* :directive (%cc-symbol "cell")
                                                 (mapcar (lambda (value) (%cc-array-value value form)) payload)))
                                 (:string (list :directive (%cc-symbol "asciz") payload))))))))

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
        (*cc-data* (make-hash-table :test 'equal))
        (*cc-function* nil) (*cc-form* nil) (*cc-labels* 0) (*cc-depth* 0) (*cc-out* '())
        (*cc-acc* nil) (*cc-temp* nil) (*cc-acc-name* nil) (*cc-temp-name* nil)
        (*cc-volatile* nil) (*cc-preserved* nil) (*cc-saves* nil)
        (*cc-positions* positions) (*cc-source* source) (*cc-file* file)
        (*cc-macros* (make-hash-table :test 'equal)) (*cc-expansions* 0)
        (*cc-rename-serial* 0) (*cc-expand-position* nil))
    (%cc-registers)
    (multiple-value-bind (definitions globals data) (%cc-collect forms)
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
                                   (list :directive (%cc-symbol "res") 1)))
                data)))))

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
