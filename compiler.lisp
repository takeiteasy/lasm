;;;; compiler.lisp
;;;; #319: a small Lisp-like source language compiled to items through a
;;;; backend (backend.lisp, items.lisp). Values are plain machine words. Every
;;;; expression leaves its value in the backend's first :RETURN register, the
;;;; accumulator, and binary operators combine it with a second register
;;;; through the backend's operations (+BACKEND-LANGUAGE-OP-ARITIES+).
;;;;
;;;; Forms:  (defun NAME (PARAM...) BODY...)  (defvar NAME [INT])  (defconstant NAME INT)
;;;;   (defarray NAME SIZE)  (defarray NAME (VALUE...))  (defstring NAME "TEXT" [:packed])
;;;;   (defmacro NAME (PARAM... [&rest R]) BODY...)  (defun-for-syntax NAME (PARAM... [&rest R]) BODY...)
;;;; Expressions: an integer or name, (set N E), (let ((V E)...) BODY...),
;;;;   (if C A [B]), (while C BODY...), (progn E...), (and E...), (or E...),
;;;;   (not E), an operator, (peek A), (poke A V), (peek-byte A), (poke-byte A V),
;;;;   (aref A I), (aset A I V), (aref-byte S I), (aset-byte S I V), (return [E]), (asm ITEM...), (function F),
;;;;   (funcall E ARG...), (F ARG...)
;;;;
;;;; #365: (function F) is F's address, a value; (funcall E ARG...) calls
;;;; through any expression, going straight to F's label when E is literally
;;;; (function F). #366: (defarray ...) and (defstring ...) are initialised,
;;;; addressed data; the name is its address, never peeked through like a
;;;; global. (peek-byte A)/(poke-byte A V) are the backend's optional
;;;; :peek-byte/:poke-byte, byte-addressing within a word, for a machine whose
;;;; registers are wider than its cells.
;;;;
;;;; #379: (defstring NAME "TEXT" :packed) holds as many 8-bit characters a cell
;;;; as fit (a .packz directive, #398), byte order from the memory's endianness; (aref-byte S I)/(aset-byte
;;;; S I V) reach one through :peek-byte/:poke-byte.
;;;;
;;;; #368: a word is *CC-WORD-CELLS* cells (BACKEND-WORD-CELLS, backend.lisp),
;;;; the split #167's (stack-pointer ... :width n) already gives a stack slot.
;;;; A DEFVAR is *CC-WORD-CELLS* cells (.res); DEFARRAY indexes and sizes by
;;;; it, and (aref A I)/(aset A I V) step a word, not a cell.
;;;;
;;;; #374: a binary operator's right operand that is a leaf goes straight into
;;;; the backend's optional :OP-imm (a constant) or :OP-slot (a frame slot)
;;;; operation when it defines one, instead of loading into the temp register.
;;;;
;;;; #388: a leaf left operand swaps to the right when that lets a variant
;;;; apply and the right operand has none: a commutative operator keeps its
;;;; name, a comparison flips its direction.
;;;;
;;;; #375: an if/while condition that is a comparison, or an and/or/not of
;;;; them, jumps on the backend's optional :BRANCH-cmp (a b target) operation,
;;;; and its -imm/-slot variants, instead of computing a 0 or 1 first.
;;;;
;;;; #389: a jump on a true value with no comparison of its own uses the
;;;; backend's optional :BRANCH-NE-IMM (a 0 target) rather than skipping a :jump.
;;;;
;;;; #390: an and/or whose value is used jumps on its comparisons into a
;;;; shared 0/1 landing when the operands save more than the landing costs.
;;;; #391: so does a nested not/and/or operand, by its estimated saving
;;;; (%CC-COSTS). #392: that estimate counts the :cmp or :branch-cmp variant
;;;; each side would use (%CC-PAIR-COST).
;;;;
;;;; #376: an operand holds a :callee-saved register across a call only inside a
;;;; loop, or when the function already saves that register; otherwise the
;;;; stack costs less than the prologue/epilogue pair. #394: with :optimize
;;;; :speed, a first compile counts the call sites per register and a register
;;;; claimed by more than one run is held outside loops too. #400: an if arm
;;;; and each and/or operand after the first halve a site; a while multiplies it by 4 (#401).
;;;; #377: (asm (:clobbers REG...) ITEM...) declares the registers the asm
;;;; writes, so an operand holding another register may reach it. #393: a
;;;; declared :callee-saved one is added to the function's :save.
;;;;
;;;; Symbols are compared by name: source is read without interning.
;;;;
;;;; #367, #380: a defmacro call, in an expression or at top level, is
;;;; replaced by its BODY evaluated at compile time (%CC-META-EVAL), with each
;;;; parameter bound to the call's argument form, unevaluated, and &rest to
;;;; the remaining argument forms as a list; a QUASIQUOTE/UNQUOTE/UNQUOTE-
;;;; SPLICING template (%CC-QQ) builds the returned form, most often. A
;;;; DEFUN-FOR-SYNTAX helper's own arguments, by contrast, are evaluated
;;;; before the call, like an ordinary function. %CC-EXPAND-ALL expands a
;;;; function body, and %CC-COLLECT a top-level call, once every macro and
;;;; helper (from anywhere in the file) is registered.
;;;; #382: each expansion marks the names its templates write, so its own
;;;; variables and a caller's never capture one another; a marked free name
;;;; must be a global. #395: so does a name QUOTE or INTERN makes, and
;;;; (unmark FORM) is the way to the caller's variable.
;;;; #384: the evaluator adds and/or/cond/when/unless, lambda and function
;;;; values (funcall/apply/mapcar), and list, string, symbol and integer
;;;; operators.

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

(defparameter *cc-source-keywords* '(:var :clobbers)
  "Keywords that source text uses, which the reader accepts only once they exist.")

(defvar *cc-backend* nil)
(defvar *cc-acc* nil "The accumulator register operand.")
(defvar *cc-temp* nil "The register operand that holds a right operand.")
(defvar *cc-acc-name* nil "The accumulator's name, for the operations that take register names.")
(defvar *cc-temp-name* nil "The temporary register's name.")
(defvar *cc-volatile* nil "Upcased names the register allocator (#373) may hold a value in across ordinary code, least-preferred last.")
(defvar *cc-preserved* nil "Upcased names the allocator may hold a value in across a call, saving it in the function's prologue.")
(defvar *cc-saves* nil "Upcased names from *CC-PRESERVED* the function being compiled has used, for its :save option.")
(defvar *cc-optimize* :size "The compile-program :optimize option: :SIZE, or :SPEED to share a preserved register between call sites (#394).")
(defvar *cc-shared* nil "Upcased names from *CC-PRESERVED* that the function being compiled's call sites claim more than once a call (#400), so a site outside a loop may claim them too (#394).")
(defvar *cc-counting* nil "NIL, or during the counting pass an EQUAL hash table, upcased register name -> the call sites that would claim it (#394).")
(defvar *cc-site-weight* 1 "How often, relative to the function's entry, the code being compiled runs: halved by each if arm and each and/or operand after the first (#400), and multiplied by 4 inside each while (#401).")
(defvar *cc-functions* nil "Upcased name -> (LABEL . ARITY).")
(defvar *cc-globals* nil "Upcased name -> label symbol.")
(defvar *cc-constants* nil "Upcased name -> integer.")
(defvar *cc-data* nil "Upcased name -> label symbol, for a DEFARRAY or DEFSTRING (#366).")
(defvar *cc-word-cells* 1 "Cells a word spans on the target backend (BACKEND-WORD-CELLS, #368).")
(defvar *cc-value-arities* nil "Argument counts of every function a (function F) has made a value (#378).")
(defvar *cc-indirect-calls* nil "(ARITY FORM FUNCTION SOURCES NAME), reversed, for each funcall through a computed target, checked once every function is compiled (#378, #397, #402).")
(defvar *cc-holdings* nil "An EQ table from a binding (a local's environment cell, a global's label, a parameter or an array element) to the sources of what is put in it, or :UNKNOWN (#397, #402).")
(defvar *cc-let-sources* nil "An EQ table from a LET form to the sources of its value (#402).")
(defvar *cc-parameters* nil "Upcased function name -> the bindings of its parameters, that callers put their arguments in (#402).")
(defvar *cc-label-keys* nil "Upcased function label -> upcased function name, for an (asm) that spells a label (#402).")
(defvar *cc-taken* nil "Upcased names of the functions a call cannot account for every caller of: made a value, or spelled in an (asm) (#402).")
(defvar *cc-arrays* nil "An EQ table from a DEFARRAY's label to the vector of its elements' bindings (#402).")
(defvar *cc-escaped* nil "Upcased names of the DEFARRAYs used other than as the base of an AREF or ASET, so any element may change (#402).")
(defvar *cc-function* nil "The source name of the function being compiled.")
(defvar *cc-form* nil "The innermost expression being compiled.")
(defvar *cc-out* nil "The items of the current function or stub, reversed.")
(defvar *cc-env* nil "(KEY . LOCATION) for each parameter and let variable in scope.")
(defvar *cc-next* 0 "The next free local slot.")
(defvar *cc-max* 0 "Local slots the function needs.")
(defvar *cc-labels* 0 "Control labels made so far.")
(defvar *cc-loop-depth* 0 "While loops enclosing the expression being compiled (#376).")
(defvar *cc-depth* 0 "Temporaries the compiler has pushed since the function's entry.")
(defvar *cc-positions* nil "EQ hash table, form -> character offset, or NIL without one (#362).")
(defvar *cc-source* nil "The program's source text, or NIL.")
(defvar *cc-file* nil "The program's path, or NIL.")
(defvar *cc-macros* nil "Upcased name -> (NAMES REST BODY): REST is a &rest parameter's key, or NIL (#367, #380).")
(defvar *cc-meta-functions* nil "Upcased name -> (NAMES REST BODY), for a DEFUN-FOR-SYNTAX compile-time helper (#380).")
(defvar *cc-expansions* 0 "Macro expansions performed so far in this program (#367).")
(defparameter +cc-expansion-limit+ 10000
  "Total macro expansions a program may perform; a budget, not a nesting-depth
limit, so it also catches a macro that expands into a call to itself (#367).")
(defvar *cc-meta-steps* 0 "Compile-time evaluation steps performed so far in this program (#380).")
(defparameter +cc-meta-step-limit+ 1000000
  "Total %CC-META-EVAL steps a program's macros and DEFUN-FOR-SYNTAX helpers
may take; catches runaway compile-time recursion (#380).")
(defvar *cc-rename-serial* 0 "Fresh names and expansion marks handed out so far (#367, #380, #382).")
(defvar *cc-expansion-mark* nil "The serial marking every name the running macro expansion's templates write, or NIL outside one (#382).")
(defvar *cc-caller-mark* nil "The mark of the running macro call's own head name, or NIL when it was written in plain source; what UNMARK gives a name (#395).")
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

(defun %cc-local-key (x form)
  "The environment key of the local variable X: its plain key, plus its
expansion's mark when a macro template wrote it. The space makes it a key no
source name can spell, so a template's own variable and a caller's never meet (#382)."
  (let ((key (%cc-key x form)) (mark (get x 'cc-mark)))
    (if mark (format nil "~A ~D" key mark) key)))

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

(defun %cc-op-p (name)
  (assoc (string name) (backend-descriptor-ops *cc-backend*) :test #'string=))

(defun %cc-op (name &rest args)
  "Emit the backend operation NAME, which must exist."
  (unless (%cc-op-p name)
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

(defun %cc-local-cell (symbol)
  "SYMBOL's (KEY . LOCATION) cell in *CC-ENV*, or NIL when it is no local or parameter."
  (assoc (%cc-local-key symbol *cc-form*) *cc-env* :test #'string=))

(defun %cc-lookup (symbol)
  "The location of the variable SYMBOL: (:LOCAL i), (:ARG i), (:GLOBAL LABEL),
(:CONSTANT n) or (:ADDRESS LABEL), a DEFARRAY or DEFSTRING (#366)."
  (let ((key (%cc-key symbol *cc-form*)))
    (or (cdr (%cc-local-cell symbol))
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
    (pushnew (cdr entry) *cc-value-arities*)
    (pushnew key *cc-taken* :test #'string=)
    (car entry)))

;; #397, #402: what a variable can hold, by its binding: a local's environment
;; cell, a global's label, a function's parameter, or a DEFARRAY's element. A
;; binding holds sources, each an argument count (a literal (function F)) or
;; another binding whose holdings flow in, or :UNKNOWN for any other value.
;; They resolve to argument counts once every function is compiled, so a
;; recursive call or a later definition counts. A parameter holds what its
;; direct callers pass; a global that starts non-zero, a taken function's
;; parameter and an escaped array's elements are :UNKNOWN.
(defun %cc-binding (symbol)
  "The key SYMBOL's holdings are recorded under, or NIL for a constant, array or string."
  (or (%cc-local-cell symbol)
      (let ((location (%cc-lookup symbol)))
        (and (eq (first location) :global) (second location)))))

(defun %cc-array-elements (array)
  "The vector of element bindings of the DEFARRAY named ARRAY, or NIL when it is no DEFARRAY."
  (and (%cc-name-p array)
       (let ((location (%cc-lookup array)))
         (and (eq (first location) :address) (gethash (second location) *cc-arrays*)))))

(defun %cc-constant-index (index)
  "INDEX's value when it is an integer or a DEFCONSTANT, else NIL."
  (cond ((integerp index) index)
        ((%cc-name-p index)
         (let ((location (%cc-lookup index)))
           (and (eq (first location) :constant) (second location))))))

(defun %cc-sources (value)
  "The sources of the compiled expression VALUE, or :UNKNOWN."
  (let ((head (%cc-head value)))
    (cond
      ((%cc-function-form-p value)
       (let ((entry (and (= (length value) 2) (%cc-name-p (second value))
                         (gethash (%cc-key (second value) value) *cc-functions*))))
         (if entry (list (cdr entry)) :unknown)))
      ((%cc-name-p value)
       (let ((binding (%cc-binding value)))
         (if binding (list binding) :unknown)))
      ((and (equal head "IF") (= (length value) 4))
       (let ((then (%cc-sources (third value))) (else (%cc-sources (fourth value))))
         (if (or (eq then :unknown) (eq else :unknown)) :unknown (union then else))))
      ((and (equal head "PROGN") (rest value))
       (%cc-sources (car (last value))))
      ((equal head "LET")
       (gethash value *cc-let-sources* :unknown))
      ((and (equal head "AREF") (= (length value) 3))
       (let* ((elements (%cc-array-elements (second value)))
              (position (and elements (%cc-constant-index (third value)))))
         (cond ((null elements) :unknown)
               (position (if (< -1 position (length elements)) (list (cl:aref elements position)) :unknown))
               (t (coerce elements 'list)))))
      (t :unknown))))

(defun %cc-hold (binding value)
  "Record that VALUE, an expression, may be put in BINDING."
  (let ((held (gethash binding *cc-holdings*))
        (sources (%cc-sources value)))
    (unless (eq held :unknown)
      (setf (gethash binding *cc-holdings*)
            (if (eq sources :unknown) :unknown (union held sources))))))

(defun %cc-hold-arguments (key args)
  "Record that the function named KEY is called with ARGS."
  (loop for binding in (gethash key *cc-parameters*)
        for arg in args
        do (%cc-hold binding arg)))

(defun %cc-hold-element (array index value)
  "Record that VALUE is put in element INDEX of ARRAY, or in any element when INDEX is not constant."
  (let ((elements (%cc-array-elements array))
        (position (%cc-constant-index index)))
    (when elements
      (if position
          (when (< -1 position (length elements))
            (%cc-hold (cl:aref elements position) value))
          (loop for element across elements do (%cc-hold element value))))))

(defun %cc-note-escapes (tree)
  "Mark each DEFARRAY TREE names other than as the base of an AREF or ASET as
escaped, and each function whose label it spells as taken."
  (cond
    ((%cc-name-p tree)
     (let ((key (%designator-name tree)))
       (when (gethash key *cc-data*)
         (setf (gethash key *cc-escaped*) t))
       (let ((function (gethash key *cc-label-keys*)))
         (when function
           (pushnew function *cc-taken* :test #'string=)))))
    ((consp tree)
     (loop for rest = (if (and (member (%cc-head tree) '("AREF" "ASET") :test #'equal)
                               (%cc-name-p (second tree)))
                          (cddr tree)
                          tree)
             then (cdr rest)
           while (consp rest)
           do (%cc-note-escapes (car rest))
           finally (%cc-note-escapes rest)))))

(defun %cc-resolve (sources)
  "The argument counts SOURCES lead to, or :UNKNOWN when any binding they pass through is."
  (if (eq sources :unknown)
      :unknown
      (let ((seen (make-hash-table :test 'eq)) (arities '()) (pending sources))
        (loop while pending
              do (let ((source (cl:pop pending)))
                   (if (integerp source)
                       (pushnew source arities)
                       (unless (gethash source seen)
                         (setf (gethash source seen) t)
                         (let ((held (gethash source *cc-holdings*)))
                           (when (eq held :unknown)
                             (return-from %cc-resolve :unknown))
                           (setf pending (append held pending)))))))
        arities)))

(defun %cc-widen-untraced ()
  "Make :UNKNOWN what a caller or an aset this compile did not see may put there."
  (dolist (key *cc-taken*)
    (dolist (binding (gethash key *cc-parameters*))
      (setf (gethash binding *cc-holdings*) :unknown)))
  (maphash (lambda (key escaped)
             (declare (ignore escaped))
             (loop for element across (gethash (gethash key *cc-data*) *cc-arrays* #())
                   do (setf (gethash element *cc-holdings*) :unknown)))
           *cc-escaped*))

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

(defun %cc-clobber-declaration-p (item)
  (and (consp item) (eq (first item) :clobbers)))

(defun %cc-asm-clobbers (form)
  "The upcased register names an (asm ...) FORM declares with a first item
(:clobbers REG...), or :ALL without one."
  (let ((item (second form)))
    (if (%cc-clobber-declaration-p item)
        (let ((descriptor (find-machine-descriptor (backend-descriptor-machine *cc-backend*))))
          (mapcar (lambda (name)
                    (unless (%cc-name-p name)
                      (%cc-fail form "expected a register name in :clobbers, got ~S" name))
                    (handler-case (%backend-register-name descriptor name)
                      (backend-definition-error ()
                        (%cc-fail form "~A in :clobbers is not a register" (%source-name name nil)))))
                  (rest item)))
        :all)))

(defun %cc-hazards (tree)
  "(VALUES CLOBBERS CALL-P): the registers the (asm ...) blocks in TREE, an
operand's source form, can write -- :ALL when one declares none -- and
whether it calls a function, which clobbers the volatile pool (#373): a
call's target may not preserve a :caller-saved register the way it preserves
:callee-saved ones."
  (if (not (consp tree))
      (values nil nil)
      (let* ((head (and (%cc-name-p (first tree)) (%designator-name (first tree))))
             (clobbers (if (equal head "ASM") (%cc-asm-clobbers tree) nil))
             (call (and head (or (equal head "FUNCALL") (and (gethash head *cc-functions*) t)))))
        (dolist (element tree)
          (multiple-value-bind (c k) (%cc-hazards element)
            (setf clobbers (if (or (eq clobbers :all) (eq c :all))
                               :all
                               (union clobbers c :test #'string=)))
            (when k (setf call t))))
        (values clobbers call))))

(defun %cc-take (form)
  "A register from the pool to hold a value across compiling FORM, an
operand not yet compiled, or NIL to fall back to the stack (#373). Registers
an (asm ...) in FORM may clobber (%CC-HAZARDS) are skipped (#377). When FORM
calls a function the pick is a *CC-PRESERVED* register, recorded in
*CC-SAVES* for the function's prologue and epilogue to save and restore: one
already saved, else a new one only inside a loop (#376) or when it is in
*CC-SHARED* (#394). The counting pass (*CC-COUNTING*) takes the first free one
and counts the site at *CC-SITE-WEIGHT* (#400). Otherwise the pick is a
*CC-VOLATILE* register, since a call is the only thing a compiled operand
can do that a :caller-saved register does not survive."
  (multiple-value-bind (clobbers call) (%cc-hazards form)
    (flet ((usable (pool)
             (remove-if (lambda (name) (or (eq clobbers :all) (member name clobbers :test #'string=)))
                        pool))
           (among (names pool)
             (find-if (lambda (name) (member name names :test #'string=)) pool)))
      (if call
          (let* ((pool (usable *cc-preserved*))
                 (register (if *cc-counting*
                               (first pool)
                               (or (among *cc-saves* pool)
                                   (and (plusp *cc-loop-depth*) (first pool))
                                   (among *cc-shared* pool)))))
            (when register
              (pushnew register *cc-saves* :test #'string=)
              (when *cc-counting*
                (incf (gethash register *cc-counting* 0) *cc-site-weight*)))
            register)
          (first (usable *cc-volatile*))))))

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

(defparameter +cc-negations+ '((:eq . :ne) (:ne . :eq) (:lt . :ge) (:ge . :lt) (:gt . :le) (:le . :gt))
  "Each comparison and the one true exactly when it is false.")

(defparameter +cc-flips+ '((:eq . :eq) (:ne . :ne) (:lt . :gt) (:gt . :lt) (:le . :ge) (:ge . :le))
  "Each comparison and the one that gives the same result with its operands swapped.")

(defparameter +cc-commutative+ '(:add :mul :and :or :xor)
  "Operators that give the same result with their operands swapped.")

(defun %cc-swapped (op)
  "The operation that OP becomes with its operands swapped, or NIL."
  (if (member op +cc-commutative+) op (cdr (assoc op +cc-flips+))))

(defun %cc-branch-name (comparison)
  (and comparison (intern (format nil "BRANCH-~A" comparison) :keyword)))

(defun %cc-direct (op form)
  "(VARIANT ARGUMENT) when the leaf FORM, an operator's right operand, can go
straight into the backend's OP-IMM or OP-SLOT variant (#374), else NIL."
  (flet ((variant (suffix)
           (let ((name (intern (format nil "~A-~A" op suffix) :keyword)))
             (and (%cc-op-p name) name))))
    (cond ((integerp form)
           (let ((name (variant "IMM"))) (and name (list name form))))
          ((%cc-function-form-p form)
           (%cc-check-length form 2 2)
           (let ((name (variant "IMM"))) (and name (list name (%cc-function-label form)))))
          ((%cc-name-p form)
           (let ((location (%cc-lookup form)))
             (ecase (first location)
               ((:local :arg) (let ((name (variant "SLOT"))) (and name (list name location))))
               ((:constant :address) (let ((name (variant "IMM"))) (and name (list name (second location)))))
               (:global nil)))))))

(defun %cc-pair-plan (op swapped left right)
  "(KIND . OPERATION) for the operation OP on LEFT and RIGHT: :DIRECT with RIGHT's
variant (#374); else :SWAP with SWAPPED, OP with its operands swapped, on LEFT's
variant when RIGHT is safe to evaluate first (#388); else :OPERANDS, OP on the
temp register, with no OPERATION."
  (let ((direct (%cc-direct op right))
        (swap (and swapped (%cc-direct swapped left))))
    (cond (direct (cons :direct direct))
          ((and swap (%cc-swappable-p left right)) (cons :swap swap))
          (t (list :operands)))))

(defun %cc-pair (op swapped left right)
  "Compile the operands of the operation OP as %CC-PAIR-PLAN plans, and
return (NAME ARGUMENT...): the operation to emit after the accumulator, and
its source."
  (destructuring-bind (kind . operation) (%cc-pair-plan op swapped left right)
    (ecase kind
      (:direct (%cc-expr left) operation)
      (:swap (%cc-expr right) operation)
      (:operands (%cc-operands left right) (list op *cc-temp*)))))

(defun %cc-load-cost (form)
  "Instructions to load FORM into a register: 0 for a non-leaf, whose cost
every way of computing the pair shares."
  (cond ((not (%cc-leaf-p form)) 0)
        ((and (%cc-name-p form) (eq (first (%cc-lookup form)) :global)) 2)
        (t 1)))

(defun %cc-pair-cost (op swapped left right)
  "Instructions, operation included, that %CC-PAIR emits for OP on LEFT and
RIGHT, beyond computing a non-leaf operand."
  (ecase (first (%cc-pair-plan op swapped left right))
    (:direct (1+ (%cc-load-cost left)))
    (:swap (1+ (%cc-load-cost right)))
    (:operands (+ 1 (%cc-load-cost left)
                  (cond ((%cc-leaf-p right) (%cc-load-cost right))
                        ((%cc-swappable-p left right) 1)
                        (t 3))))))

(defun %cc-apply (op form)
  "The accumulator OP FORM, into the accumulator."
  (let ((direct (%cc-direct op form)))
    (cond (direct (apply #'%cc-op (first direct) *cc-acc* (rest direct)))
          (t (%cc-to-temp form)
             (%cc-op op *cc-acc* *cc-temp*)))))

(defun %cc-operator (entry form)
  (destructuring-bind (name op least most) entry
    (let ((args (rest form)))
      (unless (and (>= (length args) least) (or (null most) (<= (length args) most)))
        (%cc-fail form "~(~A~) takes ~D~@[ to ~D~] operand~:P, got ~D"
                  name least (and most (/= most least) most) (length args)))
      (cond ((and (eq op :sub) (null (rest args)))
             (%cc-operands 0 (first args))
             (%cc-op :sub *cc-acc* *cc-temp*))
            (t (let ((pair (%cc-pair op (%cc-swapped op) (first args) (second args))))
                 (apply #'%cc-op (first pair) *cc-acc* (rest pair)))))
      (dolist (arg (cddr args))
        (%cc-apply op arg)))))

(defun %cc-not (form)
  (%cc-check-length form 2 2)
  (let ((pair (%cc-pair :eq :eq (second form) 0)))
    (apply #'%cc-op (first pair) *cc-acc* (rest pair))))

(defun %cc-check-length (form least most)
  (unless (and (listp (cdr form)) (null (cdr (last form)))
               (>= (length form) least) (or (null most) (<= (length form) most)))
    (%cc-fail form "~(~A~) is malformed" (%source-name (first form) nil))))

(defun %cc-progn-form (form)
  (%cc-check-length form 1 nil)
  (%cc-progn (rest form)))

(defun %cc-comparison (form)
  "(OP LEFT RIGHT) when FORM is a well-formed comparison, else NIL."
  (let ((entry (and (consp form) (%cc-name-p (first form)) (%cc-proper-list-p form)
                    (assoc (%designator-name (first form)) *cc-operators* :test #'string=))))
    (and entry (assoc (second entry) +cc-negations+) (= (length form) 3)
         (list (second entry) (second form) (third form)))))

(defun %cc-branch-plan (comparison sense)
  "(BRANCH SWAPPED) for the jump when the COMPARISON, (OP LEFT RIGHT), is true if
SENSE or false if not: the backend's :BRANCH-cmp, and the one it becomes with
its operands swapped. NIL when the backend has no such :BRANCH-cmp."
  (let* ((op (if sense (first comparison) (cdr (assoc (first comparison) +cc-negations+))))
         (name (%cc-branch-name op)))
    (and name (%cc-op-p name)
         (list name (%cc-branch-name (cdr (assoc op +cc-flips+)))))))

(defun %cc-branch-nonzero (target)
  "Jump to TARGET when the accumulator is not 0 (#389)."
  (if (%cc-op-p :branch-ne-imm)
      (%cc-op :branch-ne-imm *cc-acc* 0 target)
      (let ((skip (%cc-new-label)))
        (%cc-op :branch-zero *cc-acc* skip)
        (%cc-op :jump target)
        (%cc-emit (list :label skip)))))

(defun %cc-head (form)
  "The upcased operator name of the call FORM, or NIL."
  (and (consp form) (%cc-name-p (first form)) (%cc-proper-list-p form)
       (%designator-name (first form))))

(defun %cc-branch (form sense target)
  "Jump to TARGET when FORM is true if SENSE, or false if not, else fall
through (#375). A comparison uses the backend's :BRANCH-cmp when it has one; an
and, or and not of conditions jump between their operands and produce no value."
  (let* ((head (%cc-head form))
         (args (and head (rest form)))
         (comparison (%cc-comparison form))
         (branch (and comparison (%cc-branch-plan comparison sense))))
    (cond ((and (equal head "NOT") (= (length args) 1))
           (%cc-branch (first args) (not sense) target))
          ((and (member head '("AND" "OR") :test #'equal) args)
           (let* ((stop (if (equal head "AND") nil t))
                  (skip (unless (eq sense stop) (%cc-new-label)))
                  (*cc-site-weight* *cc-site-weight*))
             (loop for (arg . more) on args
                   do (if more
                          (%cc-branch arg stop (if (eq sense stop) target skip))
                          (%cc-branch arg sense target))
                      (setf *cc-site-weight* (/ *cc-site-weight* 2)))
             (when skip (%cc-emit (list :label skip)))))
          (branch
           (let* ((*cc-form* form)
                  (pair (apply #'%cc-pair (append branch (rest comparison)))))
             (apply #'%cc-op (first pair) *cc-acc* (append (rest pair) (list target)))))
          (sense (%cc-expr form)
                 (%cc-branch-nonzero target))
          (t (%cc-expr form)
             (%cc-op :branch-zero *cc-acc* target)))))

(defun %cc-if (form)
  (%cc-check-length form 3 4)
  (let ((else (%cc-new-label)) (end (%cc-new-label)))
    (%cc-branch (second form) nil else)
    (let ((*cc-site-weight* (/ *cc-site-weight* 2)))
      (%cc-expr (third form))
      (%cc-op :jump end)
      (%cc-emit (list :label else))
      (if (fourth form) (%cc-expr (fourth form)) (%cc-const 0)))
    (%cc-emit (list :label end))))

(defun %cc-while (form)
  (%cc-check-length form 3 nil)
  (let ((top (%cc-new-label)) (end (%cc-new-label)))
    (%cc-emit (list :label top))
    (let ((*cc-loop-depth* (1+ *cc-loop-depth*))
          (*cc-site-weight* (* *cc-site-weight* 4)))
      (%cc-branch (second form) nil end)
      (dolist (body (cddr form)) (%cc-expr body)))
    (%cc-op :jump top)
    (%cc-emit (list :label end))
    (%cc-const 0)))

(defun %cc-jump-cost (sense)
  "Instructions to jump on the accumulator's value, when true if SENSE (#389)."
  (if (and sense (not (%cc-op-p :branch-ne-imm))) 2 1))

(defun %cc-boolean-p (form)
  "True when FORM's value is always 0 or 1."
  (let* ((head (%cc-head form))
         (args (and head (rest form))))
    (cond ((%cc-comparison form) t)
          ((equal head "NOT") (= (length args) 1))
          ((equal head "AND") (or (null args) (%cc-boolean-p (car (last args)))))
          ((equal head "OR") (every #'%cc-boolean-p args)))))

(defun %cc-costs (form)
  "Estimated instructions FORM needs to compute its value, to jump when it is
false and to jump when it is true, as three values. A leaf operand's load counts,
and a comparison or not counts the :cmp or :branch-cmp variant %CC-PAIR would
use (#392); a non-leaf operand's own cost is left out, as every way of computing
FORM shares it. Mirrors %CC-BRANCH and %CC-SHORT-CIRCUIT (#391)."
  (let* ((head (%cc-head form))
         (args (and head (rest form)))
         (comparison (%cc-comparison form)))
    (cond ((and (equal head "NOT") (= (length args) 1))
           (multiple-value-bind (value false true) (%cc-costs (first args))
             (let ((*cc-form* form))
               (values (+ (%cc-pair-cost :eq :eq (first args) 0)
                          (if (%cc-leaf-p (first args)) 0 value))
                       true false))))
          ((and (member head '("AND" "OR") :test #'equal) args)
           (let ((stop (equal head "OR")) (false 0) (true 0) (unfused 0) (saving 0))
             (loop for (arg . more) on args
                   do (multiple-value-bind (v f tr) (%cc-costs arg)
                        (cond (more (incf false (if stop tr f))
                                    (incf true (if stop tr f))
                                    (incf unfused (+ v (%cc-jump-cost stop)))
                                    (incf saving (%cc-saving arg stop v f tr)))
                              (t (incf false f)
                                 (incf true tr)
                                 (incf unfused v)))))
             (values (- unfused (max 0 (- saving 2))) false true)))
          (comparison
           (let* ((*cc-form* form)
                  (value (apply #'%cc-pair-cost (first comparison) (%cc-swapped (first comparison))
                                (rest comparison))))
             (flet ((jump (sense)
                      (let ((plan (%cc-branch-plan comparison sense)))
                        (if plan
                            (apply #'%cc-pair-cost (append plan (rest comparison)))
                            (+ value (%cc-jump-cost sense))))))
               (values value (jump nil) (jump t)))))
          (t (let ((load (%cc-load-cost form)))
               (values load (+ load (%cc-jump-cost nil)) (+ load (%cc-jump-cost t))))))))

(defun %cc-saving (arg sense value false true)
  "Instructions saved by jumping on the and/or operand ARG, given its %CC-COSTS,
rather than computing its value and jumping on that. SENSE is the operand value
that ends the and/or; an or's landing loads 1, so its operand must be 0 or 1."
  (if (and sense (not (%cc-boolean-p arg)))
      0
      (max 0 (- (+ value (%cc-jump-cost sense)) (if sense true false)))))

(defun %cc-fused-saving (arg sense)
  (multiple-value-call #'%cc-saving arg sense (%cc-costs arg)))

(defun %cc-fuses-p (args sense)
  "True when jumping on the and/or operands ARGS into a shared 0/1 landing
(#390, #391) is shorter: it saves more than the landing's :jump and :const."
  (> (loop for arg in (butlast args) sum (%cc-fused-saving arg sense))
     2))

(defun %cc-short-circuit (form sense)
  "An and (SENSE nil) or or (SENSE t) whose value is used. An operand that ends
it jumps to the end with its value in the accumulator, or, for a fused
comparison, to a landing that loads the result."
  (%cc-check-length form 1 nil)
  (if (null (rest form))
      (%cc-const (if sense 0 1))
      (let* ((end (%cc-new-label))
             (fuse (%cc-fuses-p (rest form) sense))
             (landing (and fuse (%cc-new-label)))
             (*cc-site-weight* *cc-site-weight*))
        (loop for (arg . more) on (rest form)
              do (cond ((not more) (%cc-expr arg))
                       ((and fuse (plusp (%cc-fused-saving arg sense)))
                        (%cc-branch arg sense landing))
                       (t (%cc-expr arg)
                          (if sense
                              (%cc-branch-nonzero end)
                              (%cc-op :branch-zero *cc-acc* end))))
                 (setf *cc-site-weight* (/ *cc-site-weight* 2)))
        (when fuse
          (%cc-op :jump end)
          (%cc-emit (list :label landing))
          (%cc-const (if sense 1 0)))
        (%cc-emit (list :label end)))))

(defun %cc-and (form)
  (%cc-short-circuit form nil))

(defun %cc-or (form)
  (%cc-short-circuit form t))

(defun %cc-let (form)
  (%cc-check-length form 2 nil)
  (unless (and (listp (second form)) (null (cdr (last (second form)))))
    (%cc-fail form "let needs a list of (NAME VALUE) bindings"))
  (let ((*cc-env* *cc-env*) (slots 0))
    (dolist (binding (second form))
      (unless (and (consp binding) (= (length binding) 2))
        (%cc-fail form "let binding ~S is not (NAME VALUE)" binding))
      (%cc-expr (second binding))
      (let* ((slot (%cc-alloc))
             (cell (cons (%cc-local-key (first binding) form) slot)))
        (%cc-op :set slot *cc-acc*)
        (incf slots)
        (%cc-hold cell (second binding))
        (cl:push cell *cc-env*)))
    (%cc-progn (cddr form))
    (setf (gethash form *cc-let-sources*)
          (if (cddr form) (%cc-sources (car (last form))) :unknown))
    (dotimes (i slots) (%cc-free))))

(defun %cc-set (form)
  (%cc-check-length form 3 3)
  (let ((location (%cc-lookup (second form))))
    (ecase (first location)
      ((:local :arg) (%cc-expr (third form))
       (%cc-hold (%cc-local-cell (second form)) (third form))
       (%cc-op :set location *cc-acc*))
      (:global (%cc-value-to-temp (third form))
       (%cc-hold (second location) (third form))
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

;; #366, #368: (aref A I)/(aset A I V) index by word, sugar for
;; (peek (+ A (* I W))) and (poke (+ A (* I W)) V), W = *CC-WORD-CELLS*.
;; %CC-PEEK/%CC-POKE already give the address expression the same leaf and
;; operand-ordering treatment as any other (#364). A literal I folds to a
;; literal offset at compile time; a computed I scales by a shift when W is a
;; power of two, so scaling a variable index never newly requires :mul.
(defun %cc-scaled-index (index &optional (factor *cc-word-cells*))
  (cond
    ((= factor 1) index)
    ((integerp index) (* index factor))
    ((= (logcount factor) 1) (list 'shl index (integer-length (1- factor))))
    (t (list '* index factor))))

(defun %cc-aref (form)
  (%cc-check-length form 3 3)
  (%cc-peek (list 'peek (list '+ (second form) (%cc-scaled-index (third form))))))

(defun %cc-aset (form)
  (%cc-check-length form 4 4)
  (%cc-poke (list 'poke (list '+ (second form) (%cc-scaled-index (third form))) (fourth form)))
  (%cc-hold-element (second form) (third form) (fourth form)))

;; #379: (aref-byte S I)/(aset-byte S I V) reach character I of a :packed
;; string, S's byte address plus I, through :peek-byte/:poke-byte. A cell that
;; holds one character (and a one-cell word) needs no byte access: the
;; character is a word, so these are AREF/ASET. Otherwise S's byte address is
;; the backend's :byte-address (d) operation on S, or S times the characters
;; a cell holds, as callfoo/ANIMA-16 LDB/STB byte-address it. The internal
;; form's name has a space, which no source symbol can spell.
(defparameter +cc-byte-address+ (make-symbol "BYTE ADDRESS"))

(defun %cc-byte-address (form)
  (%cc-check-length form 2 2)
  (if (%cc-op-p :byte-address)
      (progn (%cc-expr (second form))
             (%cc-op :byte-address *cc-acc-name*))
      (%cc-expr (%cc-scaled-index (second form) (backend-cell-bytes *cc-backend*)))))

(defun %cc-direct-character-p ()
  (and (= *cc-word-cells* 1) (= (backend-cell-bytes *cc-backend*) 1)))

(defun %cc-aref-byte (form)
  (%cc-check-length form 3 3)
  (if (%cc-direct-character-p)
      (%cc-aref (list* 'aref (rest form)))
      (%cc-peek-byte (list 'peek-byte (list '+ (list +cc-byte-address+ (second form)) (third form))))))

(defun %cc-aset-byte (form)
  (%cc-check-length form 4 4)
  (if (%cc-direct-character-p)
      (%cc-aset (list* 'aset (rest form)))
      (%cc-poke-byte (list 'poke-byte (list '+ (list +cc-byte-address+ (second form)) (third form)) (fourth form)))))

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
         (let* ((*cc-form* form)
                (location (%cc-lookup (second tree)))
                (binding (%cc-binding (second tree))))
           (when binding (setf (gethash binding *cc-holdings*) :unknown))
           (ecase (first location)
             ((:local :arg) location)
             ((:global :address) (second location))
             (:constant (second location)))))
        ((and (consp tree) (null (cdr (last tree))))
         (mapcar (lambda (element) (%cc-substitute-variables element form)) tree))
        (t tree)))

(defun %cc-asm (form)
  (%cc-check-length form 1 nil)
  (let ((clobbers (%cc-asm-clobbers form)))
    (unless (eq clobbers :all)
      (dolist (name (getf (backend-descriptor-registers *cc-backend*) :callee-saved))
        (when (member name clobbers :test #'string=)
          (pushnew name *cc-saves* :test #'string=)))))
  (dolist (item (if (%cc-clobber-declaration-p (second form)) (cddr form) (rest form)))
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
      (%cc-hold-arguments (%cc-key (first form) form) args)
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
      (%cc-hold-arguments key args)
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

(defun %cc-raw-address-p (callee)
  "T when CALLEE is an integer or a constant, an address the program did not take with (function F) (#378)."
  (or (integerp callee)
      (and (%cc-name-p callee) (eq (first (%cc-lookup callee)) :constant))))

(defun %cc-check-indirect-calls ()
  "Fail on the first funcall through a computed target whose argument count no
function value takes (#378), or that the function values reaching its target do not (#397, #402)."
  (%cc-widen-untraced)
  (loop for (arity form function sources name) in (reverse *cc-indirect-calls*)
        for held = (%cc-resolve sources)
        do (let ((*cc-function* function))
             (cond ((consp held)
                    (unless (member arity held)
                      (%cc-fail form "~A ~D argument~:P, but ~A holds only function values taking ~{~D~^, ~}"
                                (if name (format nil "funcall through ~A passes" name) "funcall passes")
                                arity (if name "it" "its target") (sort (copy-list held) #'<))))
                   ((not (member arity *cc-value-arities*))
                    (%cc-fail form "no function value takes ~D argument~:P~@[ (function values take ~{~D~^, ~})~]"
                              arity (sort (copy-list *cc-value-arities*) #'<)))))))

(defun %cc-note-indirect-call (callee args form function)
  (unless (%cc-raw-address-p callee)
    (cl:push (list (length args) form function (%cc-sources callee)
                   (and (%cc-name-p callee) (%source-name callee nil)))
             *cc-indirect-calls*)))

(defun %cc-indirect-funcall (callee args)
  (let ((form *cc-form*) (function *cc-function*)
        (callee-slot (unless (%cc-leaf-p callee)
                       (%cc-expr callee)
                       (let ((slot (%cc-alloc)))
                         (%cc-op :set slot *cc-acc*)
                         slot))))
    (let* ((slots (%cc-args-to-slots args))
           (target (%cc-call-target-register))
           (target-operand (%cc-register-operand target))
           (target-name (%cc-symbol (string-downcase target))))
      (%cc-note-indirect-call callee args form function)
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
    ("AREF-BYTE" . %cc-aref-byte) ("ASET-BYTE" . %cc-aset-byte) ("BYTE ADDRESS" . %cc-byte-address)
    ("ASM" . %cc-asm) ("RETURN" . %cc-return) ("FUNCTION" . %cc-function-expr) ("FUNCALL" . %cc-funcall)))

;;; Macros (#367, #380)

(defun %cc-parse-macro-params (params form)
  "(VALUES NAMES REST) for a defmacro/defun-for-syntax PARAM list: NAMES are
the fixed parameters' upcased keys, in order; REST is a trailing &rest
parameter's key, or NIL."
  (unless (%cc-proper-list-p params)
    (%cc-fail form "parameter list is malformed"))
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

(defun %cc-language-name-p (key)
  "T when KEY names a language form or operator, or a quotation form -- reserved
so DEFMACRO/DEFUN-FOR-SYNTAX can't shadow it."
  (or (assoc key *cc-forms* :test #'string=) (assoc key *cc-operators* :test #'string=)
      (member key '("DEFUN" "DEFVAR" "DEFCONSTANT" "DEFMACRO" "DEFUN-FOR-SYNTAX"
                    "QUOTE" "QUASIQUOTE" "UNQUOTE" "UNQUOTE-SPLICING")
              :test #'string=)))

(defun %cc-meta-built-in-p (key)
  "T when KEY also names a compile-time special form or builtin, which a
DEFUN-FOR-SYNTAX can't shadow but a DEFMACRO can: a macro body calls operators,
never macros, and a program calls macros, never operators (#384)."
  (or (%cc-language-name-p key)
      (assoc key *cc-meta-specials* :test #'string=) (assoc key *cc-meta-builtins* :test #'string=)))

(defun %cc-meta-defined-p (key)
  (or (gethash key *cc-functions*) (gethash key *cc-macros*) (gethash key *cc-meta-functions*)
      (gethash key *cc-globals*) (gethash key *cc-constants*) (gethash key *cc-data*)))

(defun %cc-parse-syntax-definition (form what)
  "(VALUES KEY NAMES REST BODY) for FORM, a (WHAT NAME (PARAM...) BODY...);
checked against a built-in name and a previous definition."
  (unless (and (%cc-proper-list-p form) (>= (length form) 4))
    (%cc-fail form "expected (~A NAME (PARAM...) BODY...)" what))
  (let* ((name (second form)) (key (%cc-key name form)))
    (when (if (string= what "defmacro") (%cc-language-name-p key) (%cc-meta-built-in-p key))
      (%cc-fail form "~A is a built-in form" (%source-name name nil)))
    (when (%cc-meta-defined-p key)
      (%cc-fail form "~A is defined twice" (%source-name name nil)))
    (multiple-value-bind (names rest) (%cc-parse-macro-params (third form) form)
      (values key names rest (cdddr form)))))

(defun %cc-parse-defmacro (form)
  "Register FORM, a (defmacro NAME (PARAM... [&rest R]) BODY...), in
*CC-MACROS*: BODY is evaluated at compile time when NAME is called (#380)."
  (multiple-value-bind (key names rest body) (%cc-parse-syntax-definition form "defmacro")
    (setf (gethash key *cc-macros*) (list names rest body))))

(defun %cc-parse-defun-for-syntax (form)
  "Register FORM, a (defun-for-syntax NAME (PARAM... [&rest R]) BODY...), in
*CC-META-FUNCTIONS*: a compile-time helper a macro or another helper can call
from %CC-META-EVAL (#380)."
  (multiple-value-bind (key names rest body) (%cc-parse-syntax-definition form "defun-for-syntax")
    (setf (gethash key *cc-meta-functions*) (list names rest body))))

(defun %cc-fresh-name (template-name)
  "A fresh name for a GENSYM: its printed name has a space, which no source
symbol can spell, so it can never collide with a caller's variable of the same
name (#380)."
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

;;; Compile-time evaluation (#380)
;;;
;;; %CC-META-EVAL runs a macro or DEFUN-FOR-SYNTAX helper's BODY over plain
;;; source data: integers, strings, symbols (compared by name, never
;;; interned) and lists. () is false; anything else, including the symbol T,
;;; is true. QUOTE returns its argument with every name marked, as a template
;;; would (#395); QUASIQUOTE (%CC-QQ) walks its template, evaluating each UNQUOTE and splicing each UNQUOTE-
;;; SPLICING, and marks every bare name the template writes with the
;;; expansion's own mark (%CC-MARK-NAME), so a binding it makes can't capture a
;;; caller's variable and a caller's binding can't capture a free name it uses
;;; (#367, #382) -- including inside a literal (asm ...), since %CC-QQ walks
;;; every symbol in a template alike. A nested quasiquote raises the depth
;;; an UNQUOTE needs to reach the outer level, as in Common Lisp (#383).

(defun %cc-qq-tagged-p (form tag)
  (and (consp form) (%cc-name-p (first form)) (equal (%designator-name (first form)) tag)
       (consp (rest form)) (null (cddr form))))

(defun %cc-meta-truthy (value) (not (null value)))

(defun %cc-meta-boolean (value) (if value (%cc-symbol "T") nil))

(defun %cc-meta-eq (a b)
  "A and B, EQ for the compile-time evaluator: symbols compare by name, never
by identity, since source is read without interning."
  (cond ((and (%cc-name-p a) (%cc-name-p b)) (string= (%designator-name a) (%designator-name b)))
        ((and (null a) (null b)) t)
        ((and (integerp a) (integerp b)) (= a b))
        ((and (stringp a) (stringp b)) (string= a b))
        (t (eq a b))))

(defun %cc-meta-equal (a b)
  (cond ((and (consp a) (consp b)) (and (%cc-meta-equal (car a) (car b)) (%cc-meta-equal (cdr a) (cdr b))))
        ((or (consp a) (consp b)) nil)
        (t (%cc-meta-eq a b))))

(defun %cc-meta-step (form)
  (when (> (incf *cc-meta-steps*) +cc-meta-step-limit+)
    (%cc-fail form "macro expansion exceeded ~D compile-time evaluation steps" +cc-meta-step-limit+)))

(defun %cc-mark-name (symbol)
  "SYMBOL, a bare name a template wrote, as a fresh symbol of the same name
tagged with the running expansion's mark, or SYMBOL itself outside one (#382)."
  (if *cc-expansion-mark*
      (let ((marked (make-symbol (symbol-name symbol))))
        (setf (get marked 'cc-mark) *cc-expansion-mark*)
        marked)
      symbol))

;; A quoted datum's copy is attributed to the original's position, so an error
;; in a macro's own literal still points at its defmacro.
(defun %cc-map-names (function datum)
  "A copy of DATUM with FUNCTION applied to each name in it."
  (labels ((attribute (new old)
             (let ((position (%cc-form-position old)))
               (when (and position new) (setf (gethash new *cc-positions*) position)))
             new)
           (walk (x)
             (cond ((%cc-name-p x) (attribute (funcall function x) x))
                   ((consp x) (attribute (cons (walk (car x)) (walk (cdr x))) x))
                   (t x))))
    (walk datum)))

(defun %cc-unmark-name (symbol)
  "SYMBOL as a fresh name carrying the running call's own mark, if it has one (#395)."
  (let ((copy (make-symbol (symbol-name symbol))))
    (when *cc-caller-mark* (setf (get copy 'cc-mark) *cc-caller-mark*))
    copy))

(defun %cc-qq (form env depth)
  "FORM, a quasiquote template nested DEPTH quasiquotes deep, with each
UNQUOTE at depth 0 evaluated in ENV, each UNQUOTE-SPLICING's value spliced in,
and each bare name marked (%CC-MARK-NAME). A nested QUASIQUOTE raises DEPTH by
one and an UNQUOTE below depth 0 lowers it, both kept literal (#383)."
  (cond
    ((and (%cc-qq-tagged-p form "UNQUOTE") (zerop depth)) (%cc-meta-eval (second form) env))
    ((and (%cc-qq-tagged-p form "UNQUOTE-SPLICING") (zerop depth))
     (%cc-fail form ",@ is only valid as a list element"))
    ((or (%cc-qq-tagged-p form "UNQUOTE") (%cc-qq-tagged-p form "UNQUOTE-SPLICING"))
     (%cc-remember-position (list* (first form) (%cc-qq-list (rest form) env (1- depth)))))
    ((%cc-qq-tagged-p form "QUASIQUOTE")
     (%cc-remember-position (list* (first form) (%cc-qq-list (rest form) env (1+ depth)))))
    ((%cc-name-p form) (if (zerop depth) (%cc-mark-name form) form))
    ((not (consp form)) form)
    (t (%cc-qq-list form env depth))))

(defun %cc-qq-list (form env depth)
  "FORM, a proper quasiquote template list, walked element by element; an
UNQUOTE-SPLICING occupying an element position at depth 0 splices its value in."
  (unless (%cc-proper-list-p form)
    (%cc-fail form "quasiquote template is malformed"))
  (%cc-remember-position
   (loop for element in form
         append (if (and (zerop depth) (%cc-qq-tagged-p element "UNQUOTE-SPLICING"))
                    (let ((value (%cc-meta-eval (second element) env)))
                      (unless (%cc-proper-list-p value)
                        (%cc-fail element ",@ must splice a list, got ~S" value))
                      (copy-list value))
                    (list (%cc-qq element env depth))))))

(defparameter *cc-meta-specials*
  '(("QUOTE" . %cc-meta-quote) ("QUASIQUOTE" . %cc-meta-quasiquote) ("IF" . %cc-meta-if)
    ("LET" . %cc-meta-let) ("LET*" . %cc-meta-let) ("PROGN" . %cc-meta-progn-form)
    ("AND" . %cc-meta-and) ("OR" . %cc-meta-or) ("COND" . %cc-meta-cond)
    ("WHEN" . %cc-meta-when) ("UNLESS" . %cc-meta-unless)
    ("LAMBDA" . %cc-meta-lambda) ("FUNCTION" . %cc-meta-function))
  "Upcased name -> the function %CC-META-EVAL calls with (FORM ENV) for a
compile-time special form.")

(defun %cc-meta-quote (form env)
  "FORM's datum with each name marked (%CC-MARK-NAME), and each NIL as (), as in Common Lisp."
  (declare (ignore env))
  (unless (= (length form) 2) (%cc-fail form "expected (quote FORM)"))
  (%cc-map-names (lambda (name) (and (string/= (%designator-name name) "NIL") (%cc-mark-name name)))
                   (second form)))

(defun %cc-meta-and (form env)
  (let ((value (%cc-meta-boolean t)))
    (dolist (test (rest form) value)
      (setf value (%cc-meta-eval test env))
      (unless (%cc-meta-truthy value) (return nil)))))

(defun %cc-meta-or (form env)
  (dolist (test (rest form) nil)
    (let ((value (%cc-meta-eval test env)))
      (when (%cc-meta-truthy value) (return value)))))

(defun %cc-meta-when (form env)
  (unless (>= (length form) 2) (%cc-fail form "expected (~A TEST BODY...)" (%source-name (first form) nil)))
  (and (%cc-meta-truthy (%cc-meta-eval (second form) env)) (%cc-meta-progn (cddr form) env)))

(defun %cc-meta-unless (form env)
  (unless (>= (length form) 2) (%cc-fail form "expected (~A TEST BODY...)" (%source-name (first form) nil)))
  (and (not (%cc-meta-truthy (%cc-meta-eval (second form) env))) (%cc-meta-progn (cddr form) env)))

(defun %cc-meta-cond (form env)
  (dolist (clause (rest form) nil)
    (unless (and (consp clause) (%cc-proper-list-p clause))
      (%cc-fail form "cond clause ~S is not (TEST BODY...)" clause))
    (let ((value (%cc-meta-eval (first clause) env)))
      (when (%cc-meta-truthy value)
        (return (if (rest clause) (%cc-meta-progn (rest clause) env) value))))))

(defstruct (cc-closure (:constructor %make-cc-closure (names rest body env))
                       (:predicate %cc-closure-p) (:conc-name %cc-closure-) (:copier nil))
  names rest body env)

(defmethod print-object ((closure cc-closure) stream)
  (write-string "#<compile-time lambda>" stream))

(defun %cc-meta-lambda (form env)
  (unless (and (%cc-proper-list-p form) (>= (length form) 3)) (%cc-fail form "expected (lambda (PARAM...) BODY...)"))
  (multiple-value-bind (names rest) (%cc-parse-macro-params (second form) form)
    (%make-cc-closure names rest (cddr form) env)))

(defun %cc-meta-function-p (key)
  (or (assoc key *cc-meta-builtins* :test #'string=) (gethash key *cc-meta-functions*)))

(defun %cc-meta-function (form env)
  "FORM, (function NAME): NAME itself, checked to be a builtin or a DEFUN-FOR-SYNTAX helper."
  (declare (ignore env))
  (unless (and (= (length form) 2) (%cc-name-p (second form))) (%cc-fail form "expected (function NAME)"))
  (unless (%cc-meta-function-p (%designator-name (second form)))
    (%cc-fail form "~A is not a compile-time function" (%source-name (second form) nil)))
  (second form))

(defun %cc-meta-quasiquote (form env)
  (unless (= (length form) 2) (%cc-fail form "expected (quasiquote FORM)"))
  (%cc-qq (second form) env 0))

(defun %cc-meta-if (form env)
  (unless (<= 3 (length form) 4) (%cc-fail form "expected (if TEST THEN [ELSE])"))
  (if (%cc-meta-truthy (%cc-meta-eval (second form) env))
      (%cc-meta-eval (third form) env)
      (and (cdddr form) (%cc-meta-eval (fourth form) env))))

(defun %cc-meta-progn (body env)
  (let ((result nil))
    (dolist (form body result) (setf result (%cc-meta-eval form env)))))

(defun %cc-meta-progn-form (form env) (%cc-meta-progn (rest form) env))

(defun %cc-meta-let (form env)
  (unless (and (consp (rest form)) (%cc-proper-list-p (second form)))
    (%cc-fail form "let needs a list of (NAME VALUE) bindings"))
  (let ((sequential (equal (%designator-name (first form)) "LET*")) (inner env) (new '()))
    (dolist (binding (second form))
      (unless (and (consp binding) (= (length binding) 2) (%cc-name-p (first binding)))
        (%cc-fail form "let binding ~S is not (NAME VALUE)" binding))
      (let ((value (%cc-meta-eval (second binding) (if sequential inner env))))
        (cl:push (cons (%designator-name (first binding)) value) new)
        (when sequential (setf inner (cons (first new) inner)))))
    (%cc-meta-progn (cddr form) (if sequential inner (append (nreverse new) env)))))

(defun %cc-meta-arity (form args n)
  (unless (= (length args) n)
    (%cc-fail form "~A takes ~D argument~:P, got ~D" (%source-name (first form) nil) n (length args))))

(defun %cc-meta-integer (form value)
  (unless (integerp value) (%cc-fail form "expected an integer, got ~S" value))
  value)

(defun %cc-meta-list (form value)
  (unless (%cc-proper-list-p value) (%cc-fail form "expected a list, got ~S" value))
  value)

(defun %cc-meta-index (form value)
  (unless (and (integerp value) (>= value 0)) (%cc-fail form "expected a non-negative index, got ~S" value))
  value)

(defun %cc-meta-divisor (form value)
  (when (zerop (%cc-meta-integer form value)) (%cc-fail form "division by zero"))
  value)

(defun %cc-meta-string (form value)
  (unless (stringp value) (%cc-fail form "expected a string, got ~S" value))
  value)

(defun %cc-meta-symbol-string (form value)
  "VALUE, a string INTERN can turn into a name: no whitespace, reader syntax or
colon, so it can't forge a mark or a gensym, and not a number."
  (%cc-meta-string form value)
  (when (or (zerop (length value))
            (find-if (lambda (c) (or (member c '(#\Space #\Tab #\Newline #\Return #\Page))
                                     (find c "()'`,;\"|\\:")))
                     value)
            (ignore-errors (parse-integer value)))
    (%cc-fail form "cannot intern ~S as a name" value))
  value)

(defun %cc-meta-bind (form who names rest args)
  "((NAME . ARG)...) for a call to WHO: NAMES bound to ARGS, and REST to the remainder."
  (let ((fixed (length names)))
    (unless (and (>= (length args) fixed) (or rest (= (length args) fixed)))
      (%cc-fail form "~A takes ~:[exactly~;at least~] ~D argument~:P, got ~D" who rest fixed (length args)))
    (append (loop for name in names for arg in args collect (cons name arg))
            (and rest (list (cons rest (nthcdr fixed args)))))))

(defun %cc-meta-apply (form function args)
  "FUNCTION, a closure or the name of a builtin or DEFUN-FOR-SYNTAX helper,
called on the evaluated ARGS; FORM is the call, for errors."
  (cond
    ((%cc-closure-p function)
     (%cc-meta-progn (%cc-closure-body function)
                     (append (%cc-meta-bind form "lambda" (%cc-closure-names function) (%cc-closure-rest function) args)
                             (%cc-closure-env function))))
    ((%cc-name-p function)
     (let* ((key (%designator-name function))
            (call (cons function (rest form)))
            (builtin (cdr (assoc key *cc-meta-builtins* :test #'string=))))
       (let ((position (%cc-form-position form)))
         (when position (setf (gethash call *cc-positions*) position)))
       (cond (builtin (funcall builtin call args))
             ((gethash key *cc-meta-functions*) (%cc-meta-call-function key call args))
             (t (%cc-fail form "~A is not a compile-time function" (%source-name function nil))))))
    (t (%cc-fail form "~S is not a compile-time function" function))))

(defparameter *cc-meta-builtins*
  (list
   (cons "CAR" (lambda (form args) (%cc-meta-arity form args 1)
                 (let ((x (first args))) (cond ((null x) nil) ((consp x) (car x))
                                                (t (%cc-fail form "car needs a list, got ~S" x))))))
   (cons "CDR" (lambda (form args) (%cc-meta-arity form args 1)
                 (let ((x (first args))) (cond ((null x) nil) ((consp x) (cdr x))
                                                (t (%cc-fail form "cdr needs a list, got ~S" x))))))
   (cons "CONS" (lambda (form args) (%cc-meta-arity form args 2) (cons (first args) (second args))))
   (cons "LIST" (lambda (form args) (declare (ignore form)) (copy-list args)))
   (cons "APPEND" (lambda (form args) (apply #'append (mapcar (lambda (a) (%cc-meta-list form a)) args))))
   (cons "LENGTH" (lambda (form args) (%cc-meta-arity form args 1) (length (%cc-meta-list form (first args)))))
   (cons "NULL" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (null (first args)))))
   (cons "CONSP" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (consp (first args)))))
   (cons "SYMBOLP" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (%cc-name-p (first args)))))
   (cons "INTEGERP" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (integerp (first args)))))
   (cons "EQ" (lambda (form args) (%cc-meta-arity form args 2) (%cc-meta-boolean (%cc-meta-eq (first args) (second args)))))
   (cons "EQUAL" (lambda (form args) (%cc-meta-arity form args 2) (%cc-meta-boolean (%cc-meta-equal (first args) (second args)))))
   (cons "NOT" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (null (first args)))))
   (cons "REVERSE" (lambda (form args) (%cc-meta-arity form args 1) (reverse (%cc-meta-list form (first args)))))
   (cons "NTH" (lambda (form args) (%cc-meta-arity form args 2)
                 (nth (%cc-meta-index form (first args)) (%cc-meta-list form (second args)))))
   (cons "NTHCDR" (lambda (form args) (%cc-meta-arity form args 2)
                    (nthcdr (%cc-meta-index form (first args)) (%cc-meta-list form (second args)))))
   (cons "SECOND" (lambda (form args) (%cc-meta-arity form args 1) (second (%cc-meta-list form (first args)))))
   (cons "THIRD" (lambda (form args) (%cc-meta-arity form args 1) (third (%cc-meta-list form (first args)))))
   (cons "LAST" (lambda (form args) (%cc-meta-arity form args 1) (last (%cc-meta-list form (first args)))))
   (cons "MEMBER" (lambda (form args) (%cc-meta-arity form args 2)
                    (member (first args) (%cc-meta-list form (second args)) :test #'%cc-meta-equal)))
   (cons "ASSOC" (lambda (form args) (%cc-meta-arity form args 2)
                   (dolist (entry (%cc-meta-list form (second args)))
                     (unless (or (null entry) (consp entry)) (%cc-fail form "assoc needs a list of pairs, got ~S" entry)))
                   (assoc (first args) (second args) :test #'%cc-meta-equal)))
   (cons "STRINGP" (lambda (form args) (%cc-meta-arity form args 1) (%cc-meta-boolean (stringp (first args)))))
   (cons "STRING=" (lambda (form args) (%cc-meta-arity form args 2)
                     (%cc-meta-boolean (string= (%cc-meta-string form (first args)) (%cc-meta-string form (second args))))))
   (cons "CONCAT" (lambda (form args) (apply #'concatenate 'string (mapcar (lambda (a) (%cc-meta-string form a)) args))))
   (cons "SYMBOL-NAME" (lambda (form args) (%cc-meta-arity form args 1)
                         (unless (%cc-name-p (first args)) (%cc-fail form "symbol-name needs a symbol, got ~S" (first args)))
                         (%source-name (first args) nil)))
   (cons "INTERN" (lambda (form args) (%cc-meta-arity form args 1)
                    (%cc-mark-name (%cc-symbol (%cc-meta-symbol-string form (first args))))))
   (cons "NUMBER-TO-STRING" (lambda (form args) (%cc-meta-arity form args 1)
                              (format nil "~D" (%cc-meta-integer form (first args)))))
   (cons "UNMARK" (lambda (form args) (%cc-meta-arity form args 1)
                    (%cc-map-names #'%cc-unmark-name (first args))))
   (cons "FUNCALL" (lambda (form args)
                     (unless args (%cc-fail form "funcall needs a function"))
                     (%cc-meta-apply form (first args) (rest args))))
   (cons "APPLY" (lambda (form args)
                   (unless args (%cc-fail form "apply needs a function"))
                   (%cc-meta-apply form (first args)
                                   (and (rest args) (append (butlast (rest args)) (%cc-meta-list form (car (last args))))))))
   (cons "MAPCAR" (lambda (form args)
                    (unless (>= (length args) 2) (%cc-fail form "mapcar needs a function and at least 1 list"))
                    (let ((lists (mapcar (lambda (a) (%cc-meta-list form a)) (rest args))))
                      (loop repeat (reduce #'min lists :key #'length)
                            collect (%cc-meta-apply form (first args) (mapcar #'car lists))
                            do (setf lists (mapcar #'cdr lists))))))
   (cons "MOD" (lambda (form args) (%cc-meta-arity form args 2)
                 (mod (%cc-meta-integer form (first args)) (%cc-meta-divisor form (second args)))))
   (cons "ASH" (lambda (form args) (%cc-meta-arity form args 2)
                 (let ((shift (%cc-meta-integer form (second args))))
                   (unless (<= -64 shift 64) (%cc-fail form "ash shifts by at most 64, got ~D" shift))
                   (ash (%cc-meta-integer form (first args)) shift))))
   (cons "GENSYM" (lambda (form args)
                    (unless (<= (length args) 1) (%cc-fail form "gensym takes at most 1 argument, got ~D" (length args)))
                    (%cc-fresh-name (if args (first args) "g"))))
   (cons "ERROR" (lambda (form args)
                   (unless (and args (stringp (first args))) (%cc-fail form "error needs a string message"))
                   (%cc-fail form "~A" (apply #'format nil (first args) (rest args))))))
  "Upcased name -> a function (FORM ARGS) of a compile-time evaluator
builtin, ARGS already evaluated.")

;; + - * min max logand logior fold over any number of integer arguments; = < >
;; <= >= /= compare a run of them, like Common Lisp's.
(dolist (spec '(("+" . +) ("-" . -) ("*" . *) ("MIN" . min) ("MAX" . max) ("LOGAND" . logand) ("LOGIOR" . logior)))
  (let ((name (car spec)) (op (cdr spec)))
    (cl:push (cons name
                (lambda (form args)
                  (unless args (%cc-fail form "~A needs at least 1 argument" name))
                  (let ((ints (mapcar (lambda (a) (%cc-meta-integer form a)) args)))
                    (if (and (string= name "-") (null (rest ints))) (- (first ints)) (reduce op ints)))))
          *cc-meta-builtins*)))
(dolist (spec '(("=" . =) ("<" . <) (">" . >) ("<=" . <=) (">=" . >=) ("/=" . /=)))
  (let ((name (car spec)) (op (cdr spec)))
    (cl:push (cons name
                (lambda (form args)
                  (unless (>= (length args) 2) (%cc-fail form "~A needs at least 2 arguments" name))
                  (%cc-meta-boolean (apply op (mapcar (lambda (a) (%cc-meta-integer form a)) args)))))
          *cc-meta-builtins*)))

(cl:push (cons "/" (lambda (form args)
                      (unless (>= (length args) 2) (%cc-fail form "/ needs at least 2 arguments"))
                      (let ((ints (mapcar (lambda (a) (%cc-meta-integer form a)) args)))
                        (dolist (divisor (rest ints)) (%cc-meta-divisor form divisor))
                        (reduce (lambda (a b) (values (truncate a b))) ints))))
         *cc-meta-builtins*)

(defun %cc-meta-eval (form env)
  "FORM, source data from a macro or DEFUN-FOR-SYNTAX helper's BODY,
evaluated in ENV ((NAME . VALUE)...). Symbols are looked up by name; NIL and
T are self-evaluating; a list dispatches on its head, a *CC-META-SPECIALS*
name, a *CC-META-BUILTINS* name, or another macro/helper's name (#380)."
  (%cc-meta-step form)
  (cond
    ((integerp form) form)
    ((stringp form) form)
    ((null form) nil)
    ((%cc-name-p form)
     (let ((name (%designator-name form)))
       (cond ((string= name "NIL") nil)
             ((string= name "T") form)
             (t (let ((binding (assoc name env :test #'string=)))
                  (unless binding (%cc-fail form "unbound compile-time variable ~A" (%source-name form nil)))
                  (cdr binding))))))
    ((consp form)
     (unless (%cc-proper-list-p form) (%cc-fail form "expected (OPERATOR ARG...)"))
     (unless (%cc-name-p (first form)) (%cc-fail form "expected an operator name, got ~S" (first form)))
     (let* ((key (%designator-name (first form))) (special (cdr (assoc key *cc-meta-specials* :test #'string=))))
       (if special
           (funcall special form env)
           (%cc-meta-call key form env))))
    (t (%cc-fail form "~S is not valid in a compile-time expression" form))))

(defun %cc-meta-call (key form env)
  "FORM, a call whose head KEY is neither a special form nor renamed: a
*CC-META-BUILTINS* or *CC-META-FUNCTIONS* name, checked before its
arguments are evaluated, so a call to an unknown operator fails on itself,
not on whatever its arguments happen to be."
  (let ((builtin (cdr (assoc key *cc-meta-builtins* :test #'string=))))
    (cond
      (builtin (funcall builtin form (mapcar (lambda (a) (%cc-meta-eval a env)) (rest form))))
      ((gethash key *cc-meta-functions*)
       (%cc-meta-call-function key form (mapcar (lambda (a) (%cc-meta-eval a env)) (rest form))))
      (t (%cc-fail form "~A is not a compile-time operator" (%source-name (first form) nil))))))

(defun %cc-meta-call-function (key form args)
  "A call to the DEFUN-FOR-SYNTAX helper KEY: its own arguments, ARGS, are
already evaluated, like an ordinary function -- unlike a macro's, which sees
its arguments unevaluated."
  (destructuring-bind (names rest body) (gethash key *cc-meta-functions*)
    (%cc-meta-progn body (%cc-meta-bind form (%source-name (first form) nil) names rest args))))

(defun %cc-expand-call (name entry form)
  "FORM, a call (NAME ARG...) matching macro ENTRY = (NAMES REST BODY), with
BODY evaluated at compile time (#380): each parameter is bound to the call's
own argument form, unevaluated, and &rest to the remaining argument forms as
a list. A STORAGE-CONDITION from runaway recursion becomes a positioned
error, same as the step and expansion budgets."
  (destructuring-bind (names rest body) entry
    (unless (%cc-proper-list-p form)
      (%cc-fail form "expected (~A ARG...)" (%source-name name nil)))
    (let* ((args (rest form)) (fixed (length names)))
      (unless (and (>= (length args) fixed) (or rest (= (length args) fixed)))
        (%cc-fail form "~A takes ~:[exactly~;at least~] ~D argument~:P, got ~D"
                  (%source-name name nil) rest fixed (length args)))
      (when (> (incf *cc-expansions*) +cc-expansion-limit+)
        (%cc-fail form "macro ~A expands too many times (over ~D expansions)"
                  (%source-name name nil) +cc-expansion-limit+))
      (let ((env (append (loop for key in names for arg in args collect (cons key arg))
                          (and rest (list (cons rest (nthcdr fixed args)))))))
        (handler-case (let ((*cc-expansion-mark* (incf *cc-rename-serial*))
                            (*cc-caller-mark* (and (%cc-name-p name) (get name 'cc-mark))))
                        (%cc-meta-progn body env))
          (storage-condition () (%cc-fail form "macro ~A recursed too deeply" (%source-name name nil))))))))

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

(defun %cc-function-items (name params body label)
  "The (:function LABEL ...) item for BODY, already macro-expanded."
  (let* ((*cc-out* '()) (*cc-env* '()) (*cc-next* 0) (*cc-max* 0) (*cc-depth* 0) (*cc-loop-depth* 0) (*cc-site-weight* 1) (*cc-saves* '())
         (arg-registers (let ((args (getf (backend-descriptor-call *cc-backend*) :args)))
                          (if (eq args :stack) 0 (length args)))))
    (loop for param in params
          for index from 0
          do (let ((key (%cc-local-key param name)))
               (when (assoc key *cc-env* :test #'string=)
                 (%cc-fail name "~A is a parameter twice" (%source-name param nil)))
               (if (< index arg-registers)
                   (let ((slot (%cc-alloc)))
                     (let ((*cc-form* name))
                       (%cc-op :set slot (list :arg index)))
                     (cl:push (cons key slot) *cc-env*))
                   (cl:push (cons key (list :arg index)) *cc-env*))
               (setf (gethash (first *cc-env*) *cc-holdings*)
                     (list (nth index (gethash (%designator-name name) *cc-parameters*))))))
    (%cc-progn body)
    (list* :function label
           (append (list :args (length params) :locals *cc-max*)
                   ;; A preserved register %CC-TAKE used (#373); the backend's
                   ;; own :callee-saved convention pushes and pops it, which
                   ;; also restores it correctly across an early (return).
                   (and *cc-saves* (list :save (mapcar (lambda (name) (%cc-symbol (string-downcase name)))
                                                        (reverse *cc-saves*)))))
           (nreverse (cl:push (list :return) *cc-out*)))))

(defun %cc-shared-registers (name params body label)
  "The upcased names of the preserved registers whose call sites, weighted by
how often they run (#400), total more than one run: the save and restore cost
a push and a pop a call, and each site saves one (#394). Counted by compiling
the function once and discarding the items."
  (let ((*cc-counting* (make-hash-table :test 'equal))
        (*cc-labels* *cc-labels*)
        (*cc-indirect-calls* *cc-indirect-calls*))
    (%cc-function-items name params body label)
    (loop for register being the hash-keys of *cc-counting* using (hash-value sites)
          when (> sites 1) collect register)))

(defun %cc-function (definition)
  (destructuring-bind (name params body label) definition
    (let* ((*cc-function* (%source-name name nil))
           ;; Every macro in the file is registered by now (%CC-COLLECT ran
           ;; first), regardless of where NAME's DEFUN sits relative to them (#367).
           (body (mapcar #'%cc-expand-all body))
           (*cc-shared* (and (eq *cc-optimize* :speed)
                             (%cc-shared-registers name params body label))))
      (mapc #'%cc-note-escapes body)
      (%cc-function-items name params body label))))

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
           (let ((label (gethash key *cc-data*)))
             (when label
               (setf (gethash key *cc-escaped*) t))
             label)
           (%cc-fail form "~A is not a constant, a function or an array/string"
                     (%source-name value nil)))))
    (t (%cc-fail form "~S is not a constant, a function or an array/string" value))))

(defun %cc-register-array (label values)
  "Give each element of the DEFARRAY LABEL, laid out from VALUES, a binding holding
what it starts as: a (function F)'s argument count, nothing for 0, else :UNKNOWN (#402)."
  (setf (gethash label *cc-arrays*)
        (map 'vector
             (lambda (value)
               (let ((element (list :element label)))
                 (cond ((%cc-function-form-p value)
                        (setf (gethash element *cc-holdings*)
                              (list (cdr (gethash (%cc-key (second value) value) *cc-functions*)))))
                       ((not (eql value 0))
                        (setf (gethash element *cc-holdings*) :unknown)))
                 element))
             values)))

(defun %cc-word-data (values)
  "The directive item laying VALUES out as *CC-WORD-CELLS*-cell words: .cell
for a one-cell word, else .emit with the width first (#386)."
  (if (= *cc-word-cells* 1)
      (list* :directive (%cc-symbol "cell") values)
      (list* :directive (%cc-symbol "emit") *cc-word-cells* values)))

(defun %cc-check-packable (string form)
  "Fail unless every character of STRING is 8 bits, as .PACKZ packs (#379, #398)."
  (when (find-if (lambda (char) (> (char-code char) 255)) string)
    (%cc-fail form "a :packed string holds 8-bit characters only")))

(defun %cc-collect (forms)
  "(VALUES DEFINITIONS GLOBALS DATA), registering functions, globals,
constants, DEFARRAY/DEFSTRING data (#366), macros and DEFUN-FOR-SYNTAX
helpers (#367, #380), in file order. A top-level macro call expands in
place, and a (progn DEF...) it (or the source) produces flattens. A DEFUN's
own body is expanded later, in %CC-FUNCTION, once every macro and helper
here is registered. DATA's array/string values are resolved only once every
form is registered, so one may name a function, macro or array/string
defined later in FORMS."
  (let ((definitions '()) (globals '()) (arrays '()) (seen (make-hash-table :test 'equal)))
    (labels
        ((claim (name form label)
           (let ((existing (gethash (symbol-name label) seen)))
             (when (and existing (string/= existing (%designator-name name)))
               (%cc-fail form "~A and ~A both make the label ~A"
                         (%source-name name nil) existing (symbol-name label)))
             (setf (gethash (symbol-name label) seen) (%designator-name name))))
         (defined-p (key) (or (gethash key *cc-globals*) (gethash key *cc-constants*)
                               (gethash key *cc-data*) (gethash key *cc-macros*)
                               (gethash key *cc-meta-functions*)))
         (process (form)
           (unless (and (consp form) (%cc-name-p (first form)) (%cc-proper-list-p form))
             (%cc-fail form "expected (defun ...), (defvar ...), (defconstant ...), (defarray ...), (defstring ...), (defmacro ...) or (defun-for-syntax ...)"))
           (let* ((head (%designator-name (first form)))
                  (macro (gethash head *cc-macros*)))
             (cond
               ((string= head "PROGN") (dolist (sub (rest form)) (process sub)))
               (macro (process (%cc-expand-macro-form (first form) macro form)))
               ((string= head "DEFMACRO") (%cc-parse-defmacro form))
               ((string= head "DEFUN-FOR-SYNTAX") (%cc-parse-defun-for-syntax form))
               ((string= head "DEFUN")
                (unless (and (>= (length form) 3) (listp (third form)) (null (cdr (last (third form)))))
                  (%cc-fail form "expected (defun NAME (PARAM...) BODY...)"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "fn" name)))
                  (when (or (assoc key *cc-forms* :test #'string=) (assoc key *cc-operators* :test #'string=))
                    (%cc-fail form "~A is a built-in form" (%source-name name nil)))
                  (when (or (gethash key *cc-functions*) (gethash key *cc-macros*) (gethash key *cc-meta-functions*))
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-functions*) (cons label (length (third form)))
                        (gethash key *cc-parameters*) (loop for index below (length (third form))
                                                            collect (list :parameter key index))
                        (gethash (string-upcase (symbol-name label)) *cc-label-keys*) key)
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
                (unless (and (<= 3 (length form) 4) (stringp (third form))
                             (or (null (cdddr form)) (%keyword-named-p (fourth form) "PACKED")))
                  (%cc-fail form "expected (defstring NAME \"text\" [:packed])"))
                (let* ((name (second form)) (key (%cc-key name form)) (label (%cc-mangle "st" name)))
                  (when (defined-p key)
                    (%cc-fail form "~A is defined twice" (%source-name name nil)))
                  (claim name form label)
                  (setf (gethash key *cc-data*) label)
                  (cl:push (list label (if (cdddr form) :packed :string) (third form) form) arrays)))
               (t (%cc-fail form "expected (defun ...), (defvar ...), (defconstant ...), (defarray ...), (defstring ...), (defmacro ...) or (defun-for-syntax ...)"))))))
      (dolist (form forms) (process form)))
    (values (nreverse definitions) (nreverse globals)
            (loop for (label kind payload form) in (nreverse arrays)
                  append (list (list :label label)
                               (ecase kind
                                 (:size (%cc-register-array label (make-list payload :initial-element 0))
                                  (list :directive (%cc-symbol "res") (* payload *cc-word-cells*)))
                                 (:values (prog1 (%cc-word-data (mapcar (lambda (value) (%cc-array-value value form)) payload))
                                            (%cc-register-array label payload)))
                                 ;; #368: W=1 keeps .asciz's own terminator; a
                                 ;; wider word has no terminated-string
                                 ;; directive, so the trailing 0 is emitted as
                                 ;; a value alongside the string's characters.
                                 (:packed (%cc-check-packable payload form)
                                          (list :directive (%cc-symbol "packz") payload))
                                 (:string (if (= *cc-word-cells* 1)
                                              (list :directive (%cc-symbol "asciz") payload)
                                              (%cc-word-data (list payload 0))))))))))

(defun compile-program (forms &key backend positions source file (optimize :size))
  "The items that compile FORMS, a list of (defun ...), (defvar ...) and
(defconstant ...) forms, for BACKEND. A stub at the start stores the
globals' initial values, calls main and halts. Signals PROGRAM-COMPILE-ERROR.
POSITIONS, SOURCE and FILE, as READ-SOURCE and READ-SOURCE-FROM-STRING set
them on an ITEMS-PROGRAM, let errors report FILE:LINE:COLUMN (#362). OPTIMIZE
is :SIZE, the fewest instructions, or :SPEED, which also holds an operand
across calls in a preserved register shared by two or more sites (#394)."
  (unless backend
    (%cc-fail nil "compiling needs a backend"))
  (unless (member optimize '(:size :speed))
    (%cc-fail nil ":optimize must be :size or :speed, got ~S" optimize))
  (let ((*cc-backend* (find-backend backend))
        (*cc-optimize* optimize) (*cc-shared* nil) (*cc-counting* nil)
        (*cc-functions* (make-hash-table :test 'equal))
        (*cc-globals* (make-hash-table :test 'equal))
        (*cc-constants* (make-hash-table :test 'equal))
        (*cc-data* (make-hash-table :test 'equal))
        (*cc-function* nil) (*cc-form* nil) (*cc-labels* 0) (*cc-depth* 0) (*cc-out* '())
        (*cc-acc* nil) (*cc-temp* nil) (*cc-acc-name* nil) (*cc-temp-name* nil)
        (*cc-volatile* nil) (*cc-preserved* nil) (*cc-saves* nil)
        (*cc-positions* positions) (*cc-source* source) (*cc-file* file)
        (*cc-macros* (make-hash-table :test 'equal)) (*cc-expansions* 0)
        (*cc-meta-functions* (make-hash-table :test 'equal)) (*cc-meta-steps* 0)
        (*cc-rename-serial* 0) (*cc-expand-position* nil)
        (*cc-word-cells* (backend-word-cells backend))
        (*cc-value-arities* '()) (*cc-indirect-calls* '())
        (*cc-holdings* (make-hash-table :test 'eq))
        (*cc-let-sources* (make-hash-table :test 'eq))
        (*cc-parameters* (make-hash-table :test 'equal))
        (*cc-label-keys* (make-hash-table :test 'equal)) (*cc-taken* '())
        (*cc-arrays* (make-hash-table :test 'eq))
        (*cc-escaped* (make-hash-table :test 'equal)))
    (%cc-registers)
    (multiple-value-bind (definitions globals data) (%cc-collect forms)
      (let ((main (gethash "MAIN" *cc-functions*)))
        (unless (and main (zerop (cdr main)))
          (%cc-fail nil "the program needs (defun main () ...)"))
        (loop for (label value) in globals
              do (unless (zerop value)
                   (setf (gethash label *cc-holdings*) :unknown)
                   (%cc-op :const *cc-acc* label)
                   (%cc-op :const *cc-temp* value)
                   (%cc-op :poke *cc-acc-name* *cc-temp-name*)))
        (%cc-emit (list :call (car main)))
        (%cc-op :halt)
        (append (nreverse *cc-out*)
                (prog1 (mapcar #'%cc-function definitions)
                  (%cc-check-indirect-calls))
                (loop for (label) in globals
                      append (list (list :label label)
                                   (list :directive (%cc-symbol "res") *cc-word-cells*)))
                data)))))

;;; Source files

(defun %source-fail (control &rest args)
  (let ((detail (apply #'format nil control args)))
    (error 'program-compile-error :detail detail :message detail)))

(defun %parse-source (forms)
  "The ITEMS-PROGRAM whose items are FORMS, less a leading (:program (option...))."
  (let ((head (first forms)))
    (if (and (consp head) (%keyword-named-p (first head) "PROGRAM"))
        (let ((program (%parse-program head :optimize t)))
          (setf (items-program-items program) (rest forms))
          program)
        (make-items-program :items forms))))

(defun %read-source-forms (stream path)
  "(VALUES FORMS POSITIONS) for STREAM, as READ-SOURCE and READ-SOURCE-FROM-
STRING read it. POSITIONS maps each form to a character offset into the text
STREAM reads from, for #362."
  (let ((positions (make-hash-table :test 'eq)))
    (values (read-restricted-forms stream #'%source-fail path :bare :uninterned :positions positions :quotes t)
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

(defun compile-source (program &key backend optimize)
  "An ITEMS-PROGRAM of the items that compile the source PROGRAM, with its
options. BACKEND and OPTIMIZE (:SIZE or :SPEED, as for COMPILE-PROGRAM)
override the program's; OPTIMIZE is :SIZE when neither names one."
  (let ((backend (or backend (items-program-backend program)))
        (optimize (or optimize (items-program-optimize program) :size)))
    (unless backend
      (%source-fail "no backend: name one in (:program (:backend NAME)) or pass one"))
    (let ((compiled (copy-items-program program)))
      (setf (items-program-items compiled)
            (compile-program (items-program-items program) :backend backend
                              :positions (items-program-positions program)
                              :source (items-program-source program)
                              :file (items-program-file program)
                              :optimize optimize)
            (items-program-backend compiled) backend)
      compiled)))

(defun compile-source-file (path &key backend optimize)
  "Compile the source file PATH to an ITEMS-PROGRAM."
  (compile-source (read-source path) :backend backend :optimize optimize))

(defun assemble-source-file (path &key backend machine lexer origin memory optimize)
  "Compile the source file PATH and assemble it as ASSEMBLE-ITEMS-FILE does."
  (%assemble-items-program (compile-source-file path :backend backend :optimize optimize) path
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
