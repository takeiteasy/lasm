;;;; backend.lisp
;;;; #113: DEFBACKEND -- a declarative compiler-target description for a
;;;; machine: register roles, a calling convention, frame layout, the operand
;;;; kinds a front end may name (each an addressing mode) and primitive
;;;; operations expanding to instruction forms. items.lisp assembles programs
;;;; written against one.
;;;;
;;;; Clause heads, mnemonics, registers and modes are matched by name, so a
;;;; backend can be written in any package.

(in-package #:lasm)

(define-condition backend-definition-error (definition-error) ())
(define-condition unknown-backend (lookup-error) ())

(defun %backend-error (control &rest args)
  (apply #'%definition-error 'backend-definition-error control args))

(defstruct backend-descriptor
  name        ; symbol
  machine     ; name of the machine described
  registers   ; plist of role -> upcased name, or list of names (see +BACKEND-REGISTER-ROLES+)
  call        ; plist :ARGS :ORDER :CLEANUP :RETURN-ADDRESS-SLOTS
  frame       ; plist :GROWS :ALIGNMENT
  operands    ; alist of (KIND-NAME . MODE-NAME)
  ops)        ; alist of (OP-NAME PARAMS FORM...), names upcased

(defvar *backends* (make-hash-table :test 'equal)
  "Defined backends, keyed by upcased name.")

(defun %designator-name (designator)
  "DESIGNATOR, a symbol or string, as an upcased string; NIL for anything else."
  (and (or (symbolp designator) (stringp designator))
       (string-upcase (string designator))))

(defun find-backend (designator)
  "The BACKEND-DESCRIPTOR named DESIGNATOR (a symbol or string, matched by
name), or DESIGNATOR itself when it is one. Signals UNKNOWN-BACKEND."
  (if (backend-descriptor-p designator)
      designator
      (or (and (%designator-name designator)
               (gethash (%designator-name designator) *backends*))
          (%lookup-error 'unknown-backend designator "No backend named ~S has been defined with DEFBACKEND"
                         designator))))

(defun backend-register (backend role)
  "The register name (or list of names) BACKEND assigns to ROLE, or NIL."
  (getf (backend-descriptor-registers (find-backend backend)) role))

;;; Resolution by name

(defun %find-machine-name (designator)
  (let ((key (%designator-name designator)))
    (and key
         (or (and (symbolp designator) (gethash designator *machines*) designator)
             (loop for name being the hash-keys of *machines*
                   when (string= key (%designator-name name)) return name)))))

(defun %find-mode-by-name (designator machine)
  (let ((key (%designator-name designator)))
    (and key
         (or (and (symbolp designator) (%lookup-mode designator machine))
             (find key (%visible-modes machine) :test #'string=
                                                :key (lambda (mode) (%designator-name (mode-descriptor-name mode))))))))

(defun %find-mode-name (designator machine)
  (let ((mode (%find-mode-by-name designator machine)))
    (and mode (mode-descriptor-name mode))))

(defun %backend-register-name (descriptor designator)
  "DESIGNATOR's upcased name, if it is a register or register alias of DESCRIPTOR's machine."
  (let ((key (%designator-name designator)))
    (unless key
      (%backend-error "~S is not a register name" designator))
    (unless (or (nth-value 1 (gethash key (machine-descriptor-register-aliases descriptor)))
                (find-if (lambda (element)
                           (and (eq (storage-element-kind element) :register)
                                (string= key (%designator-name (storage-element-name element)))))
                         (machine-descriptor-elements descriptor)))
      (%backend-error "~A is not a register or register alias of machine ~S"
                      key (machine-descriptor-name descriptor)))
    key))

;;; Expression operators, shared with items.lisp

(defun %expression-operator (designator)
  "The punctuation keyword of the expression operator DESIGNATOR names, or NIL."
  (let* ((name (and (or (symbolp designator) (stringp designator)) (string designator)))
         (entry (and name (assoc name *punctuators* :test #'string=))))
    (and entry
         (or (assoc (cdr entry) *binary-precedence*) (assoc (cdr entry) *unary-ops*))
         (cdr entry))))

;;; Clauses

(defparameter +backend-register-roles+ '(:return :arguments :scratch :caller-saved :callee-saved)
  "Register roles holding a list of registers.")

(defparameter +backend-register-singles+ '(:stack-pointer :program-counter :frame-pointer)
  "Register roles holding one register.")

(defun %check-plist (what plist allowed)
  (unless (and (listp plist) (evenp (length plist)))
    (%backend-error "~A: expected keyword/value pairs, got ~S" what plist))
  (let ((seen '()))
    (loop for (key nil) on plist by #'cddr
          do (unless (member key allowed)
               (%backend-error "~A: unknown option ~S; expected one of ~{~S~^ ~}" what key allowed))
             (when (member key seen)
               (%backend-error "~A: ~S given more than once" what key))
             (cl:push key seen))))

(defun %parse-registers-clause (descriptor args)
  (%check-plist "registers" args (append +backend-register-roles+ +backend-register-singles+))
  (let (result)
    (loop for (role value) on args by #'cddr
          do (setf (getf result role)
                   (if (member role +backend-register-roles+)
                       (progn
                         (unless (listp value)
                           (%backend-error "registers ~S: expected a list of registers, got ~S" role value))
                         (let ((names (mapcar (lambda (register) (%backend-register-name descriptor register))
                                              value)))
                           (unless (= (length names) (length (remove-duplicates names :test #'string=)))
                             (%backend-error "registers ~S lists a register twice" role))
                           names))
                       (%backend-register-name descriptor value))))
    (let ((both (intersection (getf result :caller-saved) (getf result :callee-saved) :test #'string=)))
      (when both
        (%backend-error "registers ~A cannot be both :caller-saved and :callee-saved" (first both))))
    result))

(defun %parse-call-clause (descriptor args)
  (%check-plist "call" args '(:args :order :cleanup :return-address-slots))
  (let ((arguments (getf args :args :stack))
        (order (getf args :order :right-to-left))
        (cleanup (getf args :cleanup :caller))
        (slots (getf args :return-address-slots 1)))
    (unless (or (eq arguments :stack) (listp arguments))
      (%backend-error "call :args must be :stack or a list of registers, got ~S" arguments))
    (unless (member order '(:left-to-right :right-to-left))
      (%backend-error "call :order must be :left-to-right or :right-to-left, got ~S" order))
    (unless (member cleanup '(:caller :callee))
      (%backend-error "call :cleanup must be :caller or :callee, got ~S" cleanup))
    (unless (typep slots '(integer 0))
      (%backend-error "call :return-address-slots must be a non-negative integer, got ~S" slots))
    (list :args (if (eq arguments :stack)
                    :stack
                    (mapcar (lambda (register) (%backend-register-name descriptor register)) arguments))
          :order order :cleanup cleanup :return-address-slots slots)))

(defun %parse-frame-clause (args)
  (%check-plist "frame" args '(:grows :alignment))
  (let ((grows (getf args :grows)) (alignment (getf args :alignment 1)))
    (unless (member grows '(nil :down :up))
      (%backend-error "frame :grows must be :down or :up, got ~S" grows))
    (unless (typep alignment '(integer 1))
      (%backend-error "frame :alignment must be a positive integer, got ~S" alignment))
    (list :grows grows :alignment alignment)))

(defun %parse-operands-clause (machine entries)
  (let (result)
    (dolist (entry entries)
      (%definition-bind (kind mode) entry
        (let ((key (%designator-name kind)))
          (unless key
            (%backend-error "operands: ~S is not a kind name" kind))
          (when (%expression-operator kind)
            (%backend-error "operands: ~A is an expression operator, not a kind name" key))
          (when (assoc key result :test #'string=)
            (%backend-error "operands: kind ~A is declared twice" key))
          (let ((mode-name (%find-mode-name mode machine)))
            (unless mode-name
              (%backend-error "operands: kind ~A names ~S, which is not an addressing mode visible to machine ~S"
                              key mode machine))
            (cl:push (cons key mode-name) result)))))
    (nreverse result)))

(defun %parse-ops-clause (entries)
  (let (result)
    (dolist (entry entries)
      (%definition-bind (name params &rest forms) entry
        (let ((key (%designator-name name)))
          (unless key
            (%backend-error "ops: ~S is not an operation name" name))
          (when (assoc key result :test #'string=)
            (%backend-error "ops: ~A is declared twice" key))
          (unless (and (listp params) (every #'%designator-name params))
            (%backend-error "ops: ~A parameters must be a list of names, got ~S" key params))
          (let ((names (mapcar #'%designator-name params)))
            (unless (= (length names) (length (remove-duplicates names :test #'string=)))
              (%backend-error "ops: ~A repeats a parameter" key))
            (unless forms
              (%backend-error "ops: ~A has no instruction forms" key))
            (cl:push (list* key names forms) result)))))
    (nreverse result)))

;;; Op template checks, run once every clause is known

(defun %check-hole-value (op value params)
  (typecase value
    ((or integer string) t)
    (symbol (when (keywordp value)
              (%backend-error "ops: ~A uses the keyword ~S as an operand value" op value)))
    (cons (unless (%expression-operator (first value))
            (%backend-error "ops: ~A: ~S is not an expression" op value))
          (dolist (element (rest value))
            (%check-hole-value op element params)))
    (t (%backend-error "ops: ~A: ~S cannot be an operand value" op value))))

(defun %check-op-operand (op operand params kinds machine)
  (flet ((param-p (name) (member (%designator-name name) params :test #'equal)))
    (typecase operand
      ((or integer string) t)
      (symbol (unless (and (not (keywordp operand)) (param-p operand))
                (%backend-error "ops: ~A: ~S is neither a parameter nor an operand; write (KIND value...)"
                                op operand)))
      (cons
       (let ((head (first operand)))
         (unless (or (symbolp head) (stringp head))
           (%backend-error "ops: ~A: ~S is not an operand" op operand))
         (cond ((and (keywordp head) (string= (symbol-name head) "MODE"))
                (unless (%find-mode-by-name (second operand) machine)
                  (%backend-error "ops: ~A: ~S is not an addressing mode" op (second operand)))
                (dolist (value (cddr operand))
                  (%check-hole-value op value params)))
               ((%expression-operator head)
                (%check-hole-value op operand params))
               ((param-p head)
                (%backend-error "ops: ~A: parameter ~A cannot head an operand" op head))
               ((assoc (%designator-name head) kinds :test #'string=)
                (dolist (value (rest operand))
                  (%check-hole-value op value params)))
               (t (%backend-error "ops: ~A: ~S is not a declared operand kind" op head)))))
      (t (%backend-error "ops: ~A: ~S cannot be an operand" op operand)))))

(defun %check-op-form (op form params kinds machine)
  (unless (and (consp form) (or (stringp (first form)) (and (symbolp (first form)) (not (keywordp (first form))))))
    (%backend-error "ops: ~A: ~S is not an instruction form" op form))
  (let ((mnemonic (string (first form))))
    (unless (and (plusp (length mnemonic)) (char= (char mnemonic 0) #\.))
      (handler-case (find-instruction-variants machine mnemonic)
        (unknown-instruction ()
          (%backend-error "ops: ~A: machine ~S has no instruction ~A" op machine mnemonic)))))
  (dolist (operand (rest form))
    (%check-op-operand op operand params kinds machine)))

(defun %check-backend-ops (descriptor)
  (dolist (entry (backend-descriptor-ops descriptor))
    (destructuring-bind (op params &rest forms) entry
      (dolist (form forms)
        (%check-op-form op form params (backend-descriptor-operands descriptor)
                        (backend-descriptor-machine descriptor))))))

(defun %finish-backend-stack (descriptor machine-descriptor)
  "Check the backend's stack pointer and frame direction against the machine's
declared (stack-pointer ...), and default the frame direction from it."
  (let* ((registers (backend-descriptor-registers descriptor))
         (sp (getf registers :stack-pointer))
         (declared (loop for pointer being the hash-values of (machine-descriptor-stack-pointers machine-descriptor)
                         collect pointer))
         (match (and sp (find sp declared :test #'string=
                                          :key (lambda (pointer) (%designator-name (stack-pointer-descriptor-register pointer))))))
         (grows (getf (backend-descriptor-frame descriptor) :grows)))
    (when (and sp declared (not match))
      (%backend-error "registers :stack-pointer ~A is not the machine's declared stack-pointer (~{~A~^, ~})"
                      sp (mapcar #'stack-pointer-descriptor-register declared)))
    (when (and match grows (not (eq grows (stack-pointer-descriptor-grows match))))
      (%backend-error "frame :grows ~S disagrees with the machine's (stack-pointer ~A :grows ~S)"
                      grows sp (stack-pointer-descriptor-grows match)))
    (setf (getf (backend-descriptor-frame descriptor) :grows)
          (or grows (and match (stack-pointer-descriptor-grows match)) :down))))

(defun %clause-head-name (clause)
  (and (consp clause) (%designator-name (first clause))))

(defun %backend-machine-option (name options)
  (unless (and (consp options) (evenp (length options)) (eq (first options) :machine) (= (length options) 2))
    (%backend-error "DEFBACKEND ~S: expected (:machine NAME) after the name, got ~S" name options))
  (or (%find-machine-name (second options))
      (%backend-error "DEFBACKEND ~S: machine ~S has not been defined" name (second options))))

(defun %define-backend (name options clauses)
  (%with-definition (name backend-definition-error)
    (unless (and (symbolp name) name)
      (%backend-error "DEFBACKEND: ~S is not a valid backend name" name))
    (let* ((machine (%backend-machine-option name options))
           (machine-descriptor (find-machine-descriptor machine))
           (descriptor (make-backend-descriptor :name name :machine machine :frame (list :grows nil :alignment 1)
                                                :call (list :args :stack :order :right-to-left :cleanup :caller
                                                            :return-address-slots 1)))
           (seen '()))
      (dolist (clause clauses)
        (let ((head (%clause-head-name clause)))
          (when (member head seen :test #'equal)
            (%backend-error "DEFBACKEND ~S: more than one ~(~A~) clause" name head))
          (cl:push head seen)
          (cond ((equal head "REGISTERS")
                 (setf (backend-descriptor-registers descriptor)
                       (%parse-registers-clause machine-descriptor (rest clause))))
                ((equal head "CALL")
                 (setf (backend-descriptor-call descriptor)
                       (%parse-call-clause machine-descriptor (rest clause))))
                ((equal head "FRAME")
                 (setf (backend-descriptor-frame descriptor) (%parse-frame-clause (rest clause))))
                ((equal head "OPERANDS")
                 (setf (backend-descriptor-operands descriptor) (%parse-operands-clause machine (rest clause))))
                ((equal head "OPS")
                 (setf (backend-descriptor-ops descriptor) (%parse-ops-clause (rest clause))))
                (t (%backend-error "DEFBACKEND ~S: unknown clause ~S; expected registers, call, frame, operands or ops"
                                   name clause)))))
      (%finish-backend-stack descriptor machine-descriptor)
      (%check-backend-ops descriptor)
      (setf (gethash (%designator-name name) *backends*) descriptor))))

(defmacro defbackend (name options &body clauses)
  "Define the compiler-target description NAME for a machine, from
OPTIONS, (:machine MACHINE), and CLAUSES, each one of:
     (registers [:return (reg...)] [:arguments (reg...)] [:scratch (reg...)]
                [:caller-saved (reg...)] [:callee-saved (reg...)]
                [:stack-pointer reg] [:program-counter reg] [:frame-pointer reg])
     (call [:args :stack/(reg...)] [:order :left-to-right/:right-to-left]
           [:cleanup :caller/:callee] [:return-address-slots n])
     (frame [:grows :down/:up] [:alignment n])
     (operands (KIND mode-name)...)
     (ops (NAME (param...) (mnemonic operand...)...)...)
An operand kind names an addressing mode; an operation expands to instruction
forms whose operands are (KIND value...) items, parameters, or expressions.
Registers, modes and mnemonics are checked against the machine, and clause
heads are matched by name, so DEFBACKEND works from any package. See
docs/backends.md."
  (%definition-toplevel-form `(%define-backend ',name ',options ',clauses) `',name))
