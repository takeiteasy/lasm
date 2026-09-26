;;;; backend.lisp
;;;; #113: DEFBACKEND -- a declarative compiler-target description for a
;;;; machine: register roles, a calling convention, frame layout, the operand
;;;; kinds a front end may name (each an addressing mode) and primitive
;;;; operations expanding to instruction forms. items.lisp assembles programs
;;;; written against one.
;;;;
;;;; Clause heads, mnemonics, registers and modes are matched by name, so a
;;;; backend can be written in any package.
;;;;
;;;; #323: (:extends PARENT) merges the parent's clauses under the child's, by
;;;; key, and checks the result against the child's machine.

(in-package #:lasm)

(define-condition backend-definition-error (definition-error) ())
(define-condition unknown-backend (lookup-error) ())

(defun %backend-error (control &rest args)
  (apply #'%definition-error 'backend-definition-error control args))

(defstruct backend-descriptor
  name        ; symbol
  machine     ; name of the machine described
  registers   ; plist of role -> upcased name, or list of names (see +BACKEND-REGISTER-ROLES+); :OPERAND -> kind name
  call        ; plist :ARGS :ORDER :CLEANUP :RETURN-ADDRESS-SLOTS
  frame       ; plist :GROWS :ALIGNMENT, and :SLOT (a kind name) and :POINTER (a register name) when given
  operands    ; alist of (KIND-NAME . MODE-NAME)
  ops         ; alist of (OP-NAME PARAMS FORM...), names upcased
  parent      ; name of the backend extended, or NIL
  clauses)    ; the registers, call, frame, operands and ops clauses after merging with the parent's

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

(defparameter +backend-clause-keys+
  `(("REGISTERS" ,@+backend-register-roles+ ,@+backend-register-singles+ :operand)
    ("CALL" :args :order :cleanup :return-address-slots)
    ("FRAME" :grows :alignment :slot :pointer))
  "The keys each plist clause takes, by clause head.")

(defun %clause-keys (head)
  (cdr (assoc head +backend-clause-keys+ :test #'string=)))

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
  (%check-plist "registers" args (%clause-keys "REGISTERS"))
  (let (result)
    (loop for (role value) on args by #'cddr
          do (setf (getf result role)
                   (cond
                     ((eq role :operand)
                      (or (%designator-name value)
                          (%backend-error "registers :operand: ~S is not a kind name" value)))
                     ((member role +backend-register-roles+)
                      (unless (listp value)
                        (%backend-error "registers ~S: expected a list of registers, got ~S" role value))
                      (let ((names (mapcar (lambda (register) (%backend-register-name descriptor register))
                                           value)))
                        (unless (= (length names) (length (remove-duplicates names :test #'string=)))
                          (%backend-error "registers ~S lists a register twice" role))
                        names))
                     (t (%backend-register-name descriptor value)))))
    (let ((both (intersection (getf result :caller-saved) (getf result :callee-saved) :test #'string=)))
      (when both
        (%backend-error "registers ~A cannot be both :caller-saved and :callee-saved" (first both))))
    result))

(defun %parse-call-clause (descriptor args)
  (%check-plist "call" args (%clause-keys "CALL"))
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

(defun %parse-frame-clause (descriptor args)
  (%check-plist "frame" args (%clause-keys "FRAME"))
  (let ((grows (getf args :grows)) (alignment (getf args :alignment 1)) (slot (getf args :slot))
        (pointer (getf args :pointer)))
    (unless (member grows '(nil :down :up))
      (%backend-error "frame :grows must be :down or :up, got ~S" grows))
    (unless (typep alignment '(integer 1))
      (%backend-error "frame :alignment must be a positive integer, got ~S" alignment))
    (when (and slot (not (%designator-name slot)))
      (%backend-error "frame :slot: ~S is not a kind name" slot))
    (append (list :grows grows :alignment alignment)
            (and slot (list :slot (%designator-name slot)))
            (and pointer (list :pointer (%backend-register-name descriptor pointer))))))

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

;;; Checks run once every clause is known

(defparameter +backend-hook-arities+
  '(("PUSH" . 1) ("POP" . 1) ("ALLOC" . 1) ("FREE" . 1) ("MOVE" . 2) ("EXCHANGE" . 2) ("CALL" . 1) ("RETURN" . 0) ("RETURN-POP" . 1)
    ("ENTER" . 0) ("LEAVE" . 0))
  "Operations that convention lowering (items.lisp) emits, with their parameter counts.")

(defun %check-backend-hooks (descriptor)
  (loop for (name . arity) in +backend-hook-arities+
        for entry = (assoc name (backend-descriptor-ops descriptor) :test #'string=)
        when (and entry (/= arity (length (second entry))))
          do (%backend-error "ops: ~A is used by call lowering and takes ~D parameter~:P, not ~D"
                             name arity (length (second entry)))))

(defun %check-backend-kinds (descriptor)
  (loop for (what kind) in `(("registers :operand" ,(getf (backend-descriptor-registers descriptor) :operand))
                             ("frame :slot" ,(getf (backend-descriptor-frame descriptor) :slot)))
        when (and kind (not (assoc kind (backend-descriptor-operands descriptor) :test #'string=)))
          do (%backend-error "~A: ~A is not a declared operand kind" what kind)))

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

(defun %label-form-p (form)
  (and (consp form) (keywordp (first form)) (string= (symbol-name (first form)) "LABEL")))

(defun %template-labels (op params forms)
  "The upcased names of the labels FORMS define, each (:label NAME). A label is
local to one expansion of the operation."
  (let ((names '()))
    (dolist (form forms)
      (when (%label-form-p form)
        (let ((name (and (= (length form) 2) (not (keywordp (second form))) (%designator-name (second form)))))
          (unless name
            (%backend-error "ops: ~A: expected (:label NAME), got ~S" op form))
          (when (member name params :test #'equal)
            (%backend-error "ops: ~A: label ~A is also a parameter" op name))
          (when (member name names :test #'equal)
            (%backend-error "ops: ~A: label ~A is defined twice" op name))
          (cl:push name names))))
    (nreverse names)))

(defun %check-op-operand (op operand params kinds machine &optional labels)
  (flet ((param-p (name) (member (%designator-name name) params :test #'equal)))
    (typecase operand
      ((or integer string) t)
      (symbol (unless (and (not (keywordp operand))
                           (or (param-p operand) (member (%designator-name operand) labels :test #'equal)))
                (%backend-error "ops: ~A: ~S is neither a parameter, a label nor an operand; write (KIND value...)"
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

(defun %check-op-form (op form params kinds machine &optional labels)
  (when (%label-form-p form)
    (return-from %check-op-form nil))
  (unless (and (consp form) (or (stringp (first form)) (and (symbolp (first form)) (not (keywordp (first form))))))
    (%backend-error "ops: ~A: ~S is not an instruction form" op form))
  (let ((mnemonic (string (first form))))
    (unless (and (plusp (length mnemonic)) (char= (char mnemonic 0) #\.))
      (handler-case (find-instruction-variants machine mnemonic)
        (unknown-instruction ()
          (%backend-error "ops: ~A: machine ~S has no instruction ~A" op machine mnemonic)))))
  (dolist (operand (rest form))
    (%check-op-operand op operand params kinds machine labels)))

(defun %check-backend-ops (descriptor)
  (dolist (entry (backend-descriptor-ops descriptor))
    (destructuring-bind (op params &rest forms) entry
      (let ((labels (%template-labels op params forms)))
        (dolist (form forms)
          (%check-op-form op form params (backend-descriptor-operands descriptor)
                          (backend-descriptor-machine descriptor) labels))))))

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

;;; The frame pointer

(defun %finish-backend-pointer (descriptor)
  "Reconcile (frame :pointer REG) with (registers :frame-pointer REG), and check the pointer is
not another register the convention uses."
  (let* ((pointer (getf (backend-descriptor-frame descriptor) :pointer))
         (registers (backend-descriptor-registers descriptor))
         (role (getf registers :frame-pointer)))
    (when (and pointer role (string/= pointer role))
      (%backend-error "frame :pointer ~A disagrees with registers :frame-pointer ~A" pointer role))
    (when pointer
      (setf (getf (backend-descriptor-registers descriptor) :frame-pointer) pointer)
      (loop for (what names) in `(("call :args" ,(let ((args (getf (backend-descriptor-call descriptor) :args)))
                                                   (and (listp args) args)))
                                  ("registers :return" ,(getf registers :return))
                                  ("registers :stack-pointer" ,(list (getf registers :stack-pointer))))
            when (member pointer names :test #'equal)
              do (%backend-error "frame :pointer ~A is also in ~A" pointer what)))))

;;; Inheritance

(defun %clause-head-name (clause)
  (and (consp clause) (%designator-name (first clause))))

(defun %entry-key (entry)
  (and (consp entry) (%designator-name (first entry))))

(defun %merge-entries (parent child)
  "PARENT's entries with CHILD's in place of those of the same name, then CHILD's new ones."
  (let ((result (copy-list parent)))
    (dolist (entry child)
      (let ((position (and (%entry-key entry)
                           (position (%entry-key entry) result :test #'equal :key #'%entry-key))))
        (if position
            (setf (nth position result) entry)
            (setf result (append result (list entry))))))
    result))

(defun %merge-clause (head parent child)
  "The clause HEAD, CHILD's over PARENT's; either may be NIL."
  (cond ((null child) parent)
        ((null parent) child)
        ((member head '("OPERANDS" "OPS") :test #'string=)
         (cons (first child) (%merge-entries (rest parent) (rest child))))
        (t (let ((result (copy-list (rest parent))))
             (%check-plist head (rest child) (%clause-keys head))
             (loop for (key value) on (rest child) by #'cddr
                   do (setf (getf result key) value))
             (cons (first child) result)))))

(defun %drop-ops (name ops without)
  "The ops clause OPS without the operations WITHOUT names."
  (dolist (op without)
    (unless (and (%designator-name op) (find (%designator-name op) (rest ops) :test #'equal :key #'%entry-key))
      (%backend-error "DEFBACKEND ~S: without-ops names ~S, which the parent does not define" name op)))
  (cons (first ops) (remove-if (lambda (entry) (member (%entry-key entry) without :test #'equal :key #'%designator-name))
                               (rest ops))))

(defun %merge-backend-clauses (name parent clauses)
  "PARENT's clauses (a descriptor's CLAUSES) with CLAUSES, a child's, merged in, in the parent's order."
  (let ((without (loop for clause in clauses
                       when (equal (%clause-head-name clause) "WITHOUT-OPS") append (rest clause)))
        (own (remove "WITHOUT-OPS" clauses :test #'equal :key #'%clause-head-name)))
    (flet ((find-clause (head list) (find head list :test #'equal :key #'%clause-head-name)))
      (let ((merged (loop for head in '("REGISTERS" "CALL" "FRAME" "OPERANDS" "OPS")
                          for clause = (%merge-clause head (find-clause head parent) (find-clause head own))
                          when clause collect clause)))
        (if (and without (find-clause "OPS" merged))
            (substitute (%drop-ops name (find-clause "OPS" merged) without) (find-clause "OPS" merged) merged)
            (progn (when without (%backend-error "DEFBACKEND ~S: without-ops names ~S, which the parent does not define"
                                                 name (first without)))
                   merged))))))

(defun %backend-options (name options)
  "The machine name and the parent descriptor (or NIL) DEFBACKEND's OPTIONS give."
  (unless (and (consp options) (evenp (length options)))
    (%backend-error "DEFBACKEND ~S: expected (:machine NAME) and/or (:extends PARENT) after the name, got ~S"
                    name options))
  (%check-plist (format nil "DEFBACKEND ~S" name) options '(:machine :extends))
  (let* ((parent-name (getf options :extends))
         (parent (and parent-name
                      (or (and (%designator-name parent-name)
                               (gethash (%designator-name parent-name) *backends*))
                          (%backend-error "DEFBACKEND ~S extends ~S, which has not been defined" name parent-name))))
         (given (getf options :machine))
         (machine (cond ((and given (%find-machine-name given)))
                        (given (%backend-error "DEFBACKEND ~S: machine ~S has not been defined" name given))
                        (parent (backend-descriptor-machine parent))
                        (t (%backend-error "DEFBACKEND ~S: expected (:machine NAME) after the name, got ~S"
                                           name options)))))
    (when (and parent (equal (%designator-name name) (%designator-name (backend-descriptor-name parent))))
      (%backend-error "DEFBACKEND ~S cannot extend itself" name))
    (when (and parent (not (eq machine (backend-descriptor-machine parent)))
               (not (member (backend-descriptor-machine parent) (%machine-ancestors machine))))
      (%backend-error "DEFBACKEND ~S: machine ~S is not ~S or a machine extending it, which ~S targets"
                      name machine (backend-descriptor-machine parent) (backend-descriptor-name parent)))
    (values machine parent)))

(defun %check-backend-clause-heads (name clauses extendsp)
  (let ((seen '()))
    (dolist (clause clauses)
      (let ((head (%clause-head-name clause)))
        (when (member head seen :test #'equal)
          (%backend-error "DEFBACKEND ~S: more than one ~(~A~) clause" name head))
        (cl:push head seen)
        (unless (or (member head '("REGISTERS" "CALL" "FRAME" "OPERANDS" "OPS") :test #'equal)
                    (and extendsp (equal head "WITHOUT-OPS")))
          (%backend-error "DEFBACKEND ~S: unknown clause ~S; expected registers, call, frame, operands, ops~:[~; or without-ops~]"
                          name clause extendsp))))))

(defun %define-backend (name options clauses)
  (%with-definition (name backend-definition-error)
    (unless (and (symbolp name) name)
      (%backend-error "DEFBACKEND: ~S is not a valid backend name" name))
    (multiple-value-bind (machine parent) (%backend-options name options)
      (%check-backend-clause-heads name clauses parent)
      (let* ((machine-descriptor (find-machine-descriptor machine))
             (clauses (if parent
                          (%merge-backend-clauses name (backend-descriptor-clauses parent) clauses)
                          clauses))
             (descriptor (make-backend-descriptor :name name :machine machine :frame (list :grows nil :alignment 1)
                                                  :call (list :args :stack :order :right-to-left :cleanup :caller
                                                              :return-address-slots 1)
                                                  :parent (and parent (backend-descriptor-name parent))
                                                  :clauses clauses)))
        (dolist (clause clauses)
          (let ((head (%clause-head-name clause)))
            (cond ((equal head "REGISTERS")
                   (setf (backend-descriptor-registers descriptor)
                         (%parse-registers-clause machine-descriptor (rest clause))))
                  ((equal head "CALL")
                   (setf (backend-descriptor-call descriptor)
                         (%parse-call-clause machine-descriptor (rest clause))))
                  ((equal head "FRAME")
                   (setf (backend-descriptor-frame descriptor)
                         (%parse-frame-clause machine-descriptor (rest clause))))
                  ((equal head "OPERANDS")
                   (setf (backend-descriptor-operands descriptor) (%parse-operands-clause machine (rest clause))))
                  ((equal head "OPS")
                   (setf (backend-descriptor-ops descriptor) (%parse-ops-clause (rest clause)))))))
        (%finish-backend-stack descriptor machine-descriptor)
        (%finish-backend-pointer descriptor)
        (%check-backend-kinds descriptor)
        (%check-backend-ops descriptor)
        (%check-backend-hooks descriptor)
        (setf (gethash (%designator-name name) *backends*) descriptor)))))

(defmacro defbackend (name options &body clauses)
  "Define the compiler-target description NAME for a machine, from
OPTIONS, (:machine MACHINE) and/or (:extends PARENT), and CLAUSES, each one of:
     (registers [:return (reg...)] [:arguments (reg...)] [:scratch (reg...)]
                [:caller-saved (reg...)] [:callee-saved (reg...)]
                [:stack-pointer reg] [:program-counter reg] [:frame-pointer reg]
                [:operand kind])
     (call [:args :stack/(reg...)] [:order :left-to-right/:right-to-left]
           [:cleanup :caller/:callee] [:return-address-slots n])
     (frame [:grows :down/:up] [:alignment n] [:slot kind] [:pointer reg])
     (operands (KIND mode-name)...)
     (ops (NAME (param...) (mnemonic operand...)...)...)
     (without-ops NAME...)                    ; with :extends only
:extends merges PARENT's clauses under these by key, for the same machine or one
extending it. An operand kind names an addressing mode; an operation expands to instruction
forms whose operands are (KIND value...) items, parameters, or expressions. A
(:label NAME) form defines a label unique to each expansion.
Registers, modes and mnemonics are checked against the machine, and clause
heads are matched by name, so DEFBACKEND works from any package. Operations
named :push :pop :alloc :free :move :call :return :return-pop :enter and :leave
are the ones call lowering emits. See docs/backends.md and docs/conventions.md."
  (%definition-toplevel-form `(%define-backend ',name ',options ',clauses) `',name))
