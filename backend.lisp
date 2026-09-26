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
;;;; key, and checks the result against the child's machine. Redefining a
;;;; parent rebuilds its children, all or none (#330).

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
  frame       ; plist :GROWS :ALIGNMENT, and :SLOT, :STACK-SLOT (kind names) and :POINTER (a register name) when given
  operands    ; alist of (KIND-NAME . MODE-NAME)
  ops         ; alist of (OP-NAME PARAMS FORM...), names upcased
  op-effects  ; alist of (OP-NAME :PUSHES X :POPS Y) for the ops that declare a stack effect
  (branches t) ; upcased mnemonics that branch, or T when the backend does not say
  stack-writers ; entries listed as writing the stack pointer, on top of those their semantics show
  stack-writer-exceptions ; entries never taken to write it; an entry is an upcased mnemonic or (MNEMONIC MODE)
  parent      ; name of the backend extended, or NIL
  options     ; the DEFBACKEND options as given
  own-clauses ; the DEFBACKEND clauses as given, before merging with the parent's
  clauses)    ; the registers, call, frame, operands, ops, branches and stack-writers clauses after merging with the parent's

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

(defun %role-register (backend role fallback)
  (or (getf (backend-descriptor-registers backend) role) fallback))

(defun %stack-roles (backend)
  "The upcased stack pointer and program counter register names of BACKEND, or NIL for each it lacks."
  (let* ((descriptor (find-machine-descriptor (backend-descriptor-machine backend)))
         (pointers (loop for pointer being the hash-values of (machine-descriptor-stack-pointers descriptor)
                         collect (%designator-name (stack-pointer-descriptor-register pointer)))))
    (values (%role-register backend :stack-pointer (and (null (rest pointers)) (first pointers)))
            (%role-register backend :program-counter "PC"))))

(defun %variant-mode-name (variant)
  (let ((mode (instruction-descriptor-mode variant)))
    (and mode (%designator-name (mode-descriptor-name mode)))))

(defun %writer-entry-covers-p (entry name mode)
  "True when a stack-writers ENTRY covers the instruction NAME in MODE, a mode name or NIL."
  (if (consp entry)
      (and (string= (first entry) name) (equal (second entry) mode))
      (string= entry name)))

(defun %stack-writer-p (backend variant)
  "True when VARIANT of BACKEND's machine is a stack writer: listed by the backend, or its
semantics write the stack pointer but not the program counter under the CHOICE-CASE
clauses its own choices select."
  (let ((name (instruction-descriptor-name variant))
        (mode (%variant-mode-name variant)))
    (flet ((listed (entries) (some (lambda (entry) (%writer-entry-covers-p entry name mode)) entries))
           (writes (register)
             (and register
                  (some (lambda (write)
                          (and (string= (first write) register)
                               (%write-conditions-hold-p (rest write) variant)))
                        (instruction-descriptor-written-registers variant)))))
      (cond ((listed (backend-descriptor-stack-writer-exceptions backend)) nil)
            ((listed (backend-descriptor-stack-writers backend)) t)
            (t (multiple-value-bind (sp pc) (%stack-roles backend)
                 (and (writes sp) (not (writes pc)))))))))

(defun backend-stack-writers (backend)
  "One (MNEMONIC MODE...) for each instruction of BACKEND's machine with a variant that writes
the stack pointer, upcased and sorted; (MNEMONIC) when the variant has no addressing mode."
  (let* ((backend (find-backend backend))
         (descriptor (find-machine-descriptor (backend-descriptor-machine backend)))
         (result '()))
    (maphash (lambda (name variants)
               (let ((writers (remove-if-not (lambda (variant) (%stack-writer-p backend variant)) variants)))
                 (when writers
                   (cl:push (cons name (sort (remove-duplicates (remove nil (mapcar #'%variant-mode-name writers))
                                                                :test #'string=)
                                             #'string<))
                            result))))
             (machine-descriptor-instructions descriptor))
    (sort result #'string< :key #'first)))

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
    ("FRAME" :grows :alignment :slot :stack-slot :pointer))
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
        (stack-slot (getf args :stack-slot)) (pointer (getf args :pointer)))
    (unless (member grows '(nil :down :up))
      (%backend-error "frame :grows must be :down or :up, got ~S" grows))
    (unless (typep alignment '(integer 1))
      (%backend-error "frame :alignment must be a positive integer, got ~S" alignment))
    (loop for (what kind) in `((":slot" ,slot) (":stack-slot" ,stack-slot))
          when (and kind (not (%designator-name kind)))
            do (%backend-error "frame ~A: ~S is not a kind name" what kind))
    (append (list :grows grows :alignment alignment)
            (and slot (list :slot (%designator-name slot)))
            (and stack-slot (list :stack-slot (%designator-name stack-slot)))
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

(defparameter +backend-hook-arities+
  '(("PUSH" . 1) ("POP" . 1) ("ALLOC" . 1) ("FREE" . 1) ("MOVE" . 2) ("EXCHANGE" . 2) ("CALL" . 1) ("RETURN" . 0) ("RETURN-POP" . 1)
    ("ENTER" . 0) ("LEAVE" . 0))
  "Operations that convention lowering (items.lisp) emits, with their parameter counts.")

(defun %parse-op-effects (key names forms)
  "The (:PUSHES X :POPS Y) leading FORMS, and the forms after them."
  (let ((effects '()))
    (loop while (keywordp (first forms))
          do (let ((effect (first forms)) (value (second forms)))
               (unless (member effect '(:pushes :pops))
                 (%backend-error "ops: ~A: unknown option ~S; expected :pushes or :pops" key effect))
               (when (getf effects effect)
                 (%backend-error "ops: ~A: ~S given more than once" key effect))
               (unless (or (typep value '(integer 0)) (and (%designator-name value) (member (%designator-name value) names :test #'equal)))
                 (%backend-error "ops: ~A: ~S must be a non-negative integer or a parameter, got ~S" key effect value))
               (setf (getf effects effect) (if (integerp value) value (%designator-name value))
                     forms (cddr forms))))
    (values effects forms)))

(defun %parse-ops-clause (entries)
  "The parsed operations, and the alist of the stack effects some declare."
  (let (result effects)
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
            (multiple-value-bind (declared forms) (%parse-op-effects key names forms)
              (unless forms
                (%backend-error "ops: ~A has no instruction forms" key))
              (when (and declared (assoc key +backend-hook-arities+ :test #'string=))
                (%backend-error "ops: ~A is used by call lowering, which knows its stack effect" key))
              (when declared
                (cl:push (list* key declared) effects))
              (cl:push (list* key names forms) result))))))
    (values (nreverse result) (nreverse effects))))

(defun %parse-stack-writer-entries (head machine entries)
  "The stack-writers ENTRIES, each an upcased mnemonic or (MNEMONIC MODE), checked against MACHINE."
  (let ((result '()))
    (dolist (entry entries (nreverse result))
      (let ((parsed
              (if (consp entry)
                  (progn
                    (unless (and (= (length entry) 2) (second entry))
                      (%backend-error "~A: expected MNEMONIC or (MNEMONIC MODE), got ~S" head entry))
                    (let ((name (first (%parse-mnemonics-clause head machine (list (first entry)))))
                          (mode (%designator-name (second entry))))
                      (unless (and mode (find mode (find-instruction-variants machine name)
                                              :key #'%variant-mode-name :test #'equal))
                        (%backend-error "~A: ~A has no mode ~A" head name (second entry)))
                      (list name mode)))
                  (first (%parse-mnemonics-clause head machine (list entry))))))
        (when (find-if (lambda (other) (or (%entry-covers-entry-p other parsed) (%entry-covers-entry-p parsed other)))
                       result)
          (%backend-error "~A: ~A is listed twice" head (if (consp parsed) (first parsed) parsed)))
        (cl:push parsed result)))))

(defun %entry-covers-entry-p (outer inner)
  (%writer-entry-covers-p outer (if (consp inner) (first inner) inner) (and (consp inner) (second inner))))

(defun %parse-stack-writers-clause (descriptor machine entries)
  "Store the entries ENTRIES add and, after :except, remove in DESCRIPTOR."
  (let* ((split (position-if (lambda (entry) (and (keywordp entry) (string= (symbol-name entry) "EXCEPT"))) entries))
         (added (%parse-stack-writer-entries "stack-writers" machine (subseq entries 0 split)))
         (excepted (and split (%parse-stack-writer-entries "stack-writers :except" machine (subseq entries (1+ split)))))
         (both (find-if (lambda (entry)
                          (some (lambda (removed) (%entry-covers-entry-p removed entry)) excepted))
                        added)))
    (when both
      (%backend-error "stack-writers: ~A is both listed and excepted" (if (consp both) (first both) both)))
    (setf (backend-descriptor-stack-writers descriptor) added
          (backend-descriptor-stack-writer-exceptions descriptor) excepted)))

(defun %parse-mnemonics-clause (head machine entries)
  "The upcased mnemonics ENTRIES name, each an instruction of MACHINE; HEAD names the clause in errors."
  (let (result)
    (dolist (entry entries)
      (let ((key (%designator-name entry)))
        (unless key
          (%backend-error "~A: ~S is not a mnemonic" head entry))
        (when (member key result :test #'string=)
          (%backend-error "~A: ~A is listed twice" head key))
        (handler-case (find-instruction-variants machine key)
          (unknown-instruction ()
            (%backend-error "~A: machine ~S has no instruction ~A" head machine key)))
        (cl:push key result)))
    (nreverse result)))

;;; Checks run once every clause is known

(defun %check-backend-hooks (descriptor)
  (loop for (name . arity) in +backend-hook-arities+
        for entry = (assoc name (backend-descriptor-ops descriptor) :test #'string=)
        when (and entry (/= arity (length (second entry))))
          do (%backend-error "ops: ~A is used by call lowering and takes ~D parameter~:P, not ~D"
                             name arity (length (second entry)))))

(defun %check-backend-kinds (descriptor)
  (loop for (what kind) in `(("registers :operand" ,(getf (backend-descriptor-registers descriptor) :operand))
                             ("frame :slot" ,(getf (backend-descriptor-frame descriptor) :slot))
                             ("frame :stack-slot" ,(getf (backend-descriptor-frame descriptor) :stack-slot)))
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
        ((member head '("BRANCHES" "STACK-WRITERS") :test #'string=) child)
        ((member head '("OPERANDS" "OPS") :test #'string=)
         (cons (first child) (%merge-entries (rest parent) (rest child))))
        (t (let ((result (copy-list (rest parent))))
             (%check-plist head (rest child) (%clause-keys head))
             (loop for (key value) on (rest child) by #'cddr
                   do (setf (getf result key) value))
             (cons (first child) result)))))

(defvar *rebuilding-child* nil
  "True while a child is rebuilt for a redefined parent, when a without-ops name the parent lacks is dropped.")

(defun %without-op-missing (name op)
  (if *rebuilding-child*
      (warn 'stale-backend :message (format nil "DEFBACKEND ~S: without-ops names ~S, which the parent no longer defines"
                                            name op))
      (%backend-error "DEFBACKEND ~S: without-ops names ~S, which the parent does not define" name op)))

(defun %drop-ops (name ops without)
  "The ops clause OPS without the operations WITHOUT names."
  (dolist (op without)
    (unless (and (%designator-name op) (find (%designator-name op) (rest ops) :test #'equal :key #'%entry-key))
      (%without-op-missing name op)))
  (cons (first ops) (remove-if (lambda (entry) (member (%entry-key entry) without :test #'equal :key #'%designator-name))
                               (rest ops))))

(defun %merge-backend-clauses (name parent clauses)
  "PARENT's clauses (a descriptor's CLAUSES) with CLAUSES, a child's, merged in, in the parent's order."
  (let ((without (loop for clause in clauses
                       when (equal (%clause-head-name clause) "WITHOUT-OPS") append (rest clause)))
        (own (remove "WITHOUT-OPS" clauses :test #'equal :key #'%clause-head-name)))
    (flet ((find-clause (head list) (find head list :test #'equal :key #'%clause-head-name)))
      (let ((merged (loop for head in '("REGISTERS" "CALL" "FRAME" "OPERANDS" "OPS" "BRANCHES" "STACK-WRITERS")
                          for clause = (%merge-clause head (find-clause head parent) (find-clause head own))
                          when clause collect clause)))
        (if (and without (find-clause "OPS" merged))
            (substitute (%drop-ops name (find-clause "OPS" merged) without) (find-clause "OPS" merged) merged)
            (progn (dolist (op without)
                     (%without-op-missing name op))
                   merged))))))

(defvar *pending-backends* nil
  "Descriptors built for a redefinition and not yet registered, by upcased name.")

(defun %backend-descendants (name)
  "The upcased names of the backends extending NAME, directly or not, each after its parent."
  (let ((found '()) (frontier (list (%designator-name name))))
    (loop while frontier
          do (let ((next (loop for descriptor being the hash-values of *backends*
                               for key = (%designator-name (backend-descriptor-name descriptor))
                               when (and (backend-descriptor-parent descriptor)
                                         (member (%designator-name (backend-descriptor-parent descriptor)) frontier
                                                 :test #'equal)
                                         (not (member key found :test #'equal)))
                                 collect key)))
               (setf found (append found next)
                     frontier next)))
    found))

(defun %backend-options (name options)
  "The machine name and the parent descriptor (or NIL) DEFBACKEND's OPTIONS give."
  (unless (and (consp options) (evenp (length options)))
    (%backend-error "DEFBACKEND ~S: expected (:machine NAME) and/or (:extends PARENT) after the name, got ~S"
                    name options))
  (%check-plist (format nil "DEFBACKEND ~S" name) options '(:machine :extends))
  (let* ((parent-name (getf options :extends))
         (parent (and parent-name
                      (or (and (%designator-name parent-name)
                               (or (cdr (assoc (%designator-name parent-name) *pending-backends* :test #'equal))
                                   (gethash (%designator-name parent-name) *backends*)))
                          (%backend-error "DEFBACKEND ~S extends ~S, which has not been defined" name parent-name))))
         (given (getf options :machine))
         (machine (cond ((and given (%find-machine-name given)))
                        (given (%backend-error "DEFBACKEND ~S: machine ~S has not been defined" name given))
                        (parent (backend-descriptor-machine parent))
                        (t (%backend-error "DEFBACKEND ~S: expected (:machine NAME) after the name, got ~S"
                                           name options)))))
    (when (and parent (equal (%designator-name name) (%designator-name (backend-descriptor-name parent))))
      (%backend-error "DEFBACKEND ~S cannot extend itself" name))
    (when (and parent (member (%designator-name parent-name) (%backend-descendants name) :test #'equal))
      (%backend-error "DEFBACKEND ~S extends ~S, which extends ~S" name parent-name name))
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
        (unless (or (member head '("REGISTERS" "CALL" "FRAME" "OPERANDS" "OPS" "BRANCHES" "STACK-WRITERS") :test #'equal)
                    (and extendsp (equal head "WITHOUT-OPS")))
          (%backend-error "DEFBACKEND ~S: unknown clause ~S; expected registers, call, frame, operands, ops, branches, stack-writers~:[~; or without-ops~]"
                          name clause extendsp))))))

(defun %build-backend (name options clauses)
  "The descriptor DEFBACKEND's arguments describe, not yet registered."
  (multiple-value-bind (machine parent) (%backend-options name options)
    (%check-backend-clause-heads name clauses parent)
    (let* ((machine-descriptor (find-machine-descriptor machine))
           (own clauses)
           (clauses (if parent
                        (%merge-backend-clauses name (backend-descriptor-clauses parent) clauses)
                        clauses))
           (descriptor (make-backend-descriptor :name name :machine machine :frame (list :grows nil :alignment 1)
                                                :call (list :args :stack :order :right-to-left :cleanup :caller
                                                            :return-address-slots 1)
                                                :parent (and parent (backend-descriptor-name parent))
                                                :options options :own-clauses own :clauses clauses)))
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
                 (multiple-value-bind (ops effects) (%parse-ops-clause (rest clause))
                   (setf (backend-descriptor-ops descriptor) ops
                         (backend-descriptor-op-effects descriptor) effects)))
                ((equal head "BRANCHES")
                 (setf (backend-descriptor-branches descriptor) (%parse-mnemonics-clause "branches" machine (rest clause))))
                ((equal head "STACK-WRITERS")
                 (%parse-stack-writers-clause descriptor machine (rest clause))))))
      (%finish-backend-stack descriptor machine-descriptor)
      (%finish-backend-pointer descriptor)
      (%check-backend-kinds descriptor)
      (%check-backend-ops descriptor)
      (%check-backend-hooks descriptor)
      descriptor)))

(defun %define-backend (name options clauses)
  (%with-definition (name backend-definition-error)
    (unless (and (symbolp name) name)
      (%backend-error "DEFBACKEND: ~S is not a valid backend name" name))
    (let ((*pending-backends* '()) (built '()))
      (flet ((build (key name options clauses)
               (let ((descriptor (%build-backend name options clauses)))
                 (cl:push (cons key descriptor) *pending-backends*)
                 (cl:push (cons key descriptor) built))))
        (build (%designator-name name) name options clauses)
        (dolist (key (%backend-descendants name))
          (let ((old (gethash key *backends*)))
            (handler-case (let ((*rebuilding-child* t))
                            (build key (backend-descriptor-name old)
                                   (backend-descriptor-options old) (backend-descriptor-own-clauses old)))
              (backend-definition-error (c)
                (%backend-error "DEFBACKEND ~S: child backend ~S no longer builds: ~A"
                                name (backend-descriptor-name old) c))))))
      (dolist (entry (reverse built))
        (setf (gethash (car entry) *backends*) (cdr entry)))
      (cdr (first (last built))))))

(defmacro defbackend (name options &body clauses)
  "Define the compiler-target description NAME for a machine, from
OPTIONS, (:machine MACHINE) and/or (:extends PARENT), and CLAUSES, each one of:
     (registers [:return (reg...)] [:arguments (reg...)] [:scratch (reg...)]
                [:caller-saved (reg...)] [:callee-saved (reg...)]
                [:stack-pointer reg] [:program-counter reg] [:frame-pointer reg]
                [:operand kind])
     (call [:args :stack/(reg...)] [:order :left-to-right/:right-to-left]
           [:cleanup :caller/:callee] [:return-address-slots n])
     (frame [:grows :down/:up] [:alignment n] [:slot kind] [:stack-slot kind] [:pointer reg])
     (operands (KIND mode-name)...)
     (ops (NAME (param...) [:pushes n] [:pops n] (mnemonic operand...)...)...)
     (branches mnemonic...)
     (stack-writers [entry...] [:except entry...])   ; an entry is MNEMONIC or (MNEMONIC MODE)
     (without-ops NAME...)                    ; with :extends only
:extends merges PARENT's clauses under these by key, for the same machine or one
extending it. An operand kind names an addressing mode; an operation expands to instruction
forms whose operands are (KIND value...) items, parameters, or expressions. A
(:label NAME) form defines a label unique to each expansion. :pushes and :pops
declare the cells (an integer or a parameter) an operation puts on or takes off
the stack. The instructions in branches are the ones whose operands are
branch targets; without the clause every instruction is taken to branch. The
instruction variants whose semantics write the stack pointer but not the program counter
are stack writers, which call lowering rejects in a function whose stack depth it
tracks, judged by the variant an instruction's operands select; stack-writers adds
entries to them and :except removes some.
Registers, modes and mnemonics are checked against the machine, and clause
heads are matched by name, so DEFBACKEND works from any package. Operations
named :push :pop :alloc :free :move :call :return :return-pop :enter and :leave
are the ones call lowering emits. See docs/backends.md and docs/conventions.md."
  (%definition-toplevel-form `(%define-backend ',name ',options ',clauses) `',name))
