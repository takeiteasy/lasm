;;;; backend.lisp
;;;; DEFBACKEND -- a declarative compiler-target description for a
;;;; machine: register roles, a calling convention, frame layout, the operand
;;;; kinds a front end may name (each an addressing mode) and primitive
;;;; operations expanding to instruction forms. items.lisp assembles programs
;;;; written against one.
;;;;
;;;; Clause heads, mnemonics, registers and modes are matched by name, so a
;;;; backend can be written in any package.
;;;;
;;;; (:extends PARENT) merges the parent's clauses under the child's, by
;;;; key, and checks the result against the child's machine. Redefining a
;;;; parent rebuilds its children, all or none.

(in-package #:lasm)

(define-condition backend-definition-error (definition-error) ())
(define-condition unknown-backend (lookup-error) ())

(defun %backend-error (control &rest args)
  (apply #'%definition-error 'backend-definition-error control args))

(defstruct backend-descriptor
  name        ; symbol
  isa         ; name of the ISA described
  cpu         ; name of the one CPU it is narrowed to, or NIL for every CPU of the ISA
  registers   ; plist of role -> upcased name, or list of names (see +BACKEND-REGISTER-ROLES+); :OPERAND -> kind name
  call        ; plist :ARGS :ORDER :CLEANUP :RETURN-ADDRESS-SLOTS
  frame       ; plist :GROWS :ALIGNMENT, and :SLOT, :STACK-SLOT (kind names), :POINTER (a register name) and :STATIC when given
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

(defun backend-descriptor-machine (backend)
  "The machine BACKEND assembles for by default: its CPU, else the CPU named like its ISA, else NIL."
  (or (backend-descriptor-cpu backend)
      (let ((isa (backend-descriptor-isa backend)))
        (and (gethash isa *machines*) isa))))

(defun %backend-storage (backend)
  "The descriptor BACKEND reads storage from: its CPU's, else its ISA's."
  (if (backend-descriptor-cpu backend)
      (find-machine-descriptor (backend-descriptor-cpu backend))
      (find-isa-descriptor (backend-descriptor-isa backend))))

(defun %backend-checking-name (backend)
  "The name whose instructions BACKEND's mnemonics are checked against."
  (or (backend-descriptor-cpu backend) (backend-descriptor-isa backend)))

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
  (let* ((descriptor (%backend-storage backend))
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
         (descriptor (%backend-storage backend))
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

;; A word is BACKEND-WORD-CELLS cells, the same split that a
;; (stack-pointer ... :width n) gives a stack slot -- the source language
;; (compiler.lisp) lays globals, DEFARRAY/DEFSTRING data and AREF/ASET
;; strides out by it.
(defun backend-cell-bytes (backend)
  "(VALUES BYTES ENDIAN): whole 8-bit characters a cell of BACKEND's machine
holds (at least 1), and the machine's memory endianness, which orders them
within a cell."
  (let* ((backend (find-backend backend))
         (descriptor (%backend-storage backend)))
    (values (max 1 (floor (%descriptor-cell-width descriptor) 8))
            (%descriptor-endian descriptor))))

(defun backend-words (backend)
  "BACKEND's register words, each (NAME (PART...) WIDTH) with an upcased name, its parts least
significant first and WIDTH the bit width of a part. A part is an upcased register name, or an
integer memory address."
  (getf (backend-descriptor-registers (find-backend backend)) :words))

(defun %word-name (word) (first word))
(defun %word-parts (word) (second word))
(defun %word-width (word) (third word))
(defun %word-count (word) (length (second word)))
(defun %word-part (word k) (nth k (second word)))

(defun %word-memory (descriptor)
  "The memory element a words backend's words live in: its stack pointer's, else the sole memory."
  (let ((pointer (loop for pointer being the hash-values of (machine-descriptor-stack-pointers descriptor)
                       return pointer)))
    (if pointer
        (descriptor-element descriptor (stack-pointer-descriptor-memory pointer))
        (let ((memories (remove-if-not (lambda (element) (eq (storage-element-kind element) :memory))
                                       (machine-descriptor-elements descriptor))))
          (unless (= (length memories) 1)
            (%backend-error "registers :words: address parts need one memory, or a stack pointer to name it"))
          (first memories)))))

(defun %word-cell-width (descriptor)
  "The cell width, in bits, of the memory a words backend's words live in."
  (let ((pointer (loop for pointer being the hash-values of (machine-descriptor-stack-pointers descriptor)
                       return pointer)))
    (if pointer
        (storage-element-cell-width (descriptor-element descriptor (stack-pointer-descriptor-memory pointer)))
        (%descriptor-cell-width descriptor))))

(defun backend-word-cells (backend)
  "Cells a word spans on BACKEND's machine: its parts' total width when the
backend declares words, else its declared stack pointer's slot width, which defaults to the
memory's own cell width, divided by that cell width and rounded up. 1 without a matching
(stack-pointer ...) clause."
  (let ((backend (find-backend backend)))
    (%descriptor-word-cells backend (%backend-storage backend))))

(defun %descriptor-word-cells (backend machine-descriptor)
  "BACKEND-WORD-CELLS for a backend descriptor that may not be registered yet."
  (let* ((sp (%role-register backend :stack-pointer nil))
         (pointer (%backend-matched-stack-pointer sp machine-descriptor))
         (words (getf (backend-descriptor-registers backend) :words)))
    (cond (words (ceiling (* (%word-count (first words)) (%word-width (first words)))
                          (%word-cell-width machine-descriptor)))
          (pointer (ceiling (stack-pointer-descriptor-width pointer)
                            (storage-element-cell-width
                             (descriptor-element machine-descriptor (stack-pointer-descriptor-memory pointer)))))
          (t 1))))

(defun %frame-push-shifted-p (frame)
  "True when FRAME records a :push order other than its :grows default, which moves every slot one cell."
  (and (getf frame :push) t))

;;; Resolution by name

(defun %find-machine-name (designator)
  (let ((key (%designator-name designator)))
    (and key
         (or (and (symbolp designator) (gethash designator *machines*) designator)
             (loop for name being the hash-keys of *machines*
                   when (string= key (%designator-name name)) return name)))))

(defun %find-isa-name (designator)
  (let ((key (%designator-name designator)))
    (and key
         (or (and (symbolp designator) (gethash designator *isas*) designator)
             (loop for name being the hash-keys of *isas*
                   when (string= key (%designator-name name)) return name)))))

(defun %mode-scope-of (name)
  "The ISA whose modes NAME, a CPU or an ISA, sees."
  (let ((cpu (gethash name *machines*)))
    (if cpu (machine-descriptor-isa cpu) name)))

(defun %find-mode-by-name (designator machine)
  (let ((key (%designator-name designator))
        (machine (%mode-scope-of machine)))
    (and key
         (or (and (symbolp designator) (%lookup-mode designator machine))
             (find key (%visible-modes machine) :test #'string=
                                                :key (lambda (mode) (%designator-name (mode-descriptor-name mode))))))))

(defun %find-mode-name (designator machine)
  (let ((mode (%find-mode-by-name designator machine)))
    (and mode (mode-descriptor-name mode))))

(defvar *backend-words* nil
  "The (NAME (PART...) WIDTH) register words of the backend being built, upcased names.")

(defun %backend-register-name (descriptor designator)
  "DESIGNATOR's upcased name, if it is a register or register alias of DESCRIPTOR's machine, or a register word."
  (let ((key (%designator-name designator)))
    (unless key
      (%backend-error "~S is not a register name" designator))
    (unless (or (assoc key *backend-words* :test #'string=)
                (nth-value 1 (gethash key (machine-descriptor-register-aliases descriptor)))
                (find-if (lambda (element)
                           (and (eq (storage-element-kind element) :register)
                                (string= key (%designator-name (storage-element-name element)))))
                         (machine-descriptor-elements descriptor)))
      (%backend-error "~A is not a register or register alias of machine ~S"
                      key (machine-descriptor-name descriptor)))
    key))

(defun %register-width (descriptor name)
  "The bit width of the machine register or alias NAME, an upcased name."
  (let ((element (or (gethash name (machine-descriptor-register-alias-elements descriptor))
                     (find-if (lambda (element)
                                (and (eq (storage-element-kind element) :register)
                                     (string= name (%designator-name (storage-element-name element)))))
                              (machine-descriptor-elements descriptor)))))
    (storage-element-width element)))

(defun %word-part-value (descriptor key part)
  "PART of word KEY: an integer memory address within the word memory, else an upcased register name."
  (cond ((not (integerp part))
         (%backend-register-name descriptor part))
        ((< -1 part (ash 1 (storage-element-addr-width (%word-memory descriptor))))
         part)
        (t (%backend-error "registers :words: ~A: ~S is not an address of memory ~A"
                           key part (storage-element-name (%word-memory descriptor))))))

(defun %word-part-width (descriptor part)
  "The bit width of word part PART: a register's, or the memory's cell for an address."
  (if (integerp part)
      (%word-cell-width descriptor)
      (%register-width descriptor part)))

(defun %parse-words (descriptor entries)
  "The (NAME (PART...) WIDTH) words the (registers :words ((NAME PART...)...)) ENTRIES declare,
each entry's parts most significant first and each word's parts stored least significant first. A
part is a register, or an integer address in memory."
  (unless (and (listp entries) (null (cdr (last entries))))
    (%backend-error "registers :words: expected a list of (NAME PART PART...), got ~S" entries))
  (let ((*backend-words* nil) (result '()))
    (dolist (entry entries (nreverse result))
      (unless (and (consp entry) (null (cdr (last entry))) (>= (length entry) 3))
        (%backend-error "registers :words: expected (NAME PART PART...), got ~S" entry))
      (destructuring-bind (name &rest parts) entry
        (let ((key (%designator-name name)))
          (unless key
            (%backend-error "registers :words: ~S is not a word name" name))
          (when (or (assoc key result :test #'string=)
                    (ignore-errors (%backend-register-name descriptor name)))
            (%backend-error "registers :words: ~A is already a register, alias or word" key))
          (when (and result (/= (length parts) (%word-count (first result))))
            (%backend-error "registers :words: ~A has ~D parts, not ~D like ~A"
                            key (length parts) (%word-count (first result)) (%word-name (first result))))
          (let* ((address (integerp (first parts)))
                 (parts (mapcar (lambda (part) (%word-part-value descriptor key part)) parts)))
            (unless (every (lambda (part) (eq address (integerp part))) parts)
              (%backend-error "registers :words: ~A mixes a register and a memory address; all parts are registers or all are addresses" key))
            (when (and result (not (eq address (integerp (first (%word-parts (first result)))))))
              (%backend-error "registers :words: ~A and ~A differ in kind; every word is registers or every word is addresses"
                              (%word-name (first result)) key))
            (loop for (part . rest) on parts
                  when (member part rest :test #'equal)
                    do (%backend-error "registers :words: ~A uses ~A for two parts" key part))
            (dolist (part parts)
              (let ((other (find-if (lambda (word) (member part (%word-parts word) :test #'equal)) result)))
                (when other
                  (%backend-error "registers :words: ~A is a part of both ~A and ~A" part (%word-name other) key))))
            (let ((width (%word-part-width descriptor (first parts))))
              (unless (every (lambda (part) (eql width (%word-part-width descriptor part))) parts)
                (%backend-error "registers :words: the parts of ~A are not the same width" key))
              (cl:push (list key (reverse parts) width) result))))))))

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

(defparameter +backend-register-singles+ '(:stack-pointer :program-counter :frame-pointer :address)
  "Register roles holding one register.")

(defparameter +backend-clause-keys+
  `(("REGISTERS" ,@+backend-register-roles+ ,@+backend-register-singles+ :operand :words)
    ("CALL" :args :order :cleanup :return-address-slots :return-address-cells)
    ("FRAME" :grows :alignment :slot :stack-slot :label-slot :pointer :pointer-cells :offsets :counts :static))
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
                     ((eq role :words) *backend-words*)
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
        (slots (getf args :return-address-slots 1))
        (cells (getf args :return-address-cells)))
    (unless (or (eq arguments :stack) (listp arguments))
      (%backend-error "call :args must be :stack or a list of registers, got ~S" arguments))
    (unless (member order '(:left-to-right :right-to-left))
      (%backend-error "call :order must be :left-to-right or :right-to-left, got ~S" order))
    (unless (member cleanup '(:caller :callee))
      (%backend-error "call :cleanup must be :caller or :callee, got ~S" cleanup))
    (unless (typep slots '(integer 0))
      (%backend-error "call :return-address-slots must be a non-negative integer, got ~S" slots))
    (unless (typep cells '(or null (integer 0)))
      (%backend-error "call :return-address-cells must be a non-negative integer, got ~S" cells))
    (append (list :args (if (eq arguments :stack)
                            :stack
                            (mapcar (lambda (register) (%backend-register-name descriptor register)) arguments))
                  :order order :cleanup cleanup :return-address-slots slots)
            (and cells (list :return-address-cells cells)))))

(defun %parse-frame-clause (descriptor args)
  (%check-plist "frame" args (%clause-keys "FRAME"))
  (let ((grows (getf args :grows)) (alignment (getf args :alignment 1)) (slot (getf args :slot))
        (stack-slot (getf args :stack-slot)) (label-slot (getf args :label-slot))
        (pointer (getf args :pointer)) (pointer-cells (getf args :pointer-cells))
        (offsets (getf args :offsets :slots)) (counts (getf args :counts :slots))
        (static (getf args :static)))
    (unless (member static '(nil t))
      (%backend-error "frame :static must be t or nil, got ~S" static))
    (loop for (what unit) in `((":offsets" ,offsets) (":counts" ,counts))
          unless (member unit '(:slots :cells))
            do (%backend-error "frame ~A must be :slots or :cells, got ~S" what unit))
    (unless (member grows '(nil :down :up))
      (%backend-error "frame :grows must be :down or :up, got ~S" grows))
    (unless (typep alignment '(integer 1))
      (%backend-error "frame :alignment must be a positive integer, got ~S" alignment))
    (unless (typep pointer-cells '(or null (integer 0)))
      (%backend-error "frame :pointer-cells must be a non-negative integer, got ~S" pointer-cells))
    (loop for (what kind) in `((":slot" ,slot) (":stack-slot" ,stack-slot) (":label-slot" ,label-slot))
          when (and kind (not (%designator-name kind)))
            do (%backend-error "frame ~A: ~S is not a kind name" what kind))
    (append (list :grows grows :alignment alignment)
            (and slot (list :slot (%designator-name slot)))
            (and stack-slot (list :stack-slot (%designator-name stack-slot)))
            (and label-slot (list :label-slot (%designator-name label-slot)))
            (and pointer (list :pointer (%backend-register-name descriptor pointer)))
            (and pointer-cells (list :pointer-cells pointer-cells))
            (and (eq offsets :cells) (list :offsets :cells))
            (and (eq counts :cells) (list :counts :cells))
            (and static (list :static t)))))

(defun %parse-operands-clause (machine entries)
  (let (result)
    (dolist (entry entries)
      (%definition-bind (kind mode) entry
        (let ((key (%designator-name kind)))
          (unless key
            (%backend-error "operands: ~S is not a kind name" kind))
          (when (%expression-operator kind)
            (%backend-error "operands: ~A is an expression operator, not a kind name" key))
          (when (member key *function-operator-keywords* :test #'string-equal)
            (%backend-error "operands: ~A is a function operator (~(~A~)(...)), not a kind name" key key))
          (when (assoc key result :test #'string=)
            (%backend-error "operands: kind ~A is declared twice" key))
          (let ((mode-name (%find-mode-name mode machine)))
            (unless mode-name
              (%backend-error "operands: kind ~A names ~S, which is not an addressing mode visible to machine ~S"
                              key mode machine))
            (cl:push (cons key mode-name) result)))))
    (nreverse result)))

(defparameter +backend-hook-arities+
  '(("PUSH" . 1) ("POP" . 1) ("ALLOC" . 1) ("FREE" . 1) ("MOVE" . 2) ("EXCHANGE" . 2) ("CALL" . 1) ("RETURN" . 0) ("RETURN-POP" . 1) ("RETURN-INTERRUPT" . 0)
    ("ENTER" . 0) ("LEAVE" . 0))
  "Operations that convention lowering (items.lisp) emits, with their parameter counts.")

(defparameter +backend-comparison-ops+ '("EQ" "NE" "LT" "GT" "LE" "GE")
  "The comparisons; each also has a BRANCH- form.")

(defparameter +backend-binary-ops+
  (append '("ADD" "SUB" "MUL" "DIV" "MOD" "AND" "OR" "XOR" "SHL" "SHR") +backend-comparison-ops+)
  "The arithmetic and comparison operations; each may have -IMM, -SLOT and -LABEL variants.")

(defparameter +backend-language-op-arities+
  (append '(("CONST" . 2) ("GET" . 2) ("SET" . 2) ("PEEK" . 2) ("POKE" . 2)
            ("PEEK-LABEL" . 2) ("POKE-LABEL" . 2)
            ("POINT" . 1) ("POINT-LABEL" . 1) ("PEEK-POINTER" . 1) ("POKE-POINTER" . 1)
            ("PEEK-BYTE" . 2) ("POKE-BYTE" . 2) ("BYTE-ADDRESS" . 1)
            ("PEEK-BYTE-POINTER" . 1) ("POKE-BYTE-POINTER" . 1)
            ("JUMP" . 1) ("BRANCH-ZERO" . 2) ("HALT" . 0))
          (loop for name in +backend-binary-ops+
                collect (cons name 2)
                collect (cons (concatenate 'string name "-IMM") 2)
                collect (cons (concatenate 'string name "-SLOT") 2)
                collect (cons (concatenate 'string name "-LABEL") 2))
          (loop for name in +backend-comparison-ops+
                for branch = (concatenate 'string "BRANCH-" name)
                collect (cons branch 3)
                collect (cons (concatenate 'string branch "-IMM") 3)
                collect (cons (concatenate 'string branch "-SLOT") 3)
                collect (cons (concatenate 'string branch "-LABEL") 3)))
  "Operations that the language compiler (compiler.lisp) emits, with their parameter counts.")

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

(defun %parse-param-specs (key params)
  "(VALUES NAMES KINDS) for PARAMS: each item a name, or (NAME KIND) restricting
NAME to an operand of the declared kind KIND -- several clauses for one
operation, tried in order, let a backend template a call target or other
operand differently by kind, such as a register versus a label."
  (let (names kinds)
    (dolist (spec params)
      (if (and (consp spec) (= (length spec) 2))
          (let ((name (%designator-name (first spec))) (kind (%designator-name (second spec))))
            (unless (and name kind)
              (%backend-error "ops: ~A: ~S is not (NAME KIND)" key spec))
            (cl:push name names)
            (cl:push kind kinds))
          (let ((name (%designator-name spec)))
            (unless name
              (%backend-error "ops: ~A: ~S is not a parameter name or (NAME KIND)" key spec))
            (cl:push name names)
            (cl:push nil kinds))))
    (values (nreverse names) (nreverse kinds))))

(defun %parse-ops-clause (entries)
  "The parsed operations, and the alist of the stack effects some declare."
  (let (result effects)
    (dolist (entry entries)
      (%definition-bind (name params &rest forms) entry
        (let ((key (%designator-name name)))
          (unless key
            (%backend-error "ops: ~S is not an operation name" name))
          (unless (listp params)
            (%backend-error "ops: ~A parameters must be a list, got ~S" key params))
          (multiple-value-bind (names kinds) (%parse-param-specs key params)
            (unless (= (length names) (length (remove-duplicates names :test #'string=)))
              (%backend-error "ops: ~A repeats a parameter" key))
            (when (find-if (lambda (other) (and (equal (first other) key) (equal (third other) kinds)))
                            result)
              (%backend-error "ops: ~A is declared twice~:[~; for the same operand kinds~]"
                               key (some #'identity kinds)))
            (multiple-value-bind (declared forms) (%parse-op-effects key names forms)
              (unless forms
                (%backend-error "ops: ~A has no instruction forms" key))
              (when (and declared (assoc key +backend-hook-arities+ :test #'string=))
                (%backend-error "ops: ~A is used by call lowering, which knows its stack effect" key))
              (when declared
                (cl:push (list* key declared) effects))
              (cl:push (list* key names kinds forms) result))))))
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
  (loop for (name . arity) in (append +backend-hook-arities+ +backend-language-op-arities+)
        do (dolist (entry (backend-descriptor-ops descriptor))
             (when (and (equal (first entry) name) (/= arity (length (second entry))))
               (%backend-error "ops: ~A is used by call lowering or the language compiler and takes ~D parameter~:P, not ~D"
                               name arity (length (second entry)))))))

(defun %check-backend-address (descriptor)
  "A backend with an :address register defines the operations that load and use it."
  (let ((address (getf (backend-descriptor-registers descriptor) :address)))
    (when address
      (dolist (name '("POINT" "PEEK-POINTER" "POKE-POINTER"))
        (unless (assoc name (backend-descriptor-ops descriptor) :test #'string=)
          (%backend-error "registers :address ~A needs the operation ~(~A~)" address name)))
      (when (member address (getf (backend-descriptor-registers descriptor) :return) :test #'string=)
        (%backend-error "registers :address ~A is also in :return" address)))))

(defun %check-backend-kinds (descriptor)
  (loop for (what kind) in `(("registers :operand" ,(getf (backend-descriptor-registers descriptor) :operand))
                             ("frame :slot" ,(getf (backend-descriptor-frame descriptor) :slot))
                             ("frame :stack-slot" ,(getf (backend-descriptor-frame descriptor) :stack-slot))
                             ("frame :label-slot" ,(getf (backend-descriptor-frame descriptor) :label-slot)))
        when (and kind (not (assoc kind (backend-descriptor-operands descriptor) :test #'string=)))
          do (%backend-error "~A: ~A is not a declared operand kind~:[~;; with :static t and no slot operand, drop :slot~]"
                             what kind (and (equal what "frame :slot") (getf (backend-descriptor-frame descriptor) :static)))))

(defun %part-form-p (form)
  "True when FORM is (:hi X), (:lo X) or (:part K X): a part of a register word."
  (and (consp form) (keywordp (first form))
       (let ((name (symbol-name (first form))))
         (cond ((member name '("HI" "LO") :test #'string=) (and (consp (cdr form)) (null (cddr form))))
               ((string= name "PART") (and (consp (cdr form)) (consp (cddr form)) (null (cdddr form))))))))

(defun %part-form-target (form)
  "The word FORM, a part form, takes a part of."
  (car (last form)))

(defun %part-form-index (form count)
  "The index, from the least significant part, that FORM, a part form of a COUNT-part word, names."
  (let ((name (symbol-name (first form))))
    (cond ((string= name "LO") 0)
          ((string= name "HI") (1- count))
          (t (second form)))))

(defun %check-part-form (op form)
  (unless *backend-words*
    (%backend-error "ops: ~A: ~S needs (registers :words ...)" op form))
  (let ((count (%word-count (first *backend-words*)))
        (index (%part-form-index form (%word-count (first *backend-words*)))))
    (unless (and (integerp index) (< -1 index count))
      (%backend-error "ops: ~A: ~S: the part must be an integer from 0 to ~D" op form (1- count)))))

(defun %check-hole-value (op value params)
  (when (%part-form-p value)
    (%check-part-form op value)
    (return-from %check-hole-value (%check-hole-value op (%part-form-target value) params)))
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
    (when (%part-form-p operand)
      (%check-part-form op operand)
      (return-from %check-op-operand
        (%check-op-operand op (%part-form-target operand) params kinds machine labels)))
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
    (destructuring-bind (op params kinds &rest forms) entry
      (loop for name in params for kind in kinds
            when (and kind (not (assoc kind (backend-descriptor-operands descriptor) :test #'string=)))
              do (%backend-error "ops: ~A: parameter ~A's kind ~A is not a declared operand kind" op name kind))
      (let ((labels (%template-labels op params forms)))
        (dolist (form forms)
          (%check-op-form op form params (backend-descriptor-operands descriptor)
                          (%backend-checking-name descriptor) labels))))))

(defun %backend-matched-stack-pointer (sp machine-descriptor)
  "The (stack-pointer ...) descriptor of MACHINE-DESCRIPTOR whose register is
SP, an upcased name, or NIL when SP is NIL or names none."
  (and sp (find sp (loop for pointer being the hash-values of (machine-descriptor-stack-pointers machine-descriptor)
                         collect pointer)
                :test #'string=
                :key (lambda (pointer) (%designator-name (stack-pointer-descriptor-register pointer))))))

(defun %finish-backend-stack (descriptor machine-descriptor)
  "Check the backend's stack pointer and frame direction against the machine's
declared (stack-pointer ...), default the frame direction from it, and record its push order."
  (let* ((registers (backend-descriptor-registers descriptor))
         (sp (getf registers :stack-pointer))
         (declared (loop for pointer being the hash-values of (machine-descriptor-stack-pointers machine-descriptor)
                         collect pointer))
         (match (%backend-matched-stack-pointer sp machine-descriptor))
         (grows (getf (backend-descriptor-frame descriptor) :grows)))
    (when (and sp declared (not match))
      (%backend-error "registers :stack-pointer ~A is not the machine's declared stack-pointer (~{~A~^, ~})"
                      sp (mapcar #'stack-pointer-descriptor-register declared)))
    (when (and match grows (not (eq grows (stack-pointer-descriptor-grows match))))
      (%backend-error "frame :grows ~S disagrees with the machine's (stack-pointer ~A :grows ~S)"
                      grows sp (stack-pointer-descriptor-grows match)))
    (let ((frame (backend-descriptor-frame descriptor)))
      (setf (getf frame :grows) (or grows (and match (stack-pointer-descriptor-grows match)) :down))
      (when (and match (not (eq (stack-pointer-descriptor-push match)
                                (if (eq (getf frame :grows) :down) :pre :post))))
        (setf (getf frame :push) (stack-pointer-descriptor-push match)))
      (setf (backend-descriptor-frame descriptor) frame)
      (when (and (%frame-push-shifted-p frame)
                 (not (eq (getf frame :offsets) :cells))
                 (> (%descriptor-word-cells descriptor machine-descriptor) 1))
        (%backend-error "frame :offsets must be :cells: the machine's (stack-pointer ~A :push ~S) moves a slot ~
one cell, which a ~D-cell slot offset cannot say"
                        sp (getf frame :push) (%descriptor-word-cells descriptor machine-descriptor))))))

;;; Register words

(defun %check-return-address-cells (descriptor)
  "Check that a backend with :return-address-cells counts its frame in cells."
  (when (and (getf (backend-descriptor-call descriptor) :return-address-cells)
             (not (eq (getf (backend-descriptor-frame descriptor) :offsets) :cells)))
    (%backend-error "call :return-address-cells needs (frame :offsets :cells)")))

(defun %finish-backend-words (descriptor machine-descriptor)
  "Check that a backend with register words holds every operand in a word, and that the words
fit the machine's memory and stack."
  (let ((words (getf (backend-descriptor-registers descriptor) :words)))
    (when words
      (let* ((names (mapcar #'%word-name words))
             (width (%word-width (first words)))
             (cell (%word-cell-width machine-descriptor))
             (registers (backend-descriptor-registers descriptor))
             (call-args (getf (backend-descriptor-call descriptor) :args)))
        (loop for (role list) in `((":return" ,(getf registers :return))
                                   (":arguments" ,(getf registers :arguments))
                                   (":scratch" ,(getf registers :scratch))
                                   (":caller-saved" ,(getf registers :caller-saved))
                                   (":callee-saved" ,(getf registers :callee-saved))
                                   ("call :args" ,(and (listp call-args) call-args)))
              do (dolist (name list)
                   (unless (member name names :test #'string=)
                     (%backend-error "registers ~A: ~A is not a register word; a backend with words holds values only in words"
                                     role name))))
        (unless (getf registers :return)
          (%backend-error "registers :words: :return must name a word, the accumulator"))
        (dolist (word (rest words))
          (unless (= width (%word-width word))
            (%backend-error "registers :words: ~A has ~D-bit parts, not ~D like ~A"
                            (%word-name word) (%word-width word) width (%word-name (first words)))))
        (unless (member (%descriptor-endian machine-descriptor) '(:little :big))
          (%backend-error "registers :words: the machine's :endian must be :little or :big"))
        (when (getf (backend-descriptor-frame descriptor) :slot)
          (unless (= width cell)
            (%backend-error "registers :words: a word's part is ~D bits but memory cells are ~D; a frame slot needs one cell a part"
                            width cell))
          (unless (and (eq (getf (backend-descriptor-frame descriptor) :offsets) :cells)
                       (eq (getf (backend-descriptor-frame descriptor) :counts) :cells))
            (%backend-error "registers :words: (frame :slot ...) needs :offsets :cells and :counts :cells, since a word spans several cells")))))))

;;; The frame pointer

(defun %finish-backend-pointer (descriptor)
  "Reconcile (frame :pointer REG) with (registers :frame-pointer REG), and check the pointer is
not another register the convention uses."
  (let* ((pointer (getf (backend-descriptor-frame descriptor) :pointer))
         (registers (backend-descriptor-registers descriptor))
         (role (getf registers :frame-pointer)))
    (when (getf (backend-descriptor-frame descriptor) :pointer-cells)
      (unless pointer
        (%backend-error "frame :pointer-cells needs frame :pointer"))
      (unless (eq (getf (backend-descriptor-frame descriptor) :offsets) :cells)
        (%backend-error "frame :pointer-cells needs (frame :offsets :cells)")))
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
  "PARENT's entries with CHILD's in place of those of the same name, then CHILD's new
ones. Several PARENT or CHILD entries may share a name -- an OPS clause's operand-kind
clauses -- and CHILD's whole group for a name replaces PARENT's whole group, at
its first position."
  (let ((child-keys (remove-duplicates (mapcar #'%entry-key child) :test #'equal))
        (result '()) (done '()))
    (dolist (entry parent)
      (let ((key (%entry-key entry)))
        (cond ((member key child-keys :test #'equal)
               (unless (member key done :test #'equal)
                 (dolist (new child) (when (equal (%entry-key new) key) (cl:push new result)))
                 (cl:push key done)))
              (t (cl:push entry result)))))
    (dolist (entry child)
      (let ((key (%entry-key entry)))
        (unless (member key done :test #'equal)
          (dolist (new child) (when (equal (%entry-key new) key) (cl:push new result)))
          (cl:push key done))))
    (nreverse result)))

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
  "The ISA name, CPU name (or NIL) and parent descriptor (or NIL) DEFBACKEND's OPTIONS give."
  (unless (and (consp options) (evenp (length options)))
    (%backend-error "DEFBACKEND ~S: expected (:isa NAME), optionally (:cpu NAME), and/or (:extends PARENT) after the name, got ~S"
                    name options))
  (%check-plist (format nil "DEFBACKEND ~S" name) options '(:isa :cpu :extends))
  (let* ((parent-name (getf options :extends))
         (parent (and parent-name
                      (or (and (%designator-name parent-name)
                               (or (cdr (assoc (%designator-name parent-name) *pending-backends* :test #'equal))
                                   (gethash (%designator-name parent-name) *backends*)))
                          (%backend-error "DEFBACKEND ~S extends ~S, which has not been defined" name parent-name))))
         (given-cpu (getf options :cpu))
         (given-isa (getf options :isa))
         (cpu (cond ((and given-cpu (%find-machine-name given-cpu)))
                    (given-cpu (%backend-error "DEFBACKEND ~S: CPU ~S has not been defined" name given-cpu))
                    (parent (backend-descriptor-cpu parent))))
         (isa (cond ((and given-isa (%find-isa-name given-isa)))
                    (given-isa (%backend-error "DEFBACKEND ~S: ISA ~S has not been defined" name given-isa))
                    (cpu (machine-descriptor-isa (find-machine-descriptor cpu)))
                    (parent (backend-descriptor-isa parent))
                    (t (%backend-error "DEFBACKEND ~S: expected (:isa NAME) after the name, got ~S"
                                       name options)))))
    (when (and parent (equal (%designator-name name) (%designator-name (backend-descriptor-name parent))))
      (%backend-error "DEFBACKEND ~S cannot extend itself" name))
    (when (and parent (member (%designator-name parent-name) (%backend-descendants name) :test #'equal))
      (%backend-error "DEFBACKEND ~S extends ~S, which extends ~S" name parent-name name))
    (when cpu
      (let ((cpu-isa (machine-descriptor-isa (find-machine-descriptor cpu))))
        (unless (or (eq cpu-isa isa) (member isa (%isa-ancestors cpu-isa)))
          (%backend-error "DEFBACKEND ~S: CPU ~S is built on ISA ~S, not ~S or an ISA extending it"
                          name cpu cpu-isa isa))))
    (when (and parent (not (eq isa (backend-descriptor-isa parent)))
               (not (member (backend-descriptor-isa parent) (%isa-ancestors isa))))
      (%backend-error "DEFBACKEND ~S: ISA ~S is not ~S or an ISA extending it, which ~S targets"
                      name isa (backend-descriptor-isa parent) (backend-descriptor-name parent)))
    (when (and parent (backend-descriptor-cpu parent) cpu (not (eq cpu (backend-descriptor-cpu parent)))
               (not (member (backend-descriptor-cpu parent) (%machine-ancestors cpu))))
      (%backend-error "DEFBACKEND ~S: CPU ~S is not ~S or a CPU extending it, which ~S targets"
                      name cpu (backend-descriptor-cpu parent) (backend-descriptor-name parent)))
    (values isa cpu parent)))

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
  (multiple-value-bind (isa cpu parent) (%backend-options name options)
    (%check-backend-clause-heads name clauses parent)
    (let* ((machine (or cpu isa))
           (machine-descriptor (if cpu (find-machine-descriptor cpu) (find-isa-descriptor isa)))
           (own clauses)
           (clauses (if parent
                        (%merge-backend-clauses name (backend-descriptor-clauses parent) clauses)
                        clauses))
           (*backend-words* (let ((words (getf (rest (find "REGISTERS" clauses :test #'equal :key #'%clause-head-name))
                                               :words)))
                              (and words (%parse-words machine-descriptor words))))
           (descriptor (make-backend-descriptor :name name :isa isa :cpu cpu :frame (list :grows nil :alignment 1)
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
      (%check-return-address-cells descriptor)
      (%finish-backend-words descriptor machine-descriptor)
      (%finish-backend-pointer descriptor)
      (%check-backend-kinds descriptor)
      (%check-backend-ops descriptor)
      (%check-backend-hooks descriptor)
      (%check-backend-address descriptor)
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
OPTIONS, (:isa ISA), optionally (:cpu CPU), and/or (:extends PARENT), and CLAUSES, each one of:
     (registers [:return (reg...)] [:arguments (reg...)] [:scratch (reg...)]
                [:caller-saved (reg...)] [:callee-saved (reg...)]
                [:stack-pointer reg] [:program-counter reg] [:frame-pointer reg] [:address reg]
                [:operand kind] [:words ((NAME PART...)...)])
     (call [:args :stack/(reg...)] [:order :left-to-right/:right-to-left]
           [:cleanup :caller/:callee] [:return-address-slots n/:return-address-cells n])
     (frame [:grows :down/:up] [:alignment n] [:slot kind] [:stack-slot kind] [:label-slot kind]
            [:pointer reg] [:pointer-cells n] [:offsets :slots/:cells] [:counts :slots/:cells] [:static t/nil])
     (operands (KIND mode-name)...)
     (ops (NAME (param...) [:pushes n] [:pops n] (mnemonic operand...)...)...)
       ; a param is a name, or (NAME KIND) restricting it to an operand of that
       ; declared kind; NAME may repeat across several clauses of one
       ; operation, tried in the order written, the first whose arguments
       ; match its params winning
     (branches mnemonic...)
     (stack-writers [entry...] [:except entry...])   ; an entry is MNEMONIC or (MNEMONIC MODE)
     (without-ops NAME...)                    ; with :extends only
:extends merges PARENT's clauses under these by key, for the same ISA or one
extending it. :cpu narrows the backend to one machine of the ISA, whose removed
instructions its templates may not use. An operand kind names an addressing mode; an operation expands to instruction
forms whose operands are (KIND value...) items, parameters, or expressions. A
(:label NAME) form defines a label unique to each expansion. :pushes and :pops
declare the cells (an integer or a parameter) an operation puts on or takes off
the stack. The instructions in branches are the ones whose operands are
branch targets; without the clause every instruction is taken to branch. The
instruction variants whose semantics write the stack pointer but not the program counter
are stack writers, which call lowering rejects in a function whose stack depth it
tracks, judged by the variant an instruction's operands select; stack-writers adds
entries to them and :except removes some.
:words names register words that hold a word, each of two or more parts, most significant
first; every role list then names words, and an operation reaches a word's parts with
(:part K X), K from 0 at the least significant, or (:lo X) and (:hi X) for the ends, see
docs/register-words.md.
Registers, modes and mnemonics are checked against the machine, and clause
heads are matched by name, so DEFBACKEND works from any package. Operations
named :push :pop :alloc :free :move :call :return :return-pop :enter and :leave
are the ones call lowering emits. See docs/backends.md and docs/conventions.md."
  (%definition-toplevel-form `(%define-backend ',name ',options ',clauses) `',name))
