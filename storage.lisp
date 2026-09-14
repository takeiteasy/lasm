;;;; storage.lisp
;;;; Storage descriptors and runtime state for lasm machines.

(in-package #:lasm)

;;; Conditions

(define-condition lasm-error (error) ())

(define-condition storage-error (lasm-error)
  ((machine :initarg :machine :reader storage-error-machine)
   (name :initarg :name :reader storage-error-name)))

(define-condition unknown-storage (storage-error) ()
  (:report (lambda (c s)
             (format s "Unknown storage element ~S on machine ~S"
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition address-out-of-range (storage-error)
  ((address :initarg :address :reader address-out-of-range-address))
  (:report (lambda (c s)
             (format s "Address ~S out of range for memory ~S on machine ~S"
                     (address-out-of-range-address c)
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition stack-overflow (storage-error) ()
  (:report (lambda (c s)
             (format s "Stack overflow on ~S (machine ~S)"
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition stack-underflow (storage-error) ()
  (:report (lambda (c s)
             (format s "Stack underflow on ~S (machine ~S)"
                     (storage-error-name c) (storage-error-machine c)))))

;; #50: signalled by STACK-REF/(SETF STACK-REF) for an OFFSET outside the
;; stack's live region -- distinct from STACK-UNDERFLOW (which is specifically
;; "popped an empty stack") since an out-of-range indexed access is a
;; different program bug, e.g. reading three deep into a stack that only has
;; one live entry. Mirrors ADDRESS-OUT-OF-RANGE's shape.
(define-condition stack-index-out-of-range (storage-error)
  ((index :initarg :index :reader stack-index-out-of-range-index))
  (:report (lambda (c s)
             (format s "Stack index ~S out of range for stack ~S on machine ~S"
                     (stack-index-out-of-range-index c)
                     (storage-error-name c) (storage-error-machine c)))))

;; #13: signalled by REGREF/(SETF REGREF) for an INDEX outside a banked
;; register's [0, count) range. Mirrors STACK-INDEX-OUT-OF-RANGE's shape.
(define-condition register-index-out-of-range (storage-error)
  ((index :initarg :index :reader register-index-out-of-range-index))
  (:report (lambda (c s)
             (format s "Register index ~S out of range for register ~S on machine ~S"
                     (register-index-out-of-range-index c)
                     (storage-error-name c) (storage-error-machine c)))))

;; Generalized trap primitive placeholder. M6 replaces this with a full
;; interrupt/exception model (deftrap/definterrupt, vectors, priority);
;; for now `trap` just signals this condition with a tag and optional data.
(define-condition lasm-trap (lasm-error)
  ((tag :initarg :tag :reader lasm-trap-tag)
   (data :initarg :data :initform nil :reader lasm-trap-data))
  (:report (lambda (c s) (format s "Trap: ~S ~S" (lasm-trap-tag c) (lasm-trap-data c)))))

;; LASM-SYNTAX-ERROR, LEX-ERROR and PARSE-FAILURE (formerly defined here) now
;; live in diagnostic.lisp, loaded immediately after this file -- they moved
;; there to sit alongside DIAGNOSTIC-TEXT, their shared report renderer (#74).

;;; Storage element descriptors

(defstruct storage-element
  (name nil :type symbol)
  (kind nil :type (member :register :stack :memory :flag))
  (width nil :type (or null (integer 1)))
  ;; :count > 1 marks a banked/array register (e.g. CHIP8's V0-VF, #13). A
  ;; banked register allocates :count cells (MAKE-STORAGE-SLOT) and is
  ;; accessed by runtime index through REGREF/(SETF REGREF), not SREF --
  ;; SREF is scalar-only and errors on a banked element. WITH-MACHINE-
  ;; BINDINGS (semantics.lisp) binds a :count 1 register as a symbol-macro
  ;; but a :count > 1 register as a MACROLET expanding to REGREF, since
  ;; symbol-macrolet can't express an indexed form like (V x).
  (count 1 :type (integer 1))
  (depth nil :type (or null (integer 1)))       ; stacks
  (addr-width nil :type (or null (integer 1)))  ; memory
  (cell-width nil :type (or null (integer 1)))) ; memory, defaults to width

;; A machine-level fixed instruction-word bit layout (#20, M4): declared via
;; DEFMACHINE's (instruction-word :width n (field name width) ...) clause
;; (machine.lisp) for a DCPU-16-shaped machine whose whole instruction is one
;; WIDTH-bit word split into named bit fields rather than a cell-per-operand
;; stream. FIELDS is a list of (name width shift) in *declared* (most-
;; significant-first) order -- SHIFT is each field's bit offset from the
;; word's LSB, derived once here so encode/decode never recompute it.
;; WIDTH-CELLS is WIDTH/CELL-WIDTH, checked to be a whole number at parse
;; time (machine.lisp) since the word is emitted as CELL-WIDTH-wide,
;; little-endian cells (#53 -- the assembler pipeline is typed to the
;; target machine's own memory cell width, not fixed at 8 bits).
(defstruct instruction-word-layout
  (width nil :type (integer 1))
  (width-cells nil :type (integer 1))
  (cell-width nil :type (integer 1))
  (fields nil :type list))          ; (name width shift), MSB-first as declared

(defun instruction-word-field (layout name)
  "The (name width shift) entry in LAYOUT's FIELDS named NAME, or NIL."
  (find name (instruction-word-layout-fields layout) :key #'first))

(defstruct machine-descriptor
  (name nil :type symbol)
  (elements nil :type list)               ; ordered list of storage-element
  (table (make-hash-table :test 'eq))     ; name -> storage-element
  ;; Instruction registration (instruction.lisp). Keyed by upcased mnemonic
  ;; string and by opcode, so both the assembler (assembler.lisp, mnemonic ->
  ;; encoding) and the emulator (emulator.lisp, opcode -> decode) share one
  ;; table pair rather than each keeping its own index.
  ;; mnemonic string -> list of instruction-descriptor, one per addressing
  ;; mode the mnemonic accepts (mode.lisp/M2); a no-operand or single-mode
  ;; mnemonic's list has exactly one element.
  (instructions (make-hash-table :test 'equal))
  (opcodes (make-hash-table :test 'eql))          ; opcode -> instruction-descriptor
  ;; NIL for an ordinary byte-encoded machine (every machine before #20) --
  ;; DEFINSTRUCTION/the assembler/the emulator all branch on this being NIL
  ;; vs. an INSTRUCTION-WORD-LAYOUT to pick between the two encoding schemes.
  (instruction-word nil :type (or null instruction-word-layout))
  ;; #75: NIL unless DEFMACHINE declares a (clock-speed n) clause -- the
  ;; machine's nominal rate in Hz. NIL is what keeps cycle-accurate execution
  ;; a zero-cost opt-in subsystem (LASM-plan.md sec. 1, pillar 4):
  ;; RUN-FOR-DURATION requires this to be set (it has no other way to convert
  ;; cycles to seconds), while RUN-FOR-CYCLES and the plain cycle count on
  ;; MACHINE-CYCLES below need no clock speed at all.
  (clock-speed nil :type (or null (integer 1))))

(defun descriptor-element (descriptor name)
  (or (gethash name (machine-descriptor-table descriptor))
      (error 'unknown-storage :machine (machine-descriptor-name descriptor)
                               :name name)))

;; Registry of defined machine descriptors, keyed by machine name. Populated
;; by DEFMACHINE (see machine.lisp) inside an EVAL-WHEN so descriptors are
;; available at macroexpansion time -- this is what lets a later DEFINSTRUCTION
;; (M1) resolve storage names/widths for a machine defined earlier in the same
;; file, at compile time rather than only after loading.
(defvar *machines* (make-hash-table :test 'eq))

(defun find-machine-descriptor (name)
  (or (gethash name *machines*)
      (error "No machine named ~S has been defined with DEFMACHINE" name)))

;;; Runtime machine state

(defstruct (machine (:constructor %make-machine (descriptor)))
  (descriptor nil :type machine-descriptor)
  (slots (make-hash-table :test 'eq))     ; name -> slot representation
  ;; #75: total cycles consumed by every instruction STEP-MACHINE has
  ;; executed on this machine since the last RESET. Accumulated regardless of
  ;; whether the machine's descriptor declares a CLOCK-SPEED -- the count
  ;; itself is always meaningful, only the cycles-to-seconds conversion needs
  ;; one.
  (cycles 0 :type unsigned-byte))

;; Slot representations:
;;   :register / :flag -> a one-element (simple-vector 1) box holding an
;;                         unsigned integer
;;   :stack             -> a cons (vector . sp), vector is a fixed-size
;;                         simple-vector sized to :depth, not adjustable
;;   :memory            -> a (simple-array (unsigned-byte cell-width) (*))

(defun wrap-value (value width)
  "Mask VALUE to an unsigned WIDTH-bit integer."
  (logand value (1- (ash 1 width))))

(defun signed-value (value width)
  "Reinterpret unsigned WIDTH-bit VALUE as two's-complement signed."
  (if (logbitp (1- width) value)
      (- value (ash 1 width))
      value))

(defun make-storage-slot (element)
  (ecase (storage-element-kind element)
    (:register
     ;; :count cells -- 1 for an ordinary scalar register, more for a
     ;; banked register (#13, e.g. CHIP8's 16 V registers).
     (make-array (storage-element-count element) :initial-element 0))
    (:flag
     (make-array 1 :initial-element 0))
    (:stack
     (cons (make-array (storage-element-depth element) :initial-element 0)
           0))
    (:memory
     ;; Eager allocation of 2^addr-width cells. This is a deliberate M0
     ;; simplification kept behind this constructor: M5 introduces a
     ;; region-mapped/sparse memory backend (ROM/RAM/MMIO, banking) that
     ;; will replace this without touching MREF/accessor call sites.
     (let ((cell-width (or (storage-element-cell-width element)
                            (storage-element-width element))))
       (make-array (ash 1 (storage-element-addr-width element))
                   :element-type `(unsigned-byte ,cell-width)
                   :initial-element 0)))))

(defun make-machine (name)
  "Instantiate runtime state for the machine descriptor registered under NAME."
  (let ((descriptor (find-machine-descriptor name)))
    (let ((m (%make-machine descriptor)))
      (dolist (element (machine-descriptor-elements descriptor))
        (setf (gethash (storage-element-name element) (machine-slots m))
              (make-storage-slot element)))
      m)))

(defun reset (machine)
  "Zero all storage on MACHINE, including the #75 cycle counter -- which
lives on the MACHINE struct itself rather than as a storage element, so the
loop below (driven off MACHINE-DESCRIPTOR-ELEMENTS) never sees it and must
be told separately."
  (dolist (element (machine-descriptor-elements (machine-descriptor machine)))
    (let ((slot (gethash (storage-element-name element) (machine-slots machine))))
      (ecase (storage-element-kind element)
        ((:register :flag) (fill slot 0))
        (:stack (fill (car slot) 0) (setf (cdr slot) 0))
        (:memory (fill slot 0)))))
  (setf (machine-cycles machine) 0)
  machine)

;;; Accessors

(defun %slot (machine name kind)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (eq (storage-element-kind element) kind)
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun sref (machine name)
  "Read a scalar register or flag by NAME as an unsigned integer. Signals
UNKNOWN-STORAGE on a banked (:count > 1) register -- use REGREF instead."
  (multiple-value-bind (slot element) (%slot-any machine name)
    (declare (ignore element))
    (aref slot 0)))

(defun %slot-any (machine name)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (member (storage-element-kind element) '(:register :flag))
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    ;; SREF/(SETF SREF) are the scalar accessor -- a banked register (#13)
    ;; has no single cell 0 answer, so treat it as unaddressable by this
    ;; path rather than silently aliasing every index to one cell.
    (when (> (storage-element-count element) 1)
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun (setf sref) (value machine name)
  (multiple-value-bind (slot element) (%slot-any machine name)
    (setf (aref slot 0) (wrap-value value (storage-element-width element)))))

;; #13: indexed access into a banked (:count > 1) register, e.g. CHIP8's
;; V0-VF or DCPU-16's A/B/C/X/Y/Z/I/J. INDEX is evaluated at run time --
;; unlike STACK-REF's top-relative OFFSET, this is a plain 0-based bank
;; index (0 = the register's first element) since a banked register has no
;; notion of "top". Mirrors SREF/MREF's shape.
(defun regref (machine name index)
  "Read banked register NAME on MACHINE at bank INDEX as an unsigned integer."
  (multiple-value-bind (slot element) (%slot machine name :register)
    (unless (and (>= index 0) (< index (storage-element-count element)))
      (error 'register-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                           :name name :index index))
    (aref slot index)))

(defun (setf regref) (value machine name index)
  (multiple-value-bind (slot element) (%slot machine name :register)
    (unless (and (>= index 0) (< index (storage-element-count element)))
      (error 'register-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                           :name name :index index))
    (setf (aref slot index) (wrap-value value (storage-element-width element)))))

(defun flag (machine name)
  "Read a flag by NAME as 0 or 1."
  (multiple-value-bind (slot element) (%slot machine name :flag)
    (declare (ignore element))
    (aref slot 0)))

(defun (setf flag) (value machine name)
  ;; BUG (#22): VALUE is treated as a Lisp boolean here, not as an integer
  ;; 0/1 -- (setf (flag m 'z) 0) sets the flag to 1, since 0 is non-NIL.
  ;; Callers setting a flag from an integer (e.g. semantics reusing a
  ;; comparison result that happens to be 0 or 1) must pass an actual
  ;; boolean, e.g. (plusp n) rather than n itself.
  (multiple-value-bind (slot element) (%slot machine name :flag)
    (declare (ignore element))
    (setf (aref slot 0) (if value 1 0))))

(defun mref (machine name address)
  "Read memory element NAME on MACHINE at ADDRESS."
  (multiple-value-bind (slot element) (%slot machine name :memory)
    (declare (ignore element))
    (unless (and (>= address 0) (< address (length slot)))
      (error 'address-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                    :name name :address address))
    (aref slot address)))

(defun (setf mref) (value machine name address)
  (multiple-value-bind (slot element) (%slot machine name :memory)
    (unless (and (>= address 0) (< address (length slot)))
      (error 'address-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                    :name name :address address))
    (let ((cell-width (or (storage-element-cell-width element)
                           (storage-element-width element))))
      (setf (aref slot address) (wrap-value value cell-width)))))

(defun stack-push (machine name value)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (let ((vec (car slot)) (sp (cdr slot)))
      (when (>= sp (storage-element-depth element))
        (error 'stack-overflow :machine (machine-descriptor-name (machine-descriptor machine)) :name name))
      (setf (aref vec sp) (wrap-value value (storage-element-width element)))
      (setf (cdr slot) (1+ sp)))))

(defun stack-pop (machine name)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (let ((vec (car slot)) (sp (cdr slot)))
      (when (<= sp 0)
        (error 'stack-underflow :machine (machine-descriptor-name (machine-descriptor machine)) :name name))
      (let ((new-sp (1- sp)))
        (setf (cdr slot) new-sp)
        (aref vec new-sp)))))

(defun stack-depth (machine name)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (cdr slot)))

;; #50: indexed access into a stack, for a stack-relative addressing mode
;; (mode.lisp's STACK-RELATIVE) or any semantics body that needs to look past
;; the top without popping. OFFSET is top-relative and unsigned: 0 is the
;; top (the most recently pushed value, same as STACK-POP would return), 1 is
;; one below that, and so on -- Forth PICK / 65816 "n,S" convention. This
;; means the internal vector index is (- sp 1 offset), the mirror image of
;; how STACK-PUSH/STACK-POP already use SP. Bottom-relative indexing (index 0
;; = oldest entry) is still reachable by callers via STACK-DEPTH when wanted;
;; it just isn't OFFSET's own convention.
(defun stack-ref (machine name offset)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (let ((sp (cdr slot)))
      (unless (and (>= offset 0) (< offset sp))
        (error 'stack-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                          :name name :index offset))
      (aref (car slot) (- sp 1 offset)))))

(defun (setf stack-ref) (value machine name offset)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (let ((sp (cdr slot)))
      (unless (and (>= offset 0) (< offset sp))
        (error 'stack-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                          :name name :index offset))
      (setf (aref (car slot) (- sp 1 offset))
            (wrap-value value (storage-element-width element))))))
