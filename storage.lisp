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

;; Generalized trap primitive placeholder. M6 replaces this with a full
;; interrupt/exception model (deftrap/definterrupt, vectors, priority);
;; for now `trap` just signals this condition with a tag and optional data.
(define-condition lasm-trap (lasm-error)
  ((tag :initarg :tag :reader lasm-trap-tag)
   (data :initarg :data :initform nil :reader lasm-trap-data))
  (:report (lambda (c s) (format s "Trap: ~S ~S" (lasm-trap-tag c) (lasm-trap-data c)))))

;;; Storage element descriptors

(defstruct storage-element
  (name nil :type symbol)
  (kind nil :type (member :register :stack :memory :flag))
  (width nil :type (or null (integer 1)))
  ;; :count > 1 marks a banked/array register (e.g. CHIP8's V0-VF). M0 only
  ;; implements scalar (:count 1) access via WITH-MACHINE's symbol-macrolet;
  ;; indexed access for :count > 1 is parsed/stored here but not yet wired
  ;; up to an accessor -- see the M1/M4 backlog ticket for indexed access.
  (count 1 :type (integer 1))
  (depth nil :type (or null (integer 1)))       ; stacks
  (addr-width nil :type (or null (integer 1)))  ; memory
  (cell-width nil :type (or null (integer 1)))) ; memory, defaults to width

(defstruct machine-descriptor
  (name nil :type symbol)
  (elements nil :type list)               ; ordered list of storage-element
  (table (make-hash-table :test 'eq)))    ; name -> storage-element

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
  (slots (make-hash-table :test 'eq)))    ; name -> slot representation

;; Slot representations:
;;   :register / :flag -> a one-element (simple-vector 1) box holding an
;;                         unsigned integer
;;   :stack             -> a cons (vector . sp), vector is adjustable
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
    ((:register :flag)
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
  "Zero all storage on MACHINE."
  (dolist (element (machine-descriptor-elements (machine-descriptor machine)))
    (let ((slot (gethash (storage-element-name element) (machine-slots machine))))
      (ecase (storage-element-kind element)
        ((:register :flag) (setf (aref slot 0) 0))
        (:stack (fill (car slot) 0) (setf (cdr slot) 0))
        (:memory (fill slot 0)))))
  machine)

;;; Accessors

(defun %slot (machine name kind)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (eq (storage-element-kind element) kind)
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun sref (machine name)
  "Read a register or flag by NAME as an unsigned integer."
  (multiple-value-bind (slot element) (%slot-any machine name)
    (declare (ignore element))
    (aref slot 0)))

(defun %slot-any (machine name)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (member (storage-element-kind element) '(:register :flag))
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun (setf sref) (value machine name)
  (multiple-value-bind (slot element) (%slot-any machine name)
    (setf (aref slot 0) (wrap-value value (storage-element-width element)))))

(defun flag (machine name)
  "Read a flag by NAME as 0 or 1."
  (multiple-value-bind (slot element) (%slot machine name :flag)
    (declare (ignore element))
    (aref slot 0)))

(defun (setf flag) (value machine name)
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
