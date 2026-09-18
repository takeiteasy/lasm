;;;; snapshot.lisp
;;;; #112 (M7): versioned machine-state snapshots -- capture a MACHINE's
;;;; runtime state as plain readable data, restore it later, and persist it as
;;;; an s-expression file. Covers the same state RESET clears (storage.lisp);
;;;; MACHINE-INTERRUPT-HOOK is host wiring and is neither saved nor touched.

(in-package #:lasm)

(defconstant +snapshot-version+ 1)

(define-condition snapshot-error (lasm-error)
  ((detail :initarg :detail :reader snapshot-error-detail))
  (:report (lambda (c s) (format s "Snapshot error: ~A" (snapshot-error-detail c)))))

(define-condition snapshot-version-mismatch (snapshot-error) ())
(define-condition snapshot-machine-mismatch (snapshot-error) ())
(define-condition snapshot-malformed (snapshot-error) ())
(define-condition snapshot-device-unknown (snapshot-error) ())

(defun %snapshot-fail (type control &rest args)
  (error type :detail (apply #'format nil control args)))

;;; Capture

(defun %element-shape (element)
  (list (storage-element-name element)
        (storage-element-kind element)
        (storage-element-width element)
        (storage-element-count element)
        (storage-element-depth element)
        (storage-element-addr-width element)
        (storage-element-cell-width element)))

(defun %encode-runs (cells)
  "CELLS as a list of (COUNT . VALUE) runs."
  (let ((runs '()))
    (loop for value across cells
          do (if (and runs (eql (cdar runs) value))
                 (incf (caar runs))
                 (cl:push (cons 1 value) runs)))
    (nreverse runs)))

(defun %snapshot-element (machine element)
  (let* ((name (storage-element-name element))
         (slot (gethash name (machine-slots machine))))
    (ecase (storage-element-kind element)
      ((:register :flag) (list name :cells (coerce slot 'list)))
      (:stack (list name :cells (coerce (car slot) 'list) :sp (cdr slot)))
      (:memory (list name :runs (%encode-runs slot))))))

(defun %snapshot-device (machine device)
  (when device
    (let ((save (device-descriptor-save (device-descriptor device))))
      (if save
          (list (device-descriptor-name (device-descriptor device))
                :state (funcall save machine device))
          (list (device-descriptor-name (device-descriptor device)))))))

(defun machine-snapshot (machine)
  "MACHINE's runtime state as a plain-data list tree: storage, cycle
counters, idle flag, pending interrupts and the device bus (holes and bus
order included). A device contributes state only when it declares a :SAVE
hook, which must return data readable by READ. Interrupt signal data is
stored as-is and must be readable too."
  (let ((descriptor (machine-descriptor machine)))
    (list :lasm-snapshot
          :version +snapshot-version+
          :machine (machine-descriptor-name descriptor)
          :shape (mapcar #'%element-shape (machine-descriptor-elements descriptor))
          :cycles (machine-cycles machine)
          :extra-cycles (machine-extra-cycles machine)
          :idle (machine-idle machine)
          :elements (mapcar (lambda (element) (%snapshot-element machine element))
                            (machine-descriptor-elements descriptor))
          :interrupt-queue (mapcar (lambda (entry)
                                     (cons (and (car entry) (device-index (car entry)))
                                           (cdr entry)))
                                   (machine-interrupt-queue machine))
          :devices (map 'list (lambda (device) (%snapshot-device machine device))
                        (machine-devices machine)))))

;;; Validation

(defun %snapshot-field (snapshot key)
  (unless (and (consp snapshot) (eq (car snapshot) :lasm-snapshot) (evenp (length (cdr snapshot))))
    (%snapshot-fail 'snapshot-malformed "not a lasm snapshot"))
  (getf (cdr snapshot) key))

(defun %check-cell (value width what)
  (unless (and (integerp value) (<= 0 value) (< value (ash 1 width)))
    (%snapshot-fail 'snapshot-malformed "~A: ~S does not fit ~D bits" what value width)))

(defun %decode-element (element saved)
  "Validate SAVED against ELEMENT and return the values to apply: a cell
list for registers/flags, (CELLS . SP) for a stack, a cell vector for memory."
  (let ((name (storage-element-name element))
        (plist (cdr saved)))
    (ecase (storage-element-kind element)
      ((:register :flag)
       (let ((cells (getf plist :cells)))
         (unless (and (listp cells) (= (length cells) (storage-element-count element)))
           (%snapshot-fail 'snapshot-malformed "~S: expected ~D cells"
                           name (storage-element-count element)))
         (dolist (v cells) (%check-cell v (storage-element-width element) name))
         cells))
      (:stack
       (let ((cells (getf plist :cells)) (sp (getf plist :sp)))
         (unless (and (listp cells) (= (length cells) (storage-element-depth element)))
           (%snapshot-fail 'snapshot-malformed "~S: expected ~D cells"
                           name (storage-element-depth element)))
         (unless (and (integerp sp) (<= 0 sp (storage-element-depth element)))
           (%snapshot-fail 'snapshot-malformed "~S: bad stack pointer ~S" name sp))
         (dolist (v cells) (%check-cell v (storage-element-width element) name))
         (cons cells sp)))
      (:memory
       (let* ((runs (getf plist :runs))
              (cell-width (%memory-cell-width element))
              (size (ash 1 (storage-element-addr-width element)))
              (cells (make-array size :element-type `(unsigned-byte ,cell-width)))
              (pos 0))
         (unless (listp runs)
           (%snapshot-fail 'snapshot-malformed "~S: runs must be a list" name))
         (dolist (run runs)
           (unless (and (consp run) (integerp (car run)) (plusp (car run))
                        (<= (+ pos (car run)) size))
             (%snapshot-fail 'snapshot-malformed "~S: bad run ~S" name run))
           (%check-cell (cdr run) cell-width name)
           (fill cells (cdr run) :start pos :end (+ pos (car run)))
           (incf pos (car run)))
         (unless (= pos size)
           (%snapshot-fail 'snapshot-malformed "~S: runs cover ~D of ~D cells" name pos size))
         cells)))))

(defun %resolve-device-descriptor (machine name)
  "The descriptor NAME denotes on MACHINE: a declared one, or that of a
runtime-attached device already on the bus."
  (or (find name (machine-descriptor-devices (machine-descriptor machine))
            :key #'device-descriptor-name)
      (let ((live (find-device machine name)))
        (and live (device-descriptor live)))
      (%snapshot-fail 'snapshot-device-unknown
                      "device ~S is neither declared on machine ~S nor attached"
                      name (machine-descriptor-name (machine-descriptor machine)))))

(defun %validate-snapshot (machine snapshot)
  "Signal on anything wrong with SNAPSHOT; return (VALUES ELEMENT-VALUES
DEVICE-PLAN) ready to apply."
  (let ((descriptor (machine-descriptor machine)))
    (let ((version (%snapshot-field snapshot :version)))
      (unless (eql version +snapshot-version+)
        (%snapshot-fail 'snapshot-version-mismatch
                        "snapshot version ~S, this lasm reads version ~D"
                        version +snapshot-version+)))
    (unless (eq (%snapshot-field snapshot :machine) (machine-descriptor-name descriptor))
      (%snapshot-fail 'snapshot-machine-mismatch "snapshot is for machine ~S, not ~S"
                      (%snapshot-field snapshot :machine) (machine-descriptor-name descriptor)))
    (unless (equal (%snapshot-field snapshot :shape)
                   (mapcar #'%element-shape (machine-descriptor-elements descriptor)))
      (%snapshot-fail 'snapshot-machine-mismatch
                      "snapshot storage layout differs from machine ~S"
                      (machine-descriptor-name descriptor)))
    (let* ((saved (%snapshot-field snapshot :elements))
           (values (loop for element in (machine-descriptor-elements descriptor)
                         for entry = (assoc (storage-element-name element) saved)
                         do (unless entry
                              (%snapshot-fail 'snapshot-malformed "missing element ~S"
                                              (storage-element-name element)))
                         collect (%decode-element element entry)))
           (bus (%snapshot-field snapshot :devices))
           (plan (progn
                   (unless (listp bus) (%snapshot-fail 'snapshot-malformed "bad device bus"))
                   (mapcar (lambda (entry)
                             (and entry
                                  (list (%resolve-device-descriptor machine (car entry))
                                        (getf (cdr entry) :state))))
                           bus))))
      (let ((count (length plan)))
        (dolist (entry (%snapshot-field snapshot :interrupt-queue))
          (unless (and (consp entry)
                       (or (null (car entry))
                           (and (integerp (car entry)) (< -1 (car entry) count)
                                (nth (car entry) plan))))
            (%snapshot-fail 'snapshot-malformed "interrupt queue names missing device ~S"
                            (and (consp entry) (car entry))))))
      (dolist (key '(:cycles :extra-cycles))
        (unless (typep (%snapshot-field snapshot key) 'unsigned-byte)
          (%snapshot-fail 'snapshot-malformed "bad ~S" key)))
      (values values plan))))

;;; Restore

(defun %apply-element (machine element value)
  (let ((slot (gethash (storage-element-name element) (machine-slots machine))))
    (ecase (storage-element-kind element)
      ((:register :flag) (replace slot value))
      (:stack (replace (car slot) (car value))
       (setf (cdr slot) (cdr value)))
      (:memory (replace slot value)))))

(defun restore-snapshot (machine snapshot)
  "Replace MACHINE's state with SNAPSHOT (from MACHINE-SNAPSHOT or
READ-SNAPSHOT) and return MACHINE. Signals SNAPSHOT-VERSION-MISMATCH,
SNAPSHOT-MACHINE-MISMATCH (a different machine or storage layout),
SNAPSHOT-MALFORMED or SNAPSHOT-DEVICE-UNKNOWN before touching MACHINE.

The device bus is rebuilt at its saved shape: holes stay holes, and each
device is re-INIT'd and then given its saved state through :LOAD. A device
attached at runtime must already be on MACHINE's bus to be restored.
MACHINE-INTERRUPT-HOOK is left as installed."
  (multiple-value-bind (values plan) (%validate-snapshot machine snapshot)
    (loop for element in (machine-descriptor-elements (machine-descriptor machine))
          for value in values
          do (%apply-element machine element value))
    (setf (machine-cycles machine) (%snapshot-field snapshot :cycles)
          (machine-extra-cycles machine) (%snapshot-field snapshot :extra-cycles)
          (machine-idle machine) (%snapshot-field snapshot :idle))
    (let ((devices (machine-devices machine)))
      (setf (fill-pointer devices) 0)
      (dolist (entry plan)
        (vector-push-extend
         (when entry
           (let* ((device (%instantiate-device machine (first entry) (fill-pointer devices)))
                  (load (device-descriptor-load (first entry))))
             (when (and load (device-descriptor-save (first entry)))
               (funcall load machine device (second entry)))
             device))
         devices)))
    (setf (machine-interrupt-queue machine)
          (mapcar (lambda (entry)
                    (cons (and (car entry) (aref (machine-devices machine) (car entry)))
                          (cdr entry)))
                  (%snapshot-field snapshot :interrupt-queue))))
  machine)

;;; Files

(defun write-snapshot (snapshot path)
  "Write SNAPSHOT to PATH as a readable s-expression, replacing any existing
file. Returns PATH."
  (with-open-file (out path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword)))
        (prin1 snapshot out)
        (terpri out))))
  path)

(defun read-snapshot (path)
  "The snapshot stored at PATH. The file is untrusted: it is read without
reader evaluation and signals SNAPSHOT-MALFORMED if it is not a readable
snapshot form."
  (with-open-file (in path)
    (let ((form (handler-case
                    (with-standard-io-syntax
                      (let ((*package* (find-package :keyword))
                            (*read-eval* nil))
                        (read in nil :eof)))
                  ((or reader-error end-of-file package-error) (e)
                    (%snapshot-fail 'snapshot-malformed "~A: unreadable (~A)" path e)))))
      (unless (and (consp form) (eq (car form) :lasm-snapshot))
        (%snapshot-fail 'snapshot-malformed "~A: not a lasm snapshot" path))
      form)))
