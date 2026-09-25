;;;; snapshot.lisp
;;;; #112 (M7): versioned machine-state snapshots -- capture a MACHINE's
;;;; runtime state as plain readable data, restore it later, and persist it as
;;;; an s-expression or compact binary file. Covers the same state RESET clears (storage.lisp);
;;;; MACHINE-INTERRUPT-HOOK is host wiring and is neither saved nor touched.

(in-package #:lasm)

(defconstant +snapshot-version+ 6)

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

(defun %snapshot-element (machine element cells)
  (let* ((name (storage-element-name element))
         (slot (gethash name (machine-slots machine))))
    (ecase (storage-element-kind element)
      ((:register :flag) (list name :cells (coerce slot 'list)))
      (:stack (list name :cells (coerce (car slot) 'list) :sp (cdr slot)))
      (:memory (if cells (list name :runs (%encode-runs slot)) (list name))))))

(defun %snapshot-device (machine device)
  (when device
    (let ((save (device-descriptor-save (device-descriptor device))))
      (if save
          (list (device-descriptor-name (device-descriptor device))
                :state (funcall save machine device))
          (list (device-descriptor-name (device-descriptor device)))))))

(defun %bank-shape (descriptor)
  (loop for (nil . region) in (%banked-regions descriptor)
        collect (list (memory-region-name region) (memory-region-start region)
                      (memory-region-end region) (memory-region-banks region))))

(defun %snapshot-banks (machine cells)
  (loop for (nil . region) in (%banked-regions (machine-descriptor machine))
        collect (let ((state (gethash (memory-region-name region) (machine-banks machine))))
                  (list (memory-region-name region)
                        :current (car state)
                        :loaded (gethash (memory-region-name region) (machine-loaded-banks machine))
                        :banks (and cells (map 'list #'%encode-runs (cdr state)))))))

(defun %machine-snapshot (machine cells)
  "MACHINE-SNAPSHOT, leaving out memory and bank cell contents unless CELLS.
Such a partial snapshot only restores through %RESTORE-SNAPSHOT with CELLS NIL."
  (let ((descriptor (machine-descriptor machine)))
    (list :lasm-snapshot
          :version +snapshot-version+
          :machine (machine-descriptor-name descriptor)
          :shape (mapcar #'%element-shape (machine-descriptor-elements descriptor))
          :bank-shape (%bank-shape descriptor)
          :cycles (machine-cycles machine)
          :idle (machine-idle machine)
          :elements (mapcar (lambda (element) (%snapshot-element machine element cells))
                            (machine-descriptor-elements descriptor))
          :interrupt-queue (let (entries)
                             (map-pending-interrupts
                              (lambda (device data priority non-maskable)
                                (cl:push (list (and device (device-index device)) data priority non-maskable)
                                         entries))
                              machine)
                             (nreverse entries))
          :interrupt-active (copy-list (machine-interrupt-active machine))
          :banks (%snapshot-banks machine cells)
          :devices (map 'list (lambda (device) (%snapshot-device machine device))
                        (machine-devices machine))
          :region-bindings (loop for name being the hash-keys of (machine-region-bindings machine)
                                   using (hash-value index)
                                 collect (cons name index)))))

(defun %assembly-program (assembly)
  "The :PROGRAM entry for ASSEMBLY, or NIL unless it was assembled from a file."
  (let* ((root (assembly-source-unit assembly))
         (path (and root (source-unit-path root)))
         (files '())
         (includes '()))
    (when path
      (labels ((walk (unit)
                 (unless (assoc (source-unit-file unit) files :test #'string=)
                   (cl:push (cons (source-unit-file unit) (source-unit-text unit)) files))
                 (let ((children (source-unit-children unit)))
                   (dolist (line (sort (loop for line being the hash-keys of children collect line) #'<))
                     (dolist (child (gethash line children))
                       (unless (assoc (source-unit-path child) includes :test #'string=)
                         (cl:push (cons (source-unit-path child) (source-unit-file child)) includes))
                       (walk child))))))
        (walk root))
      (let ((parameters (assembly-parameters assembly)))
        (list :file (source-unit-file root)
              :path path
              :origin (getf parameters :origin)
              :memory (getf parameters :memory)
              :lexer (getf parameters :lexer)
              :files (cons (cons path (source-unit-text root))
                           (remove (source-unit-file root) (nreverse files)
                                   :key #'car :test #'string=))
              :includes (nreverse includes))))))

(defun machine-snapshot (machine &key assembly)
  "MACHINE's runtime state as a plain-data list tree: storage, cycle
counters, idle flag, pending interrupts, banked regions, the device bus (holes and bus
order included) and runtime BIND-REGION bindings. A device contributes state only when it declares a :SAVE
hook, which must return data readable by READ. Interrupt signal data is
stored as-is and must be readable too.

ASSEMBLY, when it was assembled from a file (ASSEMBLE-FILE), adds a :PROGRAM
entry holding the source text of that file and every file it includes, so
SNAPSHOT-ASSEMBLY can rebuild it without them."
  (let ((snapshot (%machine-snapshot machine t))
        (program (and assembly (%assembly-program assembly))))
    (if program
        (append snapshot (list :program program))
        snapshot)))

;;; Validation

(defun %snapshot-field (snapshot key)
  (unless (and (consp snapshot) (eq (car snapshot) :lasm-snapshot) (evenp (length (cdr snapshot))))
    (%snapshot-fail 'snapshot-malformed "not a lasm snapshot"))
  (getf (cdr snapshot) key))

(defun %check-cell (value width what)
  (unless (and (integerp value) (<= 0 value) (< value (ash 1 width)))
    (%snapshot-fail 'snapshot-malformed "~A: ~S does not fit ~D bits" what value width)))

(defun %decode-runs (runs size cell-width name)
  "Validate RUNS and expand them to a SIZE-cell array of CELL-WIDTH-bit cells."
  (let ((cells (make-array size :element-type `(unsigned-byte ,cell-width)))
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
    cells))

(defun %decode-banks (machine saved cells)
  "Validate SAVED banked-region entries; return a list of (NAME CURRENT ARRAYS LOADED).
ARRAYS is NIL unless CELLS."
  (unless (listp saved)
    (%snapshot-fail 'snapshot-malformed "bad banks"))
  (loop for (element . region) in (%banked-regions (machine-descriptor machine))
        collect (let* ((name (memory-region-name region))
                       (entry (assoc name saved))
                       (plist (cdr entry))
                       (current (getf plist :current))
                       (banks (getf plist :banks)))
                  (unless entry
                    (%snapshot-fail 'snapshot-malformed "missing banked region ~S" name))
                  (unless (or (not cells)
                              (and (listp banks) (= (length banks) (memory-region-banks region))))
                    (%snapshot-fail 'snapshot-malformed "~S: expected ~D banks"
                                    name (memory-region-banks region)))
                  (unless (and (integerp current) (< -1 current (memory-region-banks region)))
                    (%snapshot-fail 'snapshot-malformed "~S: bad current bank ~S" name current))
                  (let ((loaded (getf plist :loaded)))
                    (unless (or (null loaded) (and (integerp loaded) (< -1 loaded (memory-region-banks region))))
                      (%snapshot-fail 'snapshot-malformed "~S: bad loaded bank ~S" name loaded))
                    (list name current
                          (and cells
                               (mapcar (lambda (runs)
                                         (%decode-runs runs
                                                       (1+ (- (memory-region-end region) (memory-region-start region)))
                                                       (storage-element-cell-width element) name))
                                       banks))
                          loaded)))))

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
       (%decode-runs (getf plist :runs) (ash 1 (storage-element-addr-width element))
                     (storage-element-cell-width element) name)))))

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

(defun %check-snapshot-version (snapshot)
  (let ((version (%snapshot-field snapshot :version)))
    (unless (eql version +snapshot-version+)
      (%snapshot-fail 'snapshot-version-mismatch
                      "snapshot version ~S, this lasm reads version ~D"
                      version +snapshot-version+))))

(defun %validate-snapshot (machine snapshot cells)
  "Signal on anything wrong with SNAPSHOT; return (VALUES ELEMENT-VALUES
DEVICE-PLAN BANK-VALUES) ready to apply."
  (let ((descriptor (machine-descriptor machine)))
    (%check-snapshot-version snapshot)
    (unless (eq (%snapshot-field snapshot :machine) (machine-descriptor-name descriptor))
      (%snapshot-fail 'snapshot-machine-mismatch "snapshot is for machine ~S, not ~S"
                      (%snapshot-field snapshot :machine) (machine-descriptor-name descriptor)))
    (unless (equal (%snapshot-field snapshot :shape)
                   (mapcar #'%element-shape (machine-descriptor-elements descriptor)))
      (%snapshot-fail 'snapshot-machine-mismatch
                      "snapshot storage layout differs from machine ~S"
                      (machine-descriptor-name descriptor)))
    (unless (equal (%snapshot-field snapshot :bank-shape) (%bank-shape descriptor))
      (%snapshot-fail 'snapshot-machine-mismatch
                      "snapshot bank layout differs from machine ~S"
                      (machine-descriptor-name descriptor)))
    (let* ((saved (%snapshot-field snapshot :elements))
           (values (loop for element in (machine-descriptor-elements descriptor)
                         for entry = (assoc (storage-element-name element) saved)
                         do (unless entry
                              (%snapshot-fail 'snapshot-malformed "missing element ~S"
                                              (storage-element-name element)))
                         collect (and (or cells (not (eq (storage-element-kind element) :memory)))
                                      (%decode-element element entry))))
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
          (unless (and (consp entry) (consp (cdr entry)) (consp (cddr entry))
                       (consp (cdddr entry)) (null (cddddr entry))
                       (integerp (third entry))
                       (typep (fourth entry) 'boolean)
                       (or (null (car entry))
                           (and (integerp (car entry)) (< -1 (car entry) count)
                                (nth (car entry) plan))))
            (%snapshot-fail 'snapshot-malformed "interrupt queue names missing device ~S"
                            (and (consp entry) (car entry))))))
      (let ((active (%snapshot-field snapshot :interrupt-active)))
        (unless (and (listp active) (every #'integerp active))
          (%snapshot-fail 'snapshot-malformed "bad ~S" :interrupt-active)))
      (dolist (entry (%snapshot-field snapshot :region-bindings))
        (unless (and (consp entry) (symbolp (car entry)) (integerp (cdr entry))
                     (< -1 (cdr entry) (length plan)) (nth (cdr entry) plan)
                     (ignore-errors (%bindable-region machine (car entry))))
          (%snapshot-fail 'snapshot-malformed "bad region binding ~S" entry)))
      (unless (typep (%snapshot-field snapshot :cycles) 'unsigned-byte)
        (%snapshot-fail 'snapshot-malformed "bad ~S" :cycles))
      (values values plan (%decode-banks machine (%snapshot-field snapshot :banks) cells)))))

;;; Restore

(defun %apply-element (machine element value)
  "Apply VALUE to ELEMENT; a NIL VALUE (memory of a cell-less restore) changes nothing."
  (when value
    (let ((slot (gethash (storage-element-name element) (machine-slots machine))))
      (ecase (storage-element-kind element)
        ((:register :flag) (replace slot value))
        (:stack (replace (car slot) (car value))
         (setf (cdr slot) (cdr value)))
        (:memory (replace slot value))))))

(defun %restore-snapshot (machine snapshot cells)
  "RESTORE-SNAPSHOT; without CELLS, memory and bank cell contents stay as they are."
  (multiple-value-bind (values plan banks) (%validate-snapshot machine snapshot cells)
    (when cells (%mark-all-dirty machine))
    (loop for element in (machine-descriptor-elements (machine-descriptor machine))
          for value in values
          do (%apply-element machine element value))
    (loop for (name current arrays loaded) in banks
          for state = (gethash name (machine-banks machine))
          do (setf (car state) current)
             (if loaded
                 (setf (gethash name (machine-loaded-banks machine)) loaded)
                 (remhash name (machine-loaded-banks machine)))
             (loop for array in arrays
                   for target across (cdr state)
                   do (replace target array)))
    (setf (machine-cycles machine) (%snapshot-field snapshot :cycles)
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
    (clrhash (machine-region-bindings machine))
    (loop for (name . index) in (%snapshot-field snapshot :region-bindings)
          do (setf (gethash name (machine-region-bindings machine)) index))
    (%clear-interrupt-queue machine)
    (dolist (entry (%snapshot-field snapshot :interrupt-queue))
      (%push-pending machine
                     (make-pending-interrupt
                      :device (and (first entry) (aref (machine-devices machine) (first entry)))
                      :data (second entry) :priority (third entry) :non-maskable (fourth entry))))
    (setf (machine-interrupt-active machine)
          (copy-list (%snapshot-field snapshot :interrupt-active))))
  machine)

(defun restore-snapshot (machine snapshot)
  "Replace MACHINE's state with SNAPSHOT (from MACHINE-SNAPSHOT or
READ-SNAPSHOT) and return MACHINE. Signals SNAPSHOT-VERSION-MISMATCH,
SNAPSHOT-MACHINE-MISMATCH (a different machine or storage layout),
SNAPSHOT-MALFORMED or SNAPSHOT-DEVICE-UNKNOWN before touching MACHINE.

The device bus is rebuilt at its saved shape: holes stay holes, and each
device is re-INIT'd and then given its saved state through :LOAD. A device
attached at runtime must already be on MACHINE's bus to be restored.
MACHINE-INTERRUPT-HOOK is left as installed."
  (%restore-snapshot machine snapshot t))

;;; Embedded programs

(defun %check-program (program)
  "Signal SNAPSHOT-MALFORMED unless PROGRAM is a well-formed :PROGRAM entry."
  (flet ((bad (control &rest args)
           (%snapshot-fail 'snapshot-malformed "program: ~?" control args))
         (text-alist-p (alist)
           (and (listp alist) (null (cdr (last alist)))
                (every (lambda (entry) (and (consp entry) (stringp (car entry)) (stringp (cdr entry))))
                       alist))))
    (unless (and (listp program) (null (cdr (last program))) (evenp (length program)))
      (bad "not a property list"))
    (let ((file (getf program :file)) (path (getf program :path))
          (origin (getf program :origin)) (memory (getf program :memory))
          (lexer (getf program :lexer))
          (files (getf program :files)) (includes (getf program :includes)))
      (unless (and (stringp file) (stringp path)) (bad "bad file name"))
      (unless (typep origin '(integer 0)) (bad "bad origin ~S" origin))
      (unless (symbolp memory) (bad "bad memory ~S" memory))
      (unless (and lexer (symbolp lexer) (gethash lexer *lexers*)) (bad "unknown lexer ~S" lexer))
      (unless (and (consp files) (text-alist-p files)) (bad "bad files"))
      (unless (= (length files) (length (remove-duplicates (mapcar #'car files) :test #'string=)))
        (bad "duplicate files"))
      (unless (assoc path files :test #'string=) (bad "main file ~S is not among the files" path))
      (unless (and (text-alist-p includes)
                   (every (lambda (entry) (assoc (cdr entry) files :test #'string=)) includes))
        (bad "bad includes"))
      (dolist (name (append (mapcar #'car files) (mapcar #'car includes) (list file)))
        (unless (ignore-errors (pathname name) t)
          (bad "bad path ~S" name))))))

(defun snapshot-assembly (snapshot &key machine)
  "The ASSEMBLY rebuilt from SNAPSHOT's embedded program (see MACHINE-SNAPSHOT),
or NIL when it has none. MACHINE is the machine name to assemble for, checked
against the snapshot's, and defaults to the snapshot's. .include reads only
the embedded files, never the disk. Signals SNAPSHOT-VERSION-MISMATCH,
SNAPSHOT-MACHINE-MISMATCH or SNAPSHOT-MALFORMED for a snapshot that does not
fit, and whatever assembling signals for a program that no longer assembles."
  (%check-snapshot-version snapshot)
  (let ((program (%snapshot-field snapshot :program))
        (name (%snapshot-field snapshot :machine)))
    (when program
      (when (and machine (not (eq machine name)))
        (%snapshot-fail 'snapshot-machine-mismatch "snapshot is for machine ~S, not ~S" name machine))
      (%check-program program)
      (let* ((files (getf program :files))
             (sources (make-hash-table :test 'equal)))
        (dolist (entry files)
          (setf (gethash (car entry) sources) entry))
        (dolist (entry (getf program :includes))
          (setf (gethash (car entry) sources) (assoc (cdr entry) files :test #'string=)))
        (setf (gethash (getf program :file) sources)
              (assoc (getf program :path) files :test #'string=))
        (let ((*include-sources* sources))
          (%assemble-source (cdr (gethash (getf program :file) sources))
                            (getf program :file) (pathname (getf program :path))
                            :machine name :lexer (getf program :lexer)
                            :origin (getf program :origin) :memory (getf program :memory)))))))

;;; Binary files

(defparameter *binary-snapshot-magic*
  (make-array 8 :element-type '(unsigned-byte 8)
                :initial-contents '(#x89 #.(char-code #\L) #.(char-code #\S) #.(char-code #\N)
                                    #.(char-code #\P) #x0D #x0A #x1A)))

(defconstant +binary-snapshot-format+ 1)

(defconstant +binary-max-depth+ 1000
  "Deepest list nesting the binary reader accepts.")

(defconstant +binary-max-varint+ 1024
  "Longest varint, in bytes, the binary reader accepts.")

(defun %binary-buffer ()
  (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %put-uleb (n out)
  (loop (let ((byte (ldb (byte 7 0) n)))
          (setf n (ash n -7))
          (when (zerop n)
            (return (vector-push-extend byte out)))
          (vector-push-extend (logior byte #x80) out))))

(defun %put-string (string out)
  (let ((octets (%binary-buffer)))
    (loop for char across string
          for code = (char-code char)
          do (flet ((put (bits shift lead)
                      (vector-push-extend (logior lead (ldb (byte bits shift) code)) octets)))
               (cond ((< code #x80) (put 7 0 0))
                     ((< code #x800) (put 5 6 #xC0) (put 6 0 #x80))
                     ((< code #x10000) (put 4 12 #xE0) (put 6 6 #x80) (put 6 0 #x80))
                     (t (put 3 18 #xF0) (put 6 12 #x80) (put 6 6 #x80) (put 6 0 #x80)))))
    (%put-uleb (length octets) out)
    (loop for octet across octets do (vector-push-extend octet out))))

(defun %put-fallback (node out)
  (vector-push-extend #x09 out)
  (%put-string (with-standard-io-syntax
                 (let ((*package* (find-package :keyword)))
                   (prin1-to-string node)))
               out))

(defun %put-node (node out)
  (typecase node
    (null (vector-push-extend #x00 out))
    ((eql t) (vector-push-extend #x01 out))
    ((integer 0 127) (vector-push-extend (+ #x80 node) out))
    (integer (cond ((> (integer-length node) 6000)
                    (%put-fallback node out))
                   ((minusp node) (vector-push-extend #x03 out) (%put-uleb (- -1 node) out))
                   (t (vector-push-extend #x02 out) (%put-uleb node out))))
    (keyword (vector-push-extend #x04 out) (%put-string (symbol-name node) out))
    (symbol (if (symbol-package node)
                (progn (vector-push-extend #x05 out)
                       (%put-string (package-name (symbol-package node)) out)
                       (%put-string (symbol-name node) out))
                (%put-fallback node out)))
    (string (vector-push-extend #x06 out) (%put-string node out))
    (cons (let ((items (loop for tail = node then (cdr tail)
                             while (consp tail) collect (car tail)))
                (tail (cdr (last node))))
            (vector-push-extend (if tail #x08 #x07) out)
            (%put-uleb (length items) out)
            (dolist (item items) (%put-node item out))
            (when tail (%put-node tail out))))
    (t (%put-fallback node out))))

(defun %write-binary-snapshot (snapshot path)
  (let ((out (%binary-buffer)))
    (loop for octet across *binary-snapshot-magic* do (vector-push-extend octet out))
    (vector-push-extend +binary-snapshot-format+ out)
    (%put-node snapshot out)
    (with-open-file (stream path :direction :output :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (write-sequence out stream))))

(defstruct (bin-cursor (:constructor make-bin-cursor (octets path)))
  octets path (pos 0 :type fixnum))

(defun %bin-fail (cursor control &rest args)
  (%snapshot-fail 'snapshot-malformed "~A: ~?" (bin-cursor-path cursor) control args))

(defun %bin-remaining (cursor)
  (- (length (bin-cursor-octets cursor)) (bin-cursor-pos cursor)))

(defun %bin-byte (cursor)
  (when (zerop (%bin-remaining cursor))
    (%bin-fail cursor "truncated snapshot"))
  (prog1 (aref (bin-cursor-octets cursor) (bin-cursor-pos cursor))
    (incf (bin-cursor-pos cursor))))

(defun %bin-uleb (cursor)
  (let ((n 0))
    (dotimes (i +binary-max-varint+ (%bin-fail cursor "varint too long"))
      (let ((byte (%bin-byte cursor)))
        (setf n (logior n (ash (ldb (byte 7 0) byte) (* 7 i))))
        (when (< byte #x80)
          (return n))))))

(defun %bin-string (cursor)
  (let ((length (%bin-uleb cursor)))
    (when (> length (%bin-remaining cursor))
      (%bin-fail cursor "string longer than the file"))
    (let* ((octets (bin-cursor-octets cursor))
           (pos (bin-cursor-pos cursor))
           (end (+ pos length)))
      (prog1
          (with-output-to-string (out)
            (loop while (< pos end)
                  do (let* ((lead (aref octets pos))
                            (extra (cond ((< lead #x80) 0) ((<= #xC2 lead #xDF) 1)
                                         ((<= #xE0 lead #xEF) 2) ((<= #xF0 lead #xF4) 3)
                                         (t (%bin-fail cursor "bad UTF-8"))))
                            (code (if (zerop extra) lead (ldb (byte (- 6 extra) 0) lead))))
                       (when (> (+ pos 1 extra) end)
                         (%bin-fail cursor "bad UTF-8"))
                       (dotimes (i extra)
                         (let ((next (aref octets (+ pos 1 i))))
                           (unless (= (logand next #xC0) #x80)
                             (%bin-fail cursor "bad UTF-8"))
                           (setf code (logior (ash code 6) (logand next #x3F)))))
                       (unless (and (>= code (svref #(0 0 #x80 #x800 #x10000) (1+ extra)))
                                    (< code #x110000))
                         (%bin-fail cursor "bad UTF-8"))
                       (write-char (code-char code) out)
                       (incf pos (1+ extra)))))
        (setf (bin-cursor-pos cursor) end)))))

(defun %bin-fallback (cursor)
  (let ((text (%bin-string cursor)))
    (with-standard-io-syntax
      (let ((*package* (find-package :keyword))
            (*read-eval* nil))
        (multiple-value-bind (form end) (read-from-string text)
          (unless (every (lambda (char) (member char '(#\Space #\Tab #\Newline)))
                         (subseq text end))
            (%bin-fail cursor "trailing text in a fallback atom"))
          form)))))

(defun %bin-list (cursor depth dotted)
  (let ((count (%bin-uleb cursor)))
    (when (or (zerop count) (> count (%bin-remaining cursor)))
      (%bin-fail cursor "bad list length ~D" count))
    (let ((items (loop repeat count collect (%bin-node cursor (1+ depth)))))
      (when dotted
        (setf (cdr (last items)) (%bin-node cursor (1+ depth))))
      items)))

(defun %bin-node (cursor depth)
  (when (> depth +binary-max-depth+)
    (%bin-fail cursor "nested deeper than ~D" +binary-max-depth+))
  (let ((tag (%bin-byte cursor)))
    (if (>= tag #x80)
        (- tag #x80)
        (case tag
          (#x00 nil)
          (#x01 t)
          (#x02 (%bin-uleb cursor))
          (#x03 (- -1 (%bin-uleb cursor)))
          (#x04 (intern (%bin-string cursor) :keyword))
          (#x05 (let* ((package-name (%bin-string cursor))
                       (package (find-package package-name)))
                  (unless package
                    (%bin-fail cursor "no package ~A" package-name))
                  (intern (%bin-string cursor) package)))
          (#x06 (%bin-string cursor))
          (#x07 (%bin-list cursor depth nil))
          (#x08 (%bin-list cursor depth t))
          (#x09 (%bin-fallback cursor))
          (t (%bin-fail cursor "bad tag ~D" tag))))))

(defun %read-binary-snapshot (octets path)
  (let ((cursor (make-bin-cursor octets path)))
    (setf (bin-cursor-pos cursor) (min (length octets) (length *binary-snapshot-magic*)))
    (let ((format (%bin-byte cursor)))
      (unless (eql format +binary-snapshot-format+)
        (%snapshot-fail 'snapshot-version-mismatch
                        "~A: binary snapshot format ~D, this lasm reads format ~D"
                        path format +binary-snapshot-format+)))
    (let ((form (handler-case
                    (prog1 (%bin-node cursor 0)
                      (unless (zerop (%bin-remaining cursor))
                        (%bin-fail cursor "trailing data")))
                  (snapshot-error (e) (error e))
                  (error (e) (%bin-fail cursor "unreadable (~A)" e)))))
      (unless (and (consp form) (eq (car form) :lasm-snapshot))
        (%snapshot-fail 'snapshot-malformed "~A: not a lasm snapshot" path))
      form)))

(defun %binary-snapshot-file-p (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((head (make-array (length *binary-snapshot-magic*) :element-type '(unsigned-byte 8))))
      (and (= (read-sequence head in) (length head))
           (equalp head *binary-snapshot-magic*)))))

(defun %file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

;;; Files

(defun write-snapshot (snapshot path &key (format :sexp))
  "Write SNAPSHOT to PATH, replacing any existing file: as a readable
s-expression for FORMAT :SEXP, or as a compact binary encoding of the same
data for :BINARY. Returns PATH."
  (check-type format (member :sexp :binary))
  (ecase format
    (:sexp
     (with-open-file (out path :direction :output :if-exists :supersede)
       (with-standard-io-syntax
         (let ((*package* (find-package :keyword)))
           (prin1 snapshot out)
           (terpri out)))))
    (:binary (%write-binary-snapshot snapshot path)))
  path)

(defun read-snapshot (path)
  "The snapshot stored at PATH, in either format WRITE-SNAPSHOT writes. The
file is untrusted: it is read without reader evaluation and signals
SNAPSHOT-MALFORMED if it is not a readable snapshot form, and
SNAPSHOT-VERSION-MISMATCH for a binary format this lasm does not read."
  (when (%binary-snapshot-file-p path)
    (return-from read-snapshot (%read-binary-snapshot (%file-octets path) path)))
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
