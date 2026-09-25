;;;; tests/snapshot.lisp
;;;; #112: versioned machine-state snapshots.

(in-package #:lasm)

(fiveam:def-suite snapshot :in lasm)
(fiveam:in-suite snapshot)

;;; Fixture

(defun %saved-init (machine device)
  (declare (ignore machine device))
  (list 0))

(defun %saved-save (machine device)
  (declare (ignore machine))
  (copy-list (device-state device)))

(defun %saved-load (machine device data)
  (declare (ignore machine))
  (setf (device-state device) (copy-list data)))

(defmachine snapshot-test-machine
  (register pc :width 16)
  (register a :width 8)
  (register bank :width 8 :names (r0 r1 r2 r3))
  (register sp :width 8)
  (stack s :width 8 :depth 4)
  (memory ram :width 8 :addr-width 16)
  (flags z c)
  (interrupts :vector pc :message a :save (z) :queue 4)
  (device kept :id 1 :init %saved-init :save %saved-save :load %saved-load)
  (device plain :id 2 :init %saved-init))

(defun %fresh () (make-machine 'snapshot-test-machine))

(defun %dirty ()
  "A machine with something non-default in every kind of state."
  (let ((m (%fresh)))
    (setf (sref m 'a) 42
          (regref m 'bank 2) 7
          (flag m 'c) 1)
    (stack-push m 's 5)
    (stack-push m 's 9)
    (stack-pop m 's)
    (%poke m 'ram 0 1)
    (%poke m 'ram 1 1)
    (%poke m 'ram #xFFFF 200)
    (setf (machine-cycles m) 123
          (machine-extra-cycles m) 4
          (machine-idle m) t)
    (setf (car (device-state (device-at m 0))) 99)
    m))

(defun %state-of (m)
  (list (sref m 'a) (regref m 'bank 2) (flag m 'c) (stack-depth m 's)
        (stack-ref m 's 0)
        (mpeek m 'ram 0) (mpeek m 'ram 1) (mpeek m 'ram 2) (mpeek m 'ram #xFFFF)
        (machine-cycles m) (machine-extra-cycles m) (machine-idle m)))

;;; Round trip

(fiveam:test snapshot-round-trips-storage-and-counters
  (let* ((source (%dirty))
         (target (%fresh)))
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal (%state-of source) (%state-of target)))))

(fiveam:test snapshot-restores-over-existing-state
  (let ((source (%dirty)) (target (%dirty)))
    (setf (sref target 'a) 1 (machine-cycles target) 0)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal (%state-of source) (%state-of target)))))

(fiveam:test snapshot-keeps-whole-stack-backing-vector
  (let ((source (%fresh)) (target (%fresh)))
    (stack-push source 's 5)
    (stack-push source 's 6)
    (stack-pop source 's)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equalp (car (gethash 's (machine-slots source)))
                      (car (gethash 's (machine-slots target)))))
    (fiveam:is (= 1 (stack-depth target 's)))))

(fiveam:test snapshot-restores-a-moved-stack-pointer
  (let ((source (%fresh)) (target (%fresh)))
    (stack-push source 's 5)
    (stack-push source 's 6)
    (setf (stack-pointer source 's) 1)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (= 1 (stack-pointer target 's)))
    (setf (stack-pointer target 's) 2)
    (fiveam:is (= 6 (stack-ref target 's 0)))))

(fiveam:test snapshot-memory-is-run-length-encoded
  (let* ((m (%dirty))
         (runs (getf (cdr (assoc 'ram (getf (cdr (machine-snapshot m)) :elements))) :runs)))
    (fiveam:is (equal '((2 . 1) (65533 . 0) (1 . 200)) runs))))

(fiveam:test snapshot-leaves-interrupt-hook-alone
  (let ((m (%fresh)))
    (setf (machine-interrupt-hook m) nil)
    (restore-snapshot m (machine-snapshot (%dirty)))
    (fiveam:is (null (machine-interrupt-hook m)))))

;;; Interrupts and devices

(fiveam:test snapshot-round-trips-interrupt-queue
  (let ((source (%fresh)) (target (%fresh)))
    (setf (sref source 'pc) #x100)
    (device-signal source (device-at source 0) 11)
    (signal-interrupt source 22)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal '(11 22) (mapcar #'cdr (machine-interrupt-queue target))))
    (fiveam:is (eq (device-at target 0) (car (first (machine-interrupt-queue target)))))
    (fiveam:is (null (car (second (machine-interrupt-queue target)))))))

(fiveam:test snapshot-device-state-round-trips-through-hooks
  (let ((target (%fresh)))
    (restore-snapshot target (machine-snapshot (%dirty)))
    (fiveam:is (equal '(99) (device-state (device-at target 0))))))

(fiveam:test snapshot-hookless-device-is-reinitialised
  (let ((source (%fresh)) (target (%fresh)))
    (setf (car (device-state (device-at source 1))) 55)
    (setf (car (device-state (device-at target 1))) 66)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal '(0) (device-state (device-at target 1))))))

(fiveam:test snapshot-preserves-bus-holes-and-indices
  (let ((source (%fresh)) (target (%fresh)))
    (detach-device source 0)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (= 2 (device-count target)))
    (fiveam:signals no-such-device (device-at target 0))
    (fiveam:is (= 1 (device-index (device-at target 1))))))

(fiveam:test snapshot-restores-a-runtime-attached-device
  (let ((source (%fresh)) (target (%fresh)))
    (attach-device source 'extra :id 9 :init '%saved-init :save '%saved-save :load '%saved-load)
    (attach-device target 'extra :id 9 :init '%saved-init :save '%saved-save :load '%saved-load)
    (setf (car (device-state (device-at source 2))) 5)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (= 3 (device-count target)))
    (fiveam:is (equal '(5) (device-state (device-at target 2))))))

(fiveam:test snapshot-unknown-runtime-device-is-rejected
  (let ((source (%fresh)) (target (%fresh)))
    (attach-device source 'extra :id 9)
    (fiveam:signals snapshot-device-unknown
      (restore-snapshot target (machine-snapshot source)))))

;;; Runtime region bindings (#264)

(defmachine snapshot-binding-machine
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8
    (region slot #x40 #x4F :kind :device))
  (device kept :id 1 :init %saved-init :save %saved-save :load %saved-load))

(defun %binding-machine ()
  (let ((m (make-machine 'snapshot-binding-machine)))
    (attach-device m 'extra :id 9 :init '%saved-init :save '%saved-save :load '%saved-load)
    m))

(fiveam:test snapshot-round-trips-runtime-region-bindings
  (let ((source (%binding-machine)) (target (%binding-machine)))
    (bind-region source 'slot 'extra)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (equal '((slot . 1)) (getf (cdr (machine-snapshot target)) :region-bindings)))))

(fiveam:test snapshot-restore-clears-bindings-the-snapshot-lacks
  (let ((source (%binding-machine)) (target (%binding-machine)))
    (bind-region target 'slot 'extra)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (null (getf (cdr (machine-snapshot target)) :region-bindings)))))

(fiveam:test snapshot-rejects-a-bad-region-binding
  (let ((snapshot (machine-snapshot (%binding-machine))))
    (dolist (bad '(((slot . 7)) ((ram . 0)) ((nonesuch . 0)) (slot)))
      (fiveam:signals snapshot-malformed
        (restore-snapshot (%binding-machine) (%with-field snapshot :region-bindings bad))))))

;;; Rejection leaves the machine untouched

(defmacro %rejected-untouched (snapshot condition)
  `(let* ((m (%dirty)) (before (%state-of m)))
     (fiveam:signals ,condition (restore-snapshot m ,snapshot))
     (fiveam:is (equal before (%state-of m)))))

(defun %with-field (snapshot key value)
  (let ((copy (copy-list snapshot)))
    (setf (getf (cdr copy) key) value)
    copy))

(fiveam:test snapshot-rejects-wrong-version
  (%rejected-untouched (%with-field (machine-snapshot (%fresh)) :version 99)
                       snapshot-version-mismatch))

(fiveam:test snapshot-rejects-other-machine
  (%rejected-untouched (%with-field (machine-snapshot (%fresh)) :machine 'test-machine)
                       snapshot-machine-mismatch))

(fiveam:test snapshot-rejects-changed-layout
  (%rejected-untouched (%with-field (machine-snapshot (%fresh)) :shape '((a :register)))
                       snapshot-machine-mismatch))

(fiveam:test snapshot-rejects-malformed-payloads
  (let ((good (machine-snapshot (%fresh))))
    (%rejected-untouched '(:not-a-snapshot) snapshot-malformed)
    (%rejected-untouched (%with-field good :elements (remove 'a (getf (cdr good) :elements)
                                                             :key #'car))
                         snapshot-malformed)
    (%rejected-untouched (%with-field good :cycles -1) snapshot-malformed)
    (%rejected-untouched
     (%with-field good :elements
                  (substitute (list 'a :cells '(256)) 'a (getf (cdr good) :elements) :key #'car))
     snapshot-malformed)
    (%rejected-untouched
     (%with-field good :elements
                  (substitute (list 'ram :runs '((10 . 0))) 'ram (getf (cdr good) :elements)
                              :key #'car))
     snapshot-malformed)))

;;; Files

(fiveam:test snapshot-file-round-trip
  (uiop:with-temporary-file (:pathname path :type "snap")
    (let ((source (%dirty)) (target (%fresh)))
      (write-snapshot (machine-snapshot source) path)
      (restore-snapshot target (read-snapshot path))
      (fiveam:is (equal (%state-of source) (%state-of target)))
      (fiveam:is (equal '(99) (device-state (device-at target 0)))))))

(fiveam:test snapshot-file-read-never-evaluates
  (uiop:with-temporary-file (:pathname path :type "snap")
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(:lasm-snapshot :version #.(error \"evaluated\"))" out))
    (fiveam:signals snapshot-malformed (read-snapshot path))))

(fiveam:test snapshot-file-rejects-non-snapshots
  (uiop:with-temporary-file (:pathname path :type "snap")
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(1 2 3)" out))
    (fiveam:signals snapshot-malformed (read-snapshot path))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string "(:lasm-snapshot" out))
    (fiveam:signals snapshot-malformed (read-snapshot path))))

;;; Coverage guard

(fiveam:test snapshot-covers-every-machine-slot
  (let ((covered '(cycles extra-cycles idle devices interrupt-queue banks loaded-banks region-bindings))
        (host-only '(descriptor slots interrupt-hook access-hook program program-memory program-offset)))
    (dolist (slot (closer-mop:class-slots (find-class 'machine)))
      (let ((name (closer-mop:slot-definition-name slot)))
        (fiveam:is (or (member (symbol-name name) covered :test #'string=)
                       (member (symbol-name name) host-only :test #'string=))
                   "machine slot ~S is neither snapshotted nor listed as host-only" name)))))

;;; Banked regions

(defmachine snapshot-bank-machine
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8
    (region window 16 31 :banks 3)))

(defmachine snapshot-other-bank-machine
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8
    (region window 16 31 :banks 4)))

(fiveam:test snapshot-round-trips-banks
  (let ((source (make-machine 'snapshot-bank-machine))
        (target (make-machine 'snapshot-bank-machine)))
    (setf (bank-peek source 'window 0 16) 1
          (bank-peek source 'window 2 31) 9
          (current-bank source 'window) 2)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (= 2 (current-bank target 'window)))
    (fiveam:is (= 1 (bank-peek target 'window 0 16)))
    (fiveam:is (= 9 (mref target 'ram 31)))))

(fiveam:test snapshot-rejects-changed-bank-layout
  (let ((snapshot (machine-snapshot (make-machine 'snapshot-other-bank-machine)))
        (target (make-machine 'snapshot-bank-machine)))
    (fiveam:signals snapshot-machine-mismatch (restore-snapshot target snapshot))))

(fiveam:test snapshot-rejects-bad-bank-state-untouched
  (let* ((good (machine-snapshot (make-machine 'snapshot-bank-machine)))
         (bad (%with-field good :banks
                           (list (list 'window :current 3
                                       :banks (getf (cdr (first (getf (cdr good) :banks))) :banks)))))
         (target (make-machine 'snapshot-bank-machine)))
    (setf (mref target 'ram 0) 5)
    (fiveam:signals snapshot-malformed (restore-snapshot target bad))
    (fiveam:is (= 5 (mref target 'ram 0)))))

(fiveam:test snapshot-round-trips-the-loaded-bank
  (let ((m (make-machine 'snapshot-bank-machine))
        (other (make-machine 'snapshot-bank-machine)))
    (setf (current-bank m 'window) 2)
    (load-program m (make-array 20 :initial-element 1) :origin 10)
    (restore-snapshot other (machine-snapshot m))
    (fiveam:is (eql 2 (gethash 'window (machine-loaded-banks other))))
    (reset other)
    (restore-snapshot other (machine-snapshot (make-machine 'snapshot-bank-machine)))
    (fiveam:is (null (gethash 'window (machine-loaded-banks other))))))

(fiveam:test snapshot-without-cells-restores-small-state-and-keeps-memory
  (let ((m (%dirty))
        (other (%fresh)))
    (setf (mref other 'ram 5) 99)
    (%restore-snapshot other (%machine-snapshot m nil) nil)
    (fiveam:is (= 42 (sref other 'a)))
    (fiveam:is (= 99 (mref other 'ram 5)))
    (fiveam:is (null (search ":RUNS" (prin1-to-string (%machine-snapshot m nil)))))))
