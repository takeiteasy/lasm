;;;; tests/defdevice.lisp
;;;; fiveam tests for DEFDEVICE, a CPU's (devices ...) clause and ATTACH-DEVICE :DEVICE.
;;;; Reuses tests/device.lisp's counter and latch hooks.

(in-package #:lasm)

(fiveam:def-suite defdevice :in lasm)
(fiveam:in-suite defdevice)

(defun %dd-save (machine device)
  (declare (ignore machine))
  (copy-list (device-state device)))

(defun %dd-load (machine device data)
  (declare (ignore machine))
  (setf (device-state device) (copy-list data)))

(defdevice dd-clock :id 1 :version 1 :manufacturer 7
  :init %counter-init :tick %counter-tick :save %dd-save :load %dd-load)
(defdevice dd-serial :id #x5E7 :version 2 :init %counter-init :receive %counter-receive)
(defdevice dd-latch :id 9 :init %latch-init :read %latch-read :write %latch-write)
(defdevice dd-extra :id 3)

(defisa dd-isa
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16))

(defcpu (dd-cpu (:isa dd-isa))
  (device before :id 40)
  (devices dd-clock (com1 :device dd-serial) (com2 :device dd-serial :id 7))
  (device after :id 41))

(defun %dd-names (machine)
  (loop for i below (device-count machine)
        collect (device-name-of (device-at machine i))))

(defun device-name-of (device)
  (device-descriptor-name (device-descriptor device)))

(fiveam:test devices-entries-join-the-bus-in-clause-order
  (let ((m (make-machine 'dd-cpu)))
    (fiveam:is (equal '(before dd-clock com1 com2 after) (%dd-names m)))
    (fiveam:is (equal '(1 1 7) (multiple-value-list (device-info m 1))))
    (fiveam:is (equal '(#x5E7 2 0) (multiple-value-list (device-info m 2))))))

(fiveam:test an-entry-overrides-keywords-per-attachment
  (let ((m (make-machine 'dd-cpu)))
    (fiveam:is (= 7 (device-info m 3)))
    (fiveam:is (= #x5E7 (device-descriptor-id (find-device-definition 'dd-serial))))))

(fiveam:test one-definition-attached-twice-keeps-separate-state
  (let ((m (make-machine 'dd-cpu)))
    (fiveam:is (not (eq (device-state (find-device m 'com1)) (device-state (find-device m 'com2)))))
    (device-send m 2)
    (fiveam:is (= 1 (cdr (device-state (find-device m 'com1)))))
    (fiveam:is (= 0 (cdr (device-state (find-device m 'com2)))))))

(fiveam:test hooks-of-a-defined-device-run
  (let ((m (make-machine 'dd-cpu)))
    (tick-devices m 5)
    (fiveam:is (= 5 (car (device-state (find-device m 'dd-clock)))))))

(fiveam:test reset-restores-entries-and-drops-attachments
  (let ((m (make-machine 'dd-cpu)))
    (tick-devices m 5)
    (attach-device m 'extra :device 'dd-extra)
    (reset m)
    (fiveam:is (equal '(before dd-clock com1 com2 after) (%dd-names m)))
    (fiveam:is (= 0 (car (device-state (find-device m 'dd-clock)))))))

(defcpu (dd-child (:extends dd-cpu))
  (device after :id 42)
  (devices (com1 :id 99) dd-extra)
  (device late :id 43))

(fiveam:test a-child-merges-entries-in-place-and-appends-new-ones
  (let ((m (make-machine 'dd-child)))
    (fiveam:is (equal '(before dd-clock com1 com2 after dd-extra late) (%dd-names m)))
    (fiveam:is (= 99 (device-info m 2)))
    (fiveam:is (= 2 (nth-value 1 (device-info m 2))))
    (fiveam:is (= 42 (device-info m 4)))))

(defcpu (dd-lean (:extends dd-cpu))
  (without-devices com1 dd-clock))

(fiveam:test without-devices-drops-entries
  (fiveam:is (equal '(before com2 after) (%dd-names (make-machine 'dd-lean)))))

(defcpu (dd-mapped (:isa dd-isa))
  (memory ram (region io #x40 #x4F :kind :device :device port))
  (devices (port :device dd-latch)))

(fiveam:test a-region-binds-to-an-entry
  (let ((m (make-machine 'dd-mapped)))
    (fiveam:is (= 5 (mref m 'ram #x41)))
    (setf (mref m 'ram #x40) 7)
    (fiveam:is (= 7 (mref m 'ram #x42)))))

(fiveam:test attach-device-copies-a-definition
  (let* ((m (make-machine 'dd-cpu))
         (index (attach-device m 'com3 :device 'dd-serial :id 11)))
    (fiveam:is (= 5 index))
    (fiveam:is (equal '(11 2 0) (multiple-value-list (device-info m index))))
    (fiveam:signals emulator-usage-error (attach-device m 'com3 :device 'dd-serial))
    (fiveam:signals unknown-device-definition (attach-device m 'com4 :device 'dd-missing))))

(fiveam:test declared-entries-survive-a-snapshot
  (let ((source (make-machine 'dd-cpu))
        (target (make-machine 'dd-cpu)))
    (tick-devices source 9)
    (restore-snapshot target (machine-snapshot source))
    (fiveam:is (= 9 (car (device-state (find-device target 'dd-clock)))))))

(fiveam:test an-attached-definition-round-trips-on-its-own-machine
  (let ((m (make-machine 'dd-cpu)))
    (attach-device m 'com3 :device 'dd-serial)
    (device-send m 5)
    (restore-snapshot m (machine-snapshot m))
    (fiveam:is (= 6 (device-count m)))))

(fiveam:test defdevice-rejects-bad-keys
  (fiveam:signals machine-definition-error (eval '(defdevice dd-bad :id -1)))
  (fiveam:signals machine-definition-error (eval '(defdevice dd-bad :bogus 1)))
  (fiveam:signals machine-definition-error (eval '(defdevice "dd-bad")))
  (fiveam:is (null (gethash 'dd-bad *device-definitions*))))

(fiveam:test devices-rejects-bad-entries
  (dolist (entry '(dd-missing (com :device dd-missing) (com :device dd-serial :bogus 1)
                   (com :device dd-serial :id) (com :device dd-serial :id -3) (1 :id 2)))
    (fiveam:signals machine-definition-error
      (eval `(defcpu (dd-bad (:isa dd-isa)) (devices ,entry))))
    (fiveam:is (null (gethash 'dd-bad *machines*)))))

(fiveam:test device-names-share-one-namespace
  (fiveam:signals machine-definition-error
    (eval '(defcpu (dd-bad (:isa dd-isa)) (device dd-clock :id 1) (devices dd-clock))))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (dd-bad (:isa dd-isa)) (devices dd-clock dd-clock))))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (dd-bad (:isa dd-isa)) (devices (a :device dd-clock)) (register a :width 8)))))

(fiveam:test a-child-cannot-cross-inline-and-defined-devices
  (fiveam:signals machine-definition-error
    (eval '(defcpu (dd-bad (:extends dd-cpu)) (device com1 :id 5))))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (dd-bad (:extends dd-cpu)) (devices (before :device dd-extra))))))

(fiveam:test defisa-rejects-devices
  (fiveam:signals machine-definition-error
    (eval '(defisa dd-bad-isa (register pc :width 8) (memory ram :width 8 :addr-width 8) (devices dd-clock)))))
