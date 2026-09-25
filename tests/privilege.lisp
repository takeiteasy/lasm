;;;; tests/privilege.lisp
;;;; #111: (privilege ...) levels, region :privilege gates, instruction
;;;; (privilege LEVEL) gates, and the :fault/:trap violation policy.

(in-package #:lasm)

(fiveam:def-suite privilege :in lasm)
(fiveam:in-suite privilege)

;;; Fixtures

(defmachine priv-machine
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16
    (region kernel #x0000 #x00ff :privilege supervisor)
    (region rom #x0100 #x01ff :kind :rom :privilege supervisor)
    (region user #x0200 #xffff))
  (flags s)
  (privilege :level s :levels (user supervisor)))

(definstruction priv-machine nop
  (encoding (opcode #x01))
  (semantics nil))

(definstruction priv-machine rte
  (privilege supervisor)
  (encoding (opcode #x02))
  (semantics (set! a 7)))

(defmachine priv-trap-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags s)
  (privilege :level s :levels (user supervisor) :on-violation :trap))

(definstruction priv-trap-machine rte
  (privilege supervisor)
  (encoding (opcode #x02))
  (semantics nil))

;; x86-style: ring 0 is the most privileged and holds the lowest value.
(defmachine priv-ring-machine
  (register pc :width 16)
  (register cpl :width 8)
  (memory ram :width 8 :addr-width 8
    (region kernel #x00 #x0f :privilege ring0)
    (region drivers #x10 #x1f :privilege ring1))
  (privilege :level cpl :levels ((ring3 3) (ring2 2) (ring1 1) (ring0 0))))

(defmachine (priv-child (:extends priv-machine))
  (properties :child t))

(defun %priv-machine (&optional (name 'priv-machine) (level 0))
  (let ((m (make-machine name)))
    (setf (sref m (if (eq name 'priv-ring-machine) 'cpl 's)) level)
    m))

;;; Definition errors

(defmacro %priv-rejects (&rest clauses)
  `(fiveam:signals machine-definition-error
     (eval '(defmachine priv-bad-machine
              (register pc :width 8) (register a :width 8 :count 4)
              (memory ram :width 8 :addr-width 8) (flags s)
              ,@clauses))))

(fiveam:test privilege-rejects-unknown-level-storage
  (%priv-rejects (privilege :level nosuch :levels (user supervisor))))

(fiveam:test privilege-rejects-banked-level-register
  (%priv-rejects (privilege :level a :levels (user supervisor))))

(fiveam:test privilege-rejects-duplicate-levels-and-values
  (%priv-rejects (privilege :level s :levels (user user)))
  (%priv-rejects (privilege :level s :levels ((user 1) (supervisor 1)))))

(fiveam:test privilege-rejects-value-that-does-not-fit
  (%priv-rejects (privilege :level s :levels ((user 0) (supervisor 2)))))

(fiveam:test privilege-rejects-bad-policy-and-missing-keys
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation :ignore))
  (%priv-rejects (privilege :level s))
  (%priv-rejects (privilege :levels (user supervisor))))

(fiveam:test privilege-rejects-a-second-clause
  (%priv-rejects (privilege :level s :levels (user supervisor))
                 (privilege :level s :levels (user supervisor))))

(fiveam:test region-privilege-needs-a-clause-and-a-declared-level
  (fiveam:signals machine-definition-error
    (eval '(defmachine priv-no-clause-machine
            (register pc :width 8)
            (memory ram :width 8 :addr-width 8 (region k 0 15 :privilege supervisor)))))
  (%priv-rejects (memory ram2 :width 8 :addr-width 8 (region k 0 15 :privilege nosuch))
                 (privilege :level s :levels (user supervisor))))

(fiveam:test instruction-privilege-needs-a-declared-level
  (fiveam:signals instruction-definition-error
    (eval '(definstruction priv-machine bad-level
            (privilege nosuch) (encoding (opcode #x03)) (semantics nil))))
  (fiveam:signals instruction-definition-error
    (eval '(definstruction test-machine bad-clause
            (privilege supervisor) (encoding (opcode #x03)) (semantics nil)))))

;;; Region gates

(fiveam:test region-gate-faults-reads-and-writes-below-the-level
  (let ((m (%priv-machine)))
    (fiveam:signals privilege-violation (mref m 'ram #x10))
    (fiveam:signals privilege-violation (setf (mref m 'ram #x10) 1))
    (fiveam:signals privilege-violation (setf (mref m 'ram #x110) 1))
    (fiveam:is (= 3 (setf (mref m 'ram #x300) 3)))
    (fiveam:is (= 3 (mref m 'ram #x300)))))

(fiveam:test region-gate-allows-the-required-level
  (let ((m (%priv-machine 'priv-machine 1)))
    (setf (mref m 'ram #x10) 9)
    (fiveam:is (= 9 (mref m 'ram #x10)))))

(fiveam:test privilege-violation-reports-what-was-required
  (let ((m (%priv-machine)))
    (handler-case (mref m 'ram #x10)
      (privilege-violation (c)
        (fiveam:is (= #x10 (privilege-violation-address c)))
        (fiveam:is (eq 'supervisor (privilege-violation-required c)))
        (fiveam:is (eq 'user (privilege-violation-current c)))
        (fiveam:is (search "requires privilege SUPERVISOR" (princ-to-string c)))))))

(fiveam:test rejected-access-is-not-reported-to-the-access-hook
  (let ((m (%priv-machine)) (seen nil))
    (setf (machine-access-hook m) (lambda (&rest args) (cl:push args seen)))
    (ignore-errors (mref m 'ram #x10))
    (fiveam:is (null seen))))

(fiveam:test region-gate-is-bypassed-by-inspection-and-loading
  (let ((m (%priv-machine)))
    (%poke m 'ram #x10 5)
    (fiveam:is (= 5 (mpeek m 'ram #x10)))
    (load-program m (assemble "nop" :machine 'priv-machine) :origin #x20)
    (fiveam:is (= 1 (mpeek m 'ram #x20)))))

(fiveam:test fetch-from-a-gated-region-faults
  (let ((m (%priv-machine)))
    (%poke m 'ram #x10 1)
    (setf (sref m 'pc) #x10)
    (fiveam:signals privilege-violation (step-machine m))
    (setf (sref m 's) 1)
    (fiveam:is (eq 'nop (intern (instruction-descriptor-name (step-machine m)))))))

(fiveam:test rom-region-can-be-gated-too
  (let ((m (%priv-machine)))
    (fiveam:signals privilege-violation (setf (mref m 'ram #x110) 1))
    (setf (sref m 's) 1)
    (fiveam:is (= 0 (setf (mref m 'ram #x110) 0)))))

(fiveam:test level-values-need-not-ascend
  (let ((m (%priv-machine 'priv-ring-machine 3)))
    (fiveam:is (eq 'ring3 (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram #x18))
    (setf (sref m 'cpl) 1)
    (fiveam:is (eq 'ring1 (privilege-level m)))
    (fiveam:is (= 0 (mref m 'ram #x18)))
    (fiveam:signals privilege-violation (mref m 'ram #x08))
    (setf (sref m 'cpl) 0)
    (fiveam:is (= 0 (mref m 'ram #x08)))))

(fiveam:test unlisted-level-value-ranks-below-every-level
  (let ((m (%priv-machine 'priv-ring-machine 9)))
    (fiveam:is (null (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram #x18))))

(fiveam:test privilege-level-is-nil-without-a-clause
  (fiveam:is (null (privilege-level (make-machine 'test-machine)))))

;;; Instruction gates

(defun %priv-run (name level source)
  (let ((m (%priv-machine name level)))
    (load-program m (assemble source :machine name :origin #x200))
    m))

(fiveam:test instruction-gate-faults-before-executing
  (let ((m (%priv-run 'priv-machine 0 "rte")))
    (handler-case (step-machine m)
      (privilege-violation (c)
        (fiveam:is (null (privilege-violation-address c)))
        (fiveam:is (string= "RTE" (storage-error-name c)))
        (fiveam:is (= #x200 (runtime-location-pc c)))
        (fiveam:is (search "Instruction RTE requires privilege" (princ-to-string c)))))
    (fiveam:is (= #x200 (sref m 'pc)))
    (fiveam:is (= 0 (machine-cycles m)))
    (fiveam:is (= 0 (sref m 'a)))))

(fiveam:test instruction-gate-passes-at-the-required-level
  (let ((m (%priv-run 'priv-machine 1 "rte")))
    (step-machine m)
    (fiveam:is (= 7 (sref m 'a)))))

(fiveam:test run-returns-fault-for-a-violation
  (let ((m (%priv-run 'priv-machine 0 "nop
rte")))
    (multiple-value-bind (reason steps condition) (run m)
      (fiveam:is (eq :fault reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (typep condition 'privilege-violation)))))

(fiveam:test trap-policy-returns-a-trap-with-the-tag
  (let ((m (%priv-run 'priv-trap-machine 0 "rte")))
    (multiple-value-bind (reason steps condition) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (eq :privilege-violation (lasm-trap-tag condition)))
      (fiveam:is (equal '(:name "RTE" :address nil :required supervisor)
                        (lasm-trap-data condition)))
      (fiveam:is (= #x200 (sref m 'pc))))))

;;; Debugger and CLI

(fiveam:test debug-write-bypasses-the-region-gate
  (let* ((m (%priv-machine))
         (session (make-debug-session m)))
    (fiveam:is (= 5 (debug-write session #x10 5)))
    (fiveam:is (= 5 (mpeek m 'ram #x10)))
    (fiveam:signals privilege-violation (mref m 'ram #x10))))

(fiveam:test cli-run-privilege-trap-exits-one
  (multiple-value-bind (status out err)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/privilege.asm")
                      "-m" (%cli-path "tests/fixtures/cli/privilege.lasm")))
    (fiveam:is (= 1 status))
    (fiveam:is (search "stopped: trap after 1 step, pc = $0000" out))
    (fiveam:is (string= "" err))))

;;; Families and snapshots

(fiveam:test child-machine-inherits-the-privilege-model
  (let ((m (%priv-machine 'priv-child)))
    (fiveam:is (eq 'user (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram #x10))
    (load-program m (assemble "rte" :machine 'priv-child :origin #x200))
    (fiveam:signals privilege-violation (step-machine m))))

(fiveam:test child-machine-cannot-change-inherited-levels
  (fiveam:signals machine-definition-error
    (eval '(defmachine (priv-bad-child (:extends priv-machine))
            (privilege :levels (user admin supervisor))))))

(fiveam:test snapshot-round-trips-the-level
  (let ((m (%priv-machine 'priv-machine 1)))
    (let ((restored (make-machine 'priv-machine)))
      (restore-snapshot restored (machine-snapshot m))
      (fiveam:is (eq 'supervisor (privilege-level restored))))))
