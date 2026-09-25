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
      (fiveam:is (equal '(:kind :instruction :name "RTE" :address nil :required supervisor)
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

;;; #301: interrupt delivery and the privilege level

;; The stack and handler live in a supervisor-gated region.
(defmacro %def-priv-int-machine (name &rest interrupt-keys)
  `(progn
     (defmachine ,name
       (register pc :width 8) (register ia :width 8) (register a :width 8)
       (register sp :width 8) (register b :width 8)
       (memory ram :width 8 :addr-width 8
         (region kernel #x00 #x3f :privilege supervisor))
       (stack-pointer sp :memory ram)
       (flags s)
       (privilege :level s :levels (user supervisor))
       (interrupts :vector ia :message a :save (pc s) :stack sp ,@interrupt-keys))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))))

(%def-priv-int-machine priv-int-machine :deliver-level supervisor)
(%def-priv-int-machine priv-int-plain-machine)

(defun %priv-int-machine (&optional (name 'priv-int-machine))
  (let ((m (make-machine name)))
    (load-program m (list #x00 #x01) :origin #x10)
    (setf (sref m 'ia) #x10 (sref m 'pc) #x80 (sref m 'sp) #x40)
    m))

(fiveam:test deliver-level-runs-the-handler-at-that-level
  (let ((m (%priv-int-machine)))
    (signal-interrupt m 5)
    (step-machine m)
    (fiveam:is (eq 'supervisor (privilege-level m)))
    (fiveam:is (= #x11 (sref m 'pc)))
    (fiveam:is (= #x3e (sref m 'sp)))))

(fiveam:test interrupt-return-restores-the-saved-level-last
  (let ((m (%priv-int-machine)))
    (signal-interrupt m 5)
    (step-machine m)
    (step-machine m)
    (fiveam:is (eq 'user (privilege-level m)))
    (fiveam:is (= #x80 (sref m 'pc)))
    (fiveam:is (= #x40 (sref m 'sp)))))

(fiveam:test delivery-without-deliver-level-faults-at-user-level
  (let ((m (%priv-int-machine 'priv-int-plain-machine)))
    (signal-interrupt m 5)
    (fiveam:signals privilege-violation (step-machine m))))

(fiveam:test delivery-violation-carries-the-interrupted-pc
  (let ((m (%priv-int-machine 'priv-int-plain-machine)))
    (signal-interrupt m 5)
    (handler-case (step-machine m)
      (privilege-violation (c)
        (fiveam:is (= #x80 (runtime-location-pc c)))))))

(fiveam:test run-reports-a-delivery-violation-as-a-fault
  (let ((m (%priv-int-machine 'priv-int-plain-machine)))
    (signal-interrupt m 5)
    (fiveam:is (eq :fault (run m :max-steps 3)))))

(fiveam:test deliver-level-needs-a-known-privilege-level
  (%priv-rejects (interrupts :vector a :message a :save (pc) :stack sp :deliver-level nosuch)
                 (stack sp :width 8 :depth 4))
  (fiveam:signals machine-definition-error
    (eval '(defmachine priv-bad-deliver
            (register pc :width 8) (register a :width 8)
            (stack sp :width 8 :depth 4) (memory ram :width 8 :addr-width 8)
            (interrupts :vector a :message a :save (pc) :deliver-level supervisor)))))

;;; #300: registers, flags and stacks

(defmacro %def-priv-gate-machine (name &rest privilege-keys)
  `(progn
     (defmachine ,name
       (register pc :width 8)
       (register a :width 8)
       (register cr :width 8 :privilege supervisor)
       (register bank :width 8 :names (b0 b1) :privilege supervisor)
       (stack ks :width 8 :depth 4 :privilege supervisor)
       (stack us :width 8 :depth 4)
       (memory ram :width 8 :addr-width 8)
       (flags s (ie :privilege supervisor) z)
       (privilege :level s :levels (user supervisor) ,@privilege-keys))
     (definstruction ,name rd-cr (encoding (opcode #x01)) (semantics (set! a cr)))
     (definstruction ,name wr-cr (encoding (opcode #x02)) (semantics (set! cr 9)))
     (definstruction ,name rd-b (encoding (opcode #x03)) (semantics (set! a b1)))
     (definstruction ,name wr-b (encoding (opcode #x04)) (semantics (set! (bank 1) 9)))
     (definstruction ,name rd-ie (encoding (opcode #x05)) (semantics (set! a ie)))
     (definstruction ,name wr-ie (encoding (opcode #x06)) (semantics (set-flags! (ie 1))))
     (definstruction ,name push-ks (encoding (opcode #x07)) (semantics (push 3 ks)))
     (definstruction ,name pop-ks (encoding (opcode #x08)) (semantics (set! a (pop ks))))
     (definstruction ,name push-us (encoding (opcode #x09)) (semantics (push 3 us)))
     (definstruction ,name wr-z (encoding (opcode #x0a)) (semantics (set-flags! (z 1))))))

(%def-priv-gate-machine priv-gate-machine)
(%def-priv-gate-machine priv-gate-trap-machine :on-violation :trap)

(defun %priv-gate-step (name opcode level)
  (let ((m (make-machine name)))
    (setf (sref m 's) level)
    (load-program m (list opcode))
    (values m (handler-case (progn (step-machine m) nil)
                (privilege-violation (c) c)))))

(fiveam:test gated-elements-fault-from-semantics-at-user-level
  (loop for (opcode kind name) in '((#x01 :register cr) (#x02 :register cr)
                                    (#x03 :register bank) (#x04 :register bank)
                                    (#x05 :flag ie) (#x06 :flag ie)
                                    (#x07 :stack ks) (#x08 :stack ks))
        do (let ((c (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 0))))
             (fiveam:is (typep c 'privilege-violation))
             (fiveam:is (eq kind (privilege-violation-kind c)))
             (fiveam:is (eq name (storage-error-name c)))
             (fiveam:is (eq 'supervisor (privilege-violation-required c))))))

(fiveam:test gated-elements-pass-at-the-required-level
  (dolist (opcode '(#x01 #x02 #x03 #x04 #x05 #x06 #x07))
    (fiveam:is (null (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 1))))))

(fiveam:test ungated-elements-stay-open-at-user-level
  (dolist (opcode '(#x09 #x0a))
    (fiveam:is (null (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 0))))))

(fiveam:test gated-element-violation-leaves-the-element-unchanged
  (let ((m (%priv-gate-step 'priv-gate-machine #x02 0)))
    (fiveam:is (= 0 (sref m 'cr)))))

(fiveam:test host-access-bypasses-element-gates
  (let ((m (make-machine 'priv-gate-machine)))
    (setf (sref m 'cr) 5 (flag m 'ie) t (regref m 'bank 1) 6)
    (stack-push m 'ks 7)
    (fiveam:is (= 5 (sref m 'cr)))
    (fiveam:is (= 7 (stack-pop m 'ks)))
    (fiveam:is (eq 'user (privilege-level m)))))

(fiveam:test gated-element-trap-data-names-the-kind
  (let ((m (make-machine 'priv-gate-trap-machine)))
    (load-program m (list #x01))
    (multiple-value-bind (result steps condition) (run m :max-steps 1)
      (declare (ignore steps))
      (fiveam:is (eq :trap result))
      (fiveam:is (equal '(:kind :register :name cr :address nil :required supervisor)
                        (lasm-trap-data condition))))))

(fiveam:test debugger-write-bypasses-element-gates
  (let ((m (make-machine 'priv-gate-machine)))
    (fiveam:finishes (let ((*privilege-checks* nil)) (%check-privilege m 'supervisor 'cr nil :register)))))

(fiveam:test element-privilege-needs-a-privilege-clause-and-a-known-level
  (%priv-rejects (register g :width 8 :privilege supervisor))
  (%priv-rejects (privilege :level s :levels (user supervisor))
                 (register g :width 8 :privilege admin))
  (%priv-rejects (privilege :level s :levels (user supervisor))
                 (stack g :width 8 :depth 2 :privilege admin))
  (%priv-rejects (privilege :level s :levels (user supervisor))
                 (flags (g :privilege admin)))
  (%priv-rejects (privilege :level s :levels (user supervisor))
                 (register g :width 8 :privilege "supervisor")))

(fiveam:test child-machine-cannot-change-an-inherited-element-gate
  (fiveam:signals machine-definition-error
    (eval '(defmachine (priv-bad-gate-child (:extends priv-gate-machine))
            (register cr :width 8 :privilege user)))))

(defmachine priv-rfi-gate-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (register cr :width 8 :privilege supervisor)
  (stack st :width 8 :depth 8)
  (memory ram :width 8 :addr-width 8)
  (flags s)
  (privilege :level s :levels (user supervisor))
  (interrupts :vector ia :message a :save (pc cr)))

(definstruction priv-rfi-gate-machine nop (encoding (opcode #x00)) (semantics nil))
(definstruction priv-rfi-gate-machine rfi (encoding (opcode #x01)) (semantics (interrupt-return)))

(fiveam:test interrupt-return-restores-a-gated-register-at-user-level
  (let ((m (make-machine 'priv-rfi-gate-machine)))
    (load-program m (list #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
    (setf (sref m 'pc) 0 (sref m 'ia) 16 (sref m 'cr) 42)
    (signal-interrupt m 1)
    (step-machine m)
    (setf (sref m 'cr) 0)
    (fiveam:finishes (step-machine m))
    (fiveam:is (= 42 (sref m 'cr)))))

;;; #302: violations that raise an interrupt

(defmacro %def-priv-irq-machine (name &rest privilege-keys)
  `(progn
     (defmachine ,name
       (register pc :width 8) (register ia :width 8) (register a :width 8)
       (register cr :width 8 :privilege supervisor)
       (stack st :width 8 :depth 8)
       (memory ram :width 8 :addr-width 8)
       (flags s)
       (privilege :level s :levels (user supervisor) ,@privilege-keys)
       (interrupts :vector ia :message a :save (pc s) :deliver-level supervisor))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil) (cycles 1))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
     (definstruction ,name rd-cr (encoding (opcode #x02)) (semantics (set! a cr)) (cycles 2))
     (definstruction ,name sup (privilege supervisor) (encoding (opcode #x03)) (semantics nil))))

(%def-priv-irq-machine priv-irq-machine :on-violation (:interrupt 7 3))
(%def-priv-irq-machine priv-irq-fault-machine :on-violation (:interrupt 7))

(defun %priv-irq-machine (program &key (name 'priv-irq-machine) (vector 32))
  (let ((m (make-machine name)))
    (load-program m program)
    (load-program m (list #x00 #x01) :origin 32)
    (setf (sref m 'pc) 0 (sref m 'ia) vector)
    m))

(fiveam:test violation-interrupt-aborts-the-instruction-and-delivers-next-step
  (let ((m (%priv-irq-machine (list #x02))))
    (fiveam:is (eq :privilege-violation (step-machine m)))
    (fiveam:is (= 0 (sref m 'pc)))
    (fiveam:is (= 0 (sref m 'a)))
    (fiveam:is (= 1 (length (machine-interrupt-queue m))))
    (step-machine m)
    (fiveam:is (= 7 (sref m 'a)))
    (fiveam:is (eq 'supervisor (privilege-level m)))
    (fiveam:is (= 33 (sref m 'pc)))))

(fiveam:test violation-interrupt-handler-returns-to-the-violating-instruction
  (let ((m (%priv-irq-machine (list #x02))))
    (step-machine m)
    (step-machine m)
    (step-machine m)
    (fiveam:is (eq 'user (privilege-level m)))
    (fiveam:is (= 0 (sref m 'pc)))))

(fiveam:test violation-interrupt-records-the-details
  (let ((m (%priv-irq-machine (list #x00 #x02))))
    (step-machine m)
    (step-machine m)
    (fiveam:is (equal '(:pc 1 :kind :register :name cr :address nil :required supervisor :current user)
                      (privilege-violation-info m)))))

(fiveam:test violation-interrupt-covers-instruction-gates-without-cost
  (let ((m (%priv-irq-machine (list #x03))))
    (multiple-value-bind (result cost) (step-machine m)
      (fiveam:is (eq :privilege-violation result))
      (fiveam:is (= 0 cost))
      (fiveam:is (= 0 (sref m 'pc)))
      (fiveam:is (eq :instruction (getf (privilege-violation-info m) :kind))))))

(fiveam:test violation-interrupt-charges-cycles-spent-before-the-violation
  (let ((m (%priv-irq-machine (list #x02))))
    (multiple-value-bind (result cost) (step-machine m)
      (declare (ignore result))
      (fiveam:is (= 2 cost)))))

(fiveam:test violation-interrupt-honours-priority
  (let ((m (%priv-irq-machine (list #x02))))
    (step-machine m)
    (fiveam:is (= 3 (third (first (machine-interrupt-queue m)))))))

(fiveam:test violation-interrupt-falls-back-to-a-fault-when-the-signal-is-dropped
  (let ((m (%priv-irq-machine (list #x02) :vector 0)))
    (fiveam:signals privilege-violation (step-machine m))
    (fiveam:is (null (machine-interrupt-queue m)))
    (fiveam:is (null (privilege-violation-info m)))))

(fiveam:test violation-interrupt-outside-a-step-faults
  (let ((m (%priv-irq-machine (list #x00))))
    (fiveam:signals privilege-violation (%gated-sref m 'cr 'supervisor))
    (fiveam:is (null (machine-interrupt-queue m)))))

(fiveam:test violation-interrupt-run-keeps-going
  (let ((m (%priv-irq-machine (list #x02))))
    (fiveam:is (eq :max-steps (run m :max-steps 6)))
    (fiveam:is (= 7 (sref m 'a)))))

(fiveam:test reset-clears-the-violation-info
  (let ((m (%priv-irq-machine (list #x02))))
    (step-machine m)
    (reset m)
    (fiveam:is (null (privilege-violation-info m)))))

(fiveam:test interrupt-violation-policy-needs-an-interrupts-clause-and-fitting-data
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt 1)))
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt 300))
                 (register ia :width 8)
                 (interrupts :vector ia :message a :save (pc) :stack st)
                 (stack st :width 8 :depth 2))
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt -1)))
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt)))
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt 1 2 3))))
