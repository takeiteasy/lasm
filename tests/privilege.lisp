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
      (fiveam:is (equal '(:kind :instruction :name "RTE" :address nil :required supervisor :access nil :mask nil)
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
                      "-m" (%cli-path "tests/fixtures/cli/privilege.lisp")))
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
     (definstruction ,name wr-z (encoding (opcode #x0a)) (semantics (set-flags! (z 1))))
     (definstruction ,name x-rd-cr (encoding (opcode #x0b)) (semantics (set! a (sref machine 'cr))))
     (definstruction ,name x-wr-cr (encoding (opcode #x0c)) (semantics (setf (sref machine 'cr) 9)))
     (definstruction ,name x-inc-cr (encoding (opcode #x0d)) (semantics (incf (sref machine 'cr))))
     (definstruction ,name x-wr-b (encoding (opcode #x0e)) (semantics (setf (regref machine 'bank 1) 9)))
     (definstruction ,name x-wr-ie (encoding (opcode #x0f)) (semantics (setf (flag machine 'ie) 1)))
     (definstruction ,name x-push-ks (encoding (opcode #x10)) (semantics (stack-push machine 'ks 3)))
     (definstruction ,name x-pop-ks (encoding (opcode #x11)) (semantics (set! a (stack-pop machine 'ks))))
     (definstruction ,name x-dyn-cr (encoding (opcode #x12))
       (semantics (let ((n 'cr)) (set! a (sref machine n)))))
     (definstruction ,name x-wr-a (encoding (opcode #x13)) (semantics (setf (sref machine 'a) 1)))
     (definstruction ,name x-rd-z (encoding (opcode #x14)) (semantics (set! a (flag machine 'z))))
     (definstruction ,name x-push-us (encoding (opcode #x15)) (semantics (stack-push machine 'us 3)))))

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

;;; #307: explicit accessor calls in semantics

(fiveam:test explicit-accessors-fault-from-semantics-at-user-level
  (loop for (opcode kind name) in '((#x0b :register cr) (#x0c :register cr) (#x0d :register cr)
                                    (#x0e :register bank) (#x0f :flag ie)
                                    (#x10 :stack ks) (#x11 :stack ks) (#x12 :register cr))
        do (let ((c (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 0))))
             (fiveam:is (typep c 'privilege-violation))
             (fiveam:is (eq kind (privilege-violation-kind c)))
             (fiveam:is (eq name (storage-error-name c))))))

(fiveam:test explicit-accessors-pass-at-the-required-level
  (dolist (opcode '(#x0b #x0c #x0d #x0e #x0f #x10 #x12))
    (fiveam:is (null (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 1))))))

(fiveam:test explicit-accessors-on-ungated-elements-stay-open
  (dolist (opcode '(#x13 #x14 #x15))
    (fiveam:is (null (nth-value 1 (%priv-gate-step 'priv-gate-machine opcode 0))))))

(fiveam:test explicit-accessor-violation-leaves-the-element-unchanged
  (fiveam:is (= 0 (sref (%priv-gate-step 'priv-gate-machine #x0c 0) 'cr)))
  (fiveam:is (= 9 (sref (%priv-gate-step 'priv-gate-machine #x0c 1) 'cr))))

(fiveam:test explicit-accessor-violation-follows-the-violation-policy
  (let ((m (make-machine 'priv-gate-trap-machine)))
    (load-program m (list #x0b))
    (fiveam:is (eq :trap (run m :max-steps 1)))))

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
      (fiveam:is (equal '(:kind :register :name cr :address nil :required supervisor :access :read :mask nil)
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

(%def-priv-irq-machine priv-irq-machine :on-violation (:interrupt 7 :priority 3))
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
    (fiveam:is (= 1 (machine-interrupt-pending-count m)))
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
    (fiveam:is (equal '(:pc 1 :kind :register :name cr :address nil :required supervisor :current user :access :read :mask nil)
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
    (fiveam:is (= 3 (third (first (%pending m)))))))

(fiveam:test violation-interrupt-falls-back-to-a-fault-when-the-signal-is-dropped
  (let ((m (%priv-irq-machine (list #x02) :vector 0)))
    (fiveam:signals privilege-violation (step-machine m))
    (fiveam:is (null (%pending m)))
    (fiveam:is (null (privilege-violation-info m)))))

(fiveam:test violation-interrupt-outside-a-step-faults
  (let ((m (%priv-irq-machine (list #x00))))
    (fiveam:signals privilege-violation (%gated-sref m 'cr 'supervisor 'supervisor))
    (fiveam:is (null (%pending m)))))

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
  (%priv-rejects (privilege :level s :levels (user supervisor) :on-violation (:interrupt 1 2 3)))
  (dolist (policy '((:interrupt 1 2) (:interrupt 1 :priority) (:interrupt 1 :priority :high)
                    (:interrupt 1 :bogus 1) (:interrupt 1 :non-maskable 5)))
    (fiveam:signals machine-definition-error
      (eval `(defmachine priv-bad-policy-machine
               (register pc :width 8) (register ia :width 8) (register a :width 8)
               (stack st :width 8 :depth 4) (memory ram :width 8 :addr-width 8) (flags s)
               (interrupts :vector ia :message a :save (pc))
               (privilege :level s :levels (user supervisor) :on-violation ,policy))))))

(fiveam:test violation-interrupt-accepts-priority-and-non-maskable-options
  (fiveam:finishes
    (eval '(defmachine priv-good-policy-machine
            (register pc :width 8) (register ia :width 8) (register a :width 8)
            (stack st :width 8 :depth 4) (memory ram :width 8 :addr-width 8) (flags s)
            (interrupts :vector ia :message a :save (pc))
            (privilege :level s :levels (user supervisor)
                       :on-violation (:interrupt 1 :non-maskable t :priority 2))))))

;;; #305: non-maskable violation interrupts

(defmacro %def-priv-masked-machine (name &rest policy)
  `(progn
     (defmachine ,name
       (register pc :width 8) (register ia :width 8) (register a :width 8)
       (register cr :width 8 :privilege supervisor)
       (stack st :width 8 :depth 8)
       (memory ram :width 8 :addr-width 8)
       (flags s im)
       (privilege :level s :levels (user supervisor) :on-violation (:interrupt 7 ,@policy))
       (interrupts :vector ia :message a :save (pc s) :deliver-level supervisor :mask-flag im))
     (definstruction ,name rd-cr (encoding (opcode #x02)) (semantics (set! a cr)))))

(%def-priv-masked-machine priv-masked-machine)
(%def-priv-masked-machine priv-nmi-machine :non-maskable t)

(fiveam:test violation-interrupt-repeats-on-a-masked-machine-until-non-maskable
  (let ((m (%priv-irq-machine (list #x02) :name 'priv-masked-machine)))
    (setf (flag m 'im) 1)
    (dotimes (i 3) (fiveam:is (eq :privilege-violation (step-machine m))))
    (fiveam:is (= 3 (machine-interrupt-pending-count m)))))

(fiveam:test non-maskable-violation-interrupt-delivers-on-a-masked-machine
  (let ((m (%priv-irq-machine (list #x02) :name 'priv-nmi-machine)))
    (setf (flag m 'im) 1)
    (fiveam:is (eq :privilege-violation (step-machine m)))
    (step-machine m)
    (fiveam:is (= 7 (sref m 'a)))
    (fiveam:is (eq 'supervisor (privilege-level m)))))

;;; #299: a level held in bits of a wider register

(defmachine priv-sr-machine
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (register sr :width 16)
  (stack st :width 16 :depth 8)
  (memory ram :width 8 :addr-width 8
    (region kernel #x00 #x0f :privilege supervisor))
  (privilege :level sr :shift 13 :width 1 :levels (user supervisor))
  (interrupts :vector ia :message a :save (pc sr) :deliver-level supervisor))

(definstruction priv-sr-machine nop (encoding (opcode #x00)) (semantics nil))
(definstruction priv-sr-machine rfi (encoding (opcode #x01)) (semantics (interrupt-return)))

(defmachine priv-cpl-machine
  (register pc :width 8)
  (register cs :width 16)
  (memory ram :width 8 :addr-width 8
    (region kernel #x00 #x0f :privilege ring0))
  (privilege :level cs :width 2 :levels ((ring3 3) (ring0 0))))

(fiveam:test level-field-follows-its-bit-and-ignores-the-rest
  (let ((m (make-machine 'priv-sr-machine)))
    (setf (sref m 'sr) #x00ff)
    (fiveam:is (eq 'user (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram 1))
    (setf (sref m 'sr) #x20ff)
    (fiveam:is (eq 'supervisor (privilege-level m)))
    (fiveam:is (= 0 (mref m 'ram 1)))
    (setf (sref m 'sr) #xdfff)
    (fiveam:is (eq 'user (privilege-level m)))))

(fiveam:test level-field-at-the-bottom-of-a-wider-register
  (let ((m (make-machine 'priv-cpl-machine)))
    (setf (sref m 'cs) #x0ab0)
    (fiveam:is (eq 'ring0 (privilege-level m)))
    (fiveam:is (= 0 (mref m 'ram 1)))
    (setf (sref m 'cs) #x0ab3)
    (fiveam:is (eq 'ring3 (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram 1))
    (setf (sref m 'cs) #x0ab1)
    (fiveam:is (null (privilege-level m)))
    (fiveam:signals privilege-violation (mref m 'ram 1))))

(fiveam:test delivery-sets-only-the-level-bits-and-return-restores-them
  (let ((m (make-machine 'priv-sr-machine)))
    (load-program m (append (make-list 17 :initial-element 0) (list 1)))
    (setf (sref m 'pc) 0 (sref m 'ia) 16 (sref m 'sr) #x00ff)
    (signal-interrupt m 1)
    (step-machine m)
    (fiveam:is (= #x20ff (sref m 'sr)))
    (step-machine m)
    (fiveam:is (= #x00ff (sref m 'sr)))
    (fiveam:is (eq 'user (privilege-level m)))))

(fiveam:test level-field-must-fit-its-register-and-values
  (flet ((rejects (&rest keys)
           (fiveam:signals machine-definition-error
             (eval `(defmachine priv-bad-machine
                      (register pc :width 8) (register sr :width 16) (flags s)
                      (memory ram :width 8 :addr-width 8)
                      (privilege ,@keys))))))
    (rejects :level 'sr :shift 15 :width 2 :levels '(user supervisor))
    (rejects :level 'sr :shift 16 :levels '(user supervisor))
    (rejects :level 'sr :width 1 :levels '(user (supervisor 2)))
    (rejects :level 's :shift 1 :levels '(user supervisor))
    (rejects :level 's :width 2 :levels '(user supervisor))
    (rejects :level 'sr :width 0 :levels '(user supervisor))
    (rejects :level 'sr :shift -1 :levels '(user supervisor))))

(fiveam:test level-field-width-defaults-to-the-rest-of-the-register
  (fiveam:finishes
    (eval '(defmachine priv-default-width-machine
            (register pc :width 8) (register sr :width 16)
            (memory ram :width 8 :addr-width 8)
            (privilege :level sr :shift 13 :levels (user supervisor (kernel 7)))))))

;;; #303: separate read, write and execute levels

(defmacro %def-priv-split-machine (name)
  `(progn
     (defmachine ,name
       (register pc :width 8) (register ia :width 8) (register a :width 8)
       (register cr :width 8 :privilege (:write supervisor))
       (register rr :width 8 :privilege (:read supervisor))
       (stack sw :width 8 :depth 4 :privilege (:write supervisor))
       (stack sr :width 8 :depth 4 :privilege (:read supervisor))
       (memory ram :width 8 :addr-width 8
         (region no-read #x00 #x0f :privilege (:read supervisor))
         (region no-exec #x10 #x1f :privilege (:execute supervisor))
         (region no-write #x20 #x2f :privilege (:write supervisor))
         (region mixed #x30 #x3f :privilege (:read user :write supervisor :execute supervisor)))
       (flags s (ie :privilege (:write supervisor)) (ro :privilege (:read supervisor)))
       (privilege :level s :levels (user supervisor))
       (interrupts :vector ia :message a :save (pc) :stack sw))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil))
     (definstruction ,name rd-cr (encoding (opcode #x01)) (semantics (set! a cr)))
     (definstruction ,name wr-cr (encoding (opcode #x02)) (semantics (set! cr 9)))
     (definstruction ,name inc-cr (encoding (opcode #x03)) (semantics (incf cr)))
     (definstruction ,name rd-rr (encoding (opcode #x04)) (semantics (set! a rr)))
     (definstruction ,name wr-rr (encoding (opcode #x05)) (semantics (set! rr 9)))
     (definstruction ,name wr-ie (encoding (opcode #x06)) (semantics (set-flags! (ie 1))))
     (definstruction ,name rd-ie (encoding (opcode #x07)) (semantics (set! a ie)))
     (definstruction ,name rd-ro (encoding (opcode #x08)) (semantics (set! a ro)))
     (definstruction ,name push-sw (encoding (opcode #x09)) (semantics (push 3 sw)))
     (definstruction ,name pop-sw (encoding (opcode #x0a)) (semantics (set! a (pop sw))))
     (definstruction ,name depth-sw (encoding (opcode #x0b)) (semantics (set! a (stack-depth sw))))
     (definstruction ,name push-sr (encoding (opcode #x0c)) (semantics (push 3 sr)))
     (definstruction ,name pop-sr (encoding (opcode #x0d)) (semantics (set! a (pop sr))))
     (definstruction ,name depth-sr (encoding (opcode #x0e)) (semantics (set! a (stack-depth sr))))
     (definstruction ,name x-wr-cr (encoding (opcode #x0f)) (semantics (setf (sref machine 'cr) 9)))
     (definstruction ,name x-rd-cr (encoding (opcode #x10)) (semantics (set! a (sref machine 'cr))))))

(%def-priv-split-machine priv-split-machine)

(defun %priv-split (level &key (pc 0) program)
  (let ((m (make-machine 'priv-split-machine)))
    (setf (sref m 's) level (sref m 'pc) pc)
    (when program (load-program m program :origin pc))
    m))

(defun %priv-split-step (opcode level)
  (let ((m (%priv-split level :program (list opcode))))
    (values m (handler-case (progn (step-machine m) nil)
                (privilege-violation (c) c)))))

(defun %priv-split-access (opcode level)
  (let ((c (nth-value 1 (%priv-split-step opcode level))))
    (and c (privilege-violation-access c))))

(fiveam:test read-gated-region-still-fetches-at-user-level
  (let ((m (%priv-split 0 :program (list #x00))))
    (fiveam:finishes (step-machine m))
    (fiveam:is (= 1 (sref m 'pc)))
    (handler-case (progn (mref m 'ram 0) (fiveam:fail "read did not fault"))
      (privilege-violation (c)
        (fiveam:is (eq :read (privilege-violation-access c)))
        (fiveam:is (= 0 (privilege-violation-address c)))))
    (fiveam:signals privilege-violation (mref m 'ram 1))
    (fiveam:finishes (setf (mref m 'ram 1) 5))))

(fiveam:test execute-gated-region-reads-and-writes-but-does-not-fetch
  (let ((m (%priv-split 0 :pc #x10 :program (list #x00))))
    (fiveam:finishes (setf (mref m 'ram #x11) 4))
    (fiveam:is (= 4 (mref m 'ram #x11)))
    (handler-case (progn (step-machine m) (fiveam:fail "fetch did not fault"))
      (privilege-violation (c)
        (fiveam:is (eq :execute (privilege-violation-access c)))
        (fiveam:is (eq :memory (privilege-violation-kind c)))
        (fiveam:is (= #x10 (privilege-violation-address c)))
        (fiveam:is (search "Fetch from" (princ-to-string c)))))
    (fiveam:is (= #x10 (sref m 'pc)))
    (setf (sref m 's) 1)
    (fiveam:finishes (step-machine m))))

(fiveam:test write-gated-region-reads-and-fetches
  (let ((m (%priv-split 0 :pc #x20 :program (list #x00))))
    (fiveam:is (= 0 (mref m 'ram #x21)))
    (handler-case (progn (setf (mref m 'ram #x21) 1) (fiveam:fail "write did not fault"))
      (privilege-violation (c) (fiveam:is (eq :write (privilege-violation-access c)))))
    (fiveam:finishes (step-machine m))))

(fiveam:test mixed-region-levels-gate-each-access
  (let ((m (%priv-split 0 :pc #x30 :program (list #x00))))
    (fiveam:is (= 0 (mref m 'ram #x31)))
    (fiveam:signals privilege-violation (setf (mref m 'ram #x31) 1))
    (fiveam:signals privilege-violation (step-machine m))
    (setf (sref m 's) 1)
    (fiveam:finishes (setf (mref m 'ram #x31) 1))
    (fiveam:finishes (step-machine m))))

(fiveam:test interrupt-handler-fetches-from-a-read-gated-region-at-user-level
  (let ((m (%priv-split 0 :program (list #x00 #x00))))
    (setf (sref m 'ia) 2)
    (signal-interrupt m 1)
    (fiveam:finishes (step-machine m))
    (fiveam:is (eq 'user (privilege-level m)))))

(fiveam:test write-only-element-gates-read-nothing
  (fiveam:is (null (%priv-split-access #x01 0)))
  (fiveam:is (eq :write (%priv-split-access #x02 0)))
  (fiveam:is (eq :write (%priv-split-access #x03 0)))
  (fiveam:is (eq :write (%priv-split-access #x0f 0)))
  (fiveam:is (null (%priv-split-access #x10 0))))

(fiveam:test read-only-gated-element-gates-reads-only
  (fiveam:is (eq :read (%priv-split-access #x04 0)))
  (fiveam:is (null (%priv-split-access #x05 0))))

(fiveam:test split-flag-levels
  (fiveam:is (eq :write (%priv-split-access #x06 0)))
  (fiveam:is (null (%priv-split-access #x07 0)))
  (fiveam:is (eq :read (%priv-split-access #x08 0))))

(fiveam:test split-stack-levels
  (fiveam:is (eq :write (%priv-split-access #x09 0)))
  (fiveam:is (eq :write (%priv-split-access #x0a 0)))
  (fiveam:is (null (%priv-split-access #x0b 0)))
  (fiveam:is (null (%priv-split-access #x0c 0)))
  (fiveam:is (eq :read (%priv-split-access #x0d 0)))
  (fiveam:is (eq :read (%priv-split-access #x0e 0))))

(fiveam:test split-gates-pass-at-the-required-level
  (dolist (opcode '(#x02 #x03 #x04 #x06 #x08 #x09 #x0c #x0e))
    (fiveam:is (null (nth-value 1 (%priv-split-step opcode 1))))))

(fiveam:test split-violation-reports-the-access
  (let ((m (%priv-split 0 :program (list #x02))))
    (handler-case (step-machine m)
      (privilege-violation (c)
        (fiveam:is (eq :register (privilege-violation-kind c)))
        (fiveam:is (eq :write (privilege-violation-access c)))))))

(fiveam:test privilege-plist-rejects-bad-specs
  (flet ((rejects (element)
           (fiveam:signals machine-definition-error
             (eval `(defmachine priv-bad-machine
                      (register pc :width 8) ,element (flags s)
                      (memory ram :width 8 :addr-width 8)
                      (privilege :level s :levels (user supervisor)))))))
    (rejects '(register r :width 8 :privilege (:execute supervisor)))
    (rejects '(register r :width 8 :privilege (:frob supervisor)))
    (rejects '(register r :width 8 :privilege (:read supervisor :read user)))
    (rejects '(register r :width 8 :privilege (:read)))
    (rejects '(register r :width 8 :privilege (:read nosuch)))
    (rejects '(register r :width 8 :privilege (:read "x")))
    (rejects '(stack k :width 8 :depth 2 :privilege (:execute supervisor)))
    (rejects '(memory m2 :width 8 :addr-width 8 (region k 0 3 :privilege (:frob supervisor))))))

(fiveam:test region-plist-of-only-the-listed-access
  (fiveam:finishes
    (eval '(defmachine priv-plist-ok-machine
            (register pc :width 8)
            (memory ram :width 8 :addr-width 8
              (region k 0 3 :privilege (:execute supervisor)))
            (flags s)
            (privilege :level s :levels (user supervisor))))))

(fiveam:test inheriting-machine-keeps-split-levels
  (fiveam:finishes
    (eval '(defmachine (priv-split-child (:extends priv-split-machine))
            (properties :child t))))
  (let ((m (make-machine 'priv-split-child)))
    (setf (sref m 's) 0)
    (fiveam:signals privilege-violation (mref m 'ram 0))))

;;; #314: per-field write gates on a register

(defmacro %def-priv-fields-machine (name &rest privilege-keys)
  `(progn
     (defmachine ,name
       (register pc :width 8) (register ia :width 8) (register a :width 8)
       (register sr :width 16
         :privilege (:fields ((#x2000 supervisor) (#x0700 supervisor :on-write :ignore))))
       (register r :width 8 :names (r0 r1) :privilege (:fields ((#x80 supervisor))))
       (stack st :width 16 :depth 4)
       (memory ram :width 8 :addr-width 8)
       (privilege :level sr :shift 13 :width 1 :levels (user supervisor) ,@privilege-keys)
       (interrupts :vector ia :message a :save (pc sr) :deliver-level supervisor))
     (definstruction ,name nop (encoding (opcode #x00)) (semantics nil))
     (definstruction ,name rfi (encoding (opcode #x01)) (semantics (interrupt-return)))
     (definstruction ,name set-cc (encoding (opcode #x02)) (semantics (set! sr (logior sr #x0001))))
     (definstruction ,name flip-s (encoding (opcode #x03)) (semantics (set! sr (logxor sr #x2000))))
     (definstruction ,name same (encoding (opcode #x04)) (semantics (set! sr sr)))
     (definstruction ,name flip-ipl (encoding (opcode #x05)) (semantics (set! sr (logxor sr #x0700))))
     (definstruction ,name flip-s-sref (encoding (opcode #x06))
       (semantics (setf (sref machine 'sr) (logxor (sref machine 'sr) #x2000))))
     (definstruction ,name flip-s-incf (encoding (opcode #x07)) (semantics (incf sr #x2000)))
     (definstruction ,name set-r1 (encoding (opcode #x08)) (semantics (set! r1 #x80)))
     (definstruction ,name set-r1-regref (encoding (opcode #x09))
       (semantics (setf (regref machine 'r 1) #x80)))))

(%def-priv-fields-machine priv-fields-machine)
(%def-priv-fields-machine priv-fields-irq-machine :on-violation (:interrupt 7))
(%def-priv-fields-machine priv-fields-trap-machine :on-violation :trap)

(defun %priv-fields-step (opcode sr &key (name 'priv-fields-machine))
  (let ((m (make-machine name)))
    (setf (sref m 'sr) sr)
    (load-program m (list opcode))
    (values m (handler-case (progn (step-machine m) nil)
                (privilege-violation (c) c)))))

(fiveam:test field-outside-the-mask-is-writable-at-user-level
  (let ((m (%priv-fields-step #x02 #x0000)))
    (fiveam:is (= #x0001 (sref m 'sr)))))

(fiveam:test field-change-below-its-level-violates-and-leaves-the-register
  (dolist (opcode '(#x03 #x06 #x07))
    (multiple-value-bind (m c) (%priv-fields-step opcode #x0005)
      (fiveam:is (typep c 'privilege-violation))
      (fiveam:is (eq :register (privilege-violation-kind c)))
      (fiveam:is (eq :write (privilege-violation-access c)))
      (fiveam:is (eq 'sr (storage-error-name c)))
      (fiveam:is (eq 'supervisor (privilege-violation-required c)))
      (fiveam:is (= #x0005 (sref m 'sr))))))

(fiveam:test field-written-back-unchanged-is-allowed
  (fiveam:is (= #x0705 (sref (%priv-fields-step #x04 #x0705) 'sr))))

(fiveam:test field-at-its-level-is-writable
  (fiveam:is (= #x0005 (sref (%priv-fields-step #x03 #x2005) 'sr)))
  (fiveam:is (= #x2005 (sref (%priv-fields-step #x05 #x2705) 'sr))))

(fiveam:test field-ignore-policy-keeps-the-old-bits
  (multiple-value-bind (m c) (%priv-fields-step #x05 #x0205)
    (fiveam:is (null c))
    (fiveam:is (= #x0205 (sref m 'sr)))))

(fiveam:test field-on-a-banked-register-gates-alias-and-regref
  (dolist (opcode '(#x08 #x09))
    (multiple-value-bind (m c) (%priv-fields-step opcode 0)
      (fiveam:is (eq 'r (storage-error-name c)))
      (fiveam:is (= 0 (regref m 'r 1))))
    (let ((m (%priv-fields-step opcode #x2000)))
      (fiveam:is (= #x80 (regref m 'r 1))))))

(fiveam:test field-gates-do-not-bind-the-host
  (let ((m (make-machine 'priv-fields-machine)))
    (setf (sref m 'sr) #x2700)
    (setf (sref m 'sr) #x0000)
    (fiveam:is (= 0 (sref m 'sr)))
    (setf (regref m 'r 1) #x80)
    (fiveam:is (= #x80 (regref m 'r 1)))))

(fiveam:test field-violation-interrupt-leaves-the-register-and-return-restores-the-level
  (let ((m (make-machine 'priv-fields-irq-machine)))
    (load-program m (list #x03))
    (load-program m (list #x00 #x01) :origin 32)
    (setf (sref m 'pc) 0 (sref m 'ia) 32 (sref m 'sr) #x0005)
    (fiveam:is (eq :privilege-violation (step-machine m)))
    (fiveam:is (= #x0005 (sref m 'sr)))
    (step-machine m)
    (fiveam:is (eq 'supervisor (privilege-level m)))
    (step-machine m)
    (step-machine m)
    (fiveam:is (= #x0005 (sref m 'sr)))))

(fiveam:test field-gates-respect-disabled-privilege-checks
  (let ((*privilege-checks* nil))
    (fiveam:is (= #x2005 (sref (%priv-fields-step #x03 #x0005) 'sr)))
    (fiveam:is (= #x0705 (sref (%priv-fields-step #x05 #x0005) 'sr)))))

(fiveam:test field-definition-errors
  (flet ((rejects (register &rest extra)
           (fiveam:signals machine-definition-error
             (eval `(defmachine priv-bad-fields-machine
                      (register pc :width 8) (register sp :width 8)
                      ,register ,@extra
                      (memory ram :width 8 :addr-width 8)
                      (flags s)
                      (privilege :level s :levels (user supervisor)))))))
    (rejects '(register x :width 8 :privilege (:fields ((#x100 supervisor)))))
    (rejects '(register x :width 8 :privilege (:fields ((0 supervisor)))))
    (rejects '(register x :width 8 :privilege (:fields ((#x03 supervisor) (#x06 supervisor)))))
    (rejects '(register x :width 8 :privilege (:fields ((#x01 nobody)))))
    (rejects '(register x :width 8 :privilege (:fields ((#x01 supervisor :on-write :frob)))))
    (rejects '(register x :width 8 :privilege (:fields ())))
    (rejects '(register x :width 8 :privilege (:fields ((#x01 supervisor :bad t)))))
    (rejects '(flags (f :privilege (:fields ((1 supervisor))))))
    (rejects '(stack k :width 8 :depth 2 :privilege (:fields ((1 supervisor)))))
    (rejects '(register x :width 8 :privilege (:fields ((#x01 supervisor))))
             '(stack-pointer x :memory ram))
    (fiveam:signals machine-definition-error
      (eval '(defmachine priv-bad-fields-machine
              (register pc :width 8)
              (register x :width 8 :privilege (:fields ((#x01 supervisor))))
              (memory ram :width 8 :addr-width 8))))))

(fiveam:test inheriting-machine-must-keep-fields
  (fiveam:finishes
    (eval '(defmachine (priv-fields-child (:extends priv-fields-machine))
            (properties :child t))))
  (fiveam:signals machine-definition-error
    (eval '(defmachine (priv-fields-bad-child (:extends priv-fields-machine))
            (register sr :width 16 :privilege (:fields ((#x2000 supervisor))))))))

;;; #315: a field violation reports the gated mask

(fiveam:test field-violation-reports-the-gated-mask
  (let ((c (nth-value 1 (%priv-fields-step #x03 #x0005))))
    (fiveam:is (= #x2000 (privilege-violation-mask c))))
  (fiveam:is (null (privilege-violation-mask (nth-value 1 (%priv-gate-step 'priv-gate-machine #x02 0))))))

(fiveam:test field-violation-interrupt-info-carries-the-mask
  (let ((m (make-machine 'priv-fields-irq-machine)))
    (load-program m (list #x03))
    (setf (sref m 'pc) 0 (sref m 'ia) 32 (sref m 'sr) #x0005)
    (step-machine m)
    (fiveam:is (= #x2000 (getf (privilege-violation-info m) :mask)))))

(fiveam:test field-violation-trap-data-carries-the-mask
  (let ((m (make-machine 'priv-fields-trap-machine)))
    (load-program m (list #x03))
    (setf (sref m 'sr) #x0005)
    (let ((c (handler-case (progn (step-machine m) nil) (lasm-trap (c) c))))
      (fiveam:is (= #x2000 (getf (lasm-trap-data c) :mask))))))

;;; #169: stack-ref on a gated (stack-pointer ...) register honors its gate.

(defmachine priv-sp-ref-machine
  (register sp :width 8 :privilege supervisor)
  (flags s)
  (memory ram :width 8 :addr-width 8)
  (stack-pointer sp :memory ram)
  (privilege :level s :levels (user supervisor)))

(fiveam:test stack-ref-on-a-gated-pointer-register-needs-its-level
  (fiveam:signals privilege-violation
    (eval '(with-machine (m priv-sp-ref-machine) (stack-ref 0))))
  (fiveam:signals privilege-violation
    (eval '(with-machine (m priv-sp-ref-machine) (setf (stack-ref 0) 1)))))
