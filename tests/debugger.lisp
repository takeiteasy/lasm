;;;; tests/debugger.lisp
;;;; fiveam tests for the debugger (debugger.lisp, #76).

(in-package #:lasm)

(fiveam:def-suite debugger :in lasm)
(fiveam:in-suite debugger)

;; Reuses EMU-TEST-MACHINE (tests/emulator.lisp) for the core address/label/
;; step/continue cases -- it already has an HLT (via TRAP) for a clean stop.
;; A second, small fixture below adds a banked register (#13) to exercise
;; DEBUG-STATE-TEXT's non-scalar path, which SREF alone cannot walk.

(defmachine dbg-bank-test-machine
  (register v :width 8 :count 4)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(definstruction dbg-bank-test-machine hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;;; Fixture program: count:  ldx #3 / .loop: dex / bne .loop / sta $10 / hlt

(defun %dbg-assembly ()
  (assemble "count:  ldx #3
.loop:  dex
        bne .loop
        sta $10
        hlt"
            :machine 'emu-test-machine :origin #x100))

(defun %dbg-session ()
  (let* ((a (%dbg-assembly))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (make-debug-session m :assembly a)))

;;; Breakpoints

(fiveam:test debug-break-by-address-stops-there
  (let ((session (%dbg-session)))
    (debug-break session #x103) ; the bne instruction
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 2 steps)) ; ldx, dex
      (fiveam:is (= #x103 (%pc session))))))

(fiveam:test debug-break-by-label-resolves-through-assembly-symbol
  (let ((session (%dbg-session)))
    (let ((bp (debug-break session ".loop" :scope "count")))
      (fiveam:is (= #x102 (breakpoint-address bp))))))

(fiveam:test debug-break-distinguishes-global-from-scoped-local
  (let* ((a (assemble "loop: hlt
.next: hlt
loop.next: hlt" :machine 'emu-test-machine))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (let ((session (make-debug-session m :assembly a)))
      (fiveam:is (= 1 (breakpoint-address
                        (debug-break session ".next" :scope "loop"))))
      (fiveam:is (= 2 (breakpoint-address
                        (debug-break session "loop.next")))))))

(fiveam:test debug-break-by-label-rejects-equ
  (let* ((a (assemble ".equ limit, 10
        ldx #1
        hlt" :machine 'emu-test-machine :origin #x100))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (let ((session (make-debug-session m :assembly a)))
      (fiveam:signals error (debug-break session "limit")))))

(fiveam:test debug-break-without-assembly-signals-on-label
  (let ((session (make-debug-session (make-machine 'emu-test-machine))))
    (fiveam:signals error (debug-break session "loop"))))

(fiveam:test debug-continue-from-a-breakpoint-proceeds-past-it
  ;; .loop (0x102) is visited three times over the fixture's loop -- a
  ;; breakpoint there lets us verify continuing from a PC that IS a
  ;; breakpoint doesn't re-trigger with zero steps, but genuinely executes
  ;; at least one instruction before stopping again (STOP-P fires *after*
  ;; the step, per %RUN-LOOP's own contract).
  (let ((session (%dbg-session)))
    (debug-break session #x102)
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 1 steps)) ; ldx only
      (fiveam:is (= #x102 (%pc session))))
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 2 steps)) ; dex, bne (branch taken back to .loop)
      (fiveam:is (= #x102 (%pc session))))))

(fiveam:test debug-unbreak-removes-by-address
  (let ((session (%dbg-session)))
    (debug-break session #x100)
    (fiveam:is (= 1 (length (debug-breakpoints session))))
    (fiveam:is (debug-unbreak session #x100))
    (fiveam:is (null (debug-breakpoints session)))))

(fiveam:test debug-unbreak-id-takes-priority-over-a-colliding-address
  ;; Breakpoint id 1's own numeric value can coincide with a *different*
  ;; breakpoint's address (ids and addresses share no reserved range) --
  ;; DELETE 1 must remove breakpoint id 1, not whatever breakpoint (if any)
  ;; happens to sit at address 1.
  (let ((session (%dbg-session)))
    (debug-break session #x102) ; becomes id 1
    (debug-break session #x1)   ; becomes id 2, address 1 -- collides with id 1
    (fiveam:is (= 2 (length (debug-breakpoints session))))
    (fiveam:is (debug-unbreak session 1))
    (let ((remaining (debug-breakpoints session)))
      (fiveam:is (= 1 (length remaining)))
      (fiveam:is (= 1 (breakpoint-address (first remaining)))) ; id 1 (at 0x102) was removed, not address 1
      (fiveam:is (= 2 (breakpoint-id (first remaining)))))))

;;; Step / continue reasons

(fiveam:test debug-step-n-reports-step-not-max-steps
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-step session 2)
      (fiveam:is (eq :step reason))
      (fiveam:is (= 2 steps)))))

(fiveam:test debug-step-stops-early-on-trap
  (let ((session (%dbg-session)))
    (debug-continue session) ; run to the hlt trap
    (multiple-value-bind (reason steps) (debug-step session 5)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 1 steps)))))

(fiveam:test debug-continue-to-address-stops-exactly-there
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-continue-to session #x106) ; sta $10
      (declare (ignore steps))
      (fiveam:is (eq :until reason))
      (fiveam:is (= #x106 (%pc session))))))

(fiveam:test debug-continue-max-steps-guard
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason) (debug-continue session :max-steps 1)
      (fiveam:is (eq :max-steps reason)))))

(fiveam:test debug-continue-reports-trap-and-decode-failure
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason) (debug-continue session)
      (fiveam:is (eq :trap reason))))
  (let* ((m (make-machine 'emu-test-machine))
         (session (make-debug-session m)))
    ;; No program loaded -- ram is all zero, #x00 is HLT's own opcode, so
    ;; feed a byte that decodes to nothing instead.
    (setf (mref m 'ram 0) #xFF)
    (multiple-value-bind (reason) (debug-continue session)
      (fiveam:is (eq :decode-failure reason)))))

(fiveam:test debugger-continue-reports-storage-fault
  (dolist (continue-function (list (lambda (session) (debug-continue session))
                                  (lambda (session) (debug-continue-to session 2))))
    (let* ((m (make-machine 'stack-test-machine))
           (session (make-debug-session m)))
      (load-program m (list #x04))
      (multiple-value-bind (reason steps condition) (funcall continue-function session)
        (fiveam:is (eq :fault reason))
        (fiveam:is (= 1 steps))
        (fiveam:is (typep condition 'stack-underflow)))))
  (let* ((m (make-machine 'stack-test-machine))
         (session (make-debug-session m)))
    (load-program m (list #x04))
    (fiveam:signals stack-underflow (debug-step session))))

(fiveam:test debugger-commands-show-storage-fault
  (dolist (command '("continue" "until 2"))
    (let* ((m (make-machine 'stack-test-machine))
           (session (make-debug-session m)))
      (load-program m (list #x04))
      (let ((response (debug-command session command)))
        (fiveam:is (search "Stopped: fault  steps=1" response))
        (fiveam:is (search "Stack underflow" response))))))

;; #110: EMU-TEST-MACHINE declares no (interrupts ...) clause and no
;; devices -- a SLP with nothing to wake it must report :IDLE, not spin to
;; :MAX-STEPS, exactly like RUN itself.
(fiveam:test debug-continue-reports-idle-with-nothing-left-to-wake-it
  (let* ((m (make-machine 'emu-test-machine))
         (session (make-debug-session m)))
    (load-program m (list #x04) :origin 0) ; slp
    (multiple-value-bind (reason) (debug-continue session)
      (fiveam:is (eq :idle reason)))))

;;; Inspection

(fiveam:test debug-state-text-covers-scalar-and-banked-registers
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (setf (regref m 'v 0) 1 (regref m 'v 3) 4)
    (let ((text (debug-state-text session)))
      (fiveam:is (search "v " text))
      (fiveam:is (search "1 0 0 4" text)))))

(defmachine dbg-alias-test-machine
  (register reg :width 8 :names (a b c d))
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defun %dbg-alias-session ()
  (let ((m (make-machine 'dbg-alias-test-machine)))
    (setf (regref m 'reg 0) 1 (regref m 'reg 1) 7 (regref m 'reg 3) 4)
    (make-debug-session m)))

(fiveam:test debug-state-text-labels-aliased-bank-cells
  (fiveam:is (search "[a=1 b=7 c=0 d=4]" (debug-state-text (%dbg-alias-session)))))

(fiveam:test debug-state-text-leaves-unaliased-bank-unlabelled
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (text (debug-state-text (make-debug-session m))))
    (fiveam:is (search "[0 0 0 0]" text))
    (fiveam:is (not (find #\= text :start (position #\[ text) :end (position #\] text))))))

(fiveam:test debug-command-print-register-alias
  (let ((session (%dbg-alias-session)))
    (fiveam:is (search "b = 7" (debug-command session "print b")))
    (fiveam:is (search "B = 7" (debug-command session "print B")))))

(fiveam:test debug-command-print-aliased-bank-whole
  (fiveam:is (search "reg = [a=1 b=7 c=0 d=4]"
                     (debug-command (%dbg-alias-session) "print reg"))))

(fiveam:test debug-command-print-unknown-name-still-reports
  (fiveam:is (search "unknown name \"zz\""
                     (debug-command (%dbg-alias-session) "print zz"))))

(fiveam:test debug-memory-text-hex-width-follows-cell-width
  ;; A 16-bit-cell machine's dump uses four hex digits.
  (let* ((m (make-machine 'test-machine)) ; tests/suites.lisp: wram is 16-bit-cell
         ;; TEST-MACHINE declares no PC register -- pass one explicitly
         ;; (:PC 'A) since this test never steps/continues, only inspects.
         (session (make-debug-session m :pc 'a :memory 'wram)))
    (setf (mref m 'wram 0) #x3E8)
    (let ((text (debug-memory-text session 0 1)))
      (fiveam:is (search "03E8" text)))))

;;; Command dispatch

(fiveam:test debug-command-break-and-continue-round-trip
  (let ((session (%dbg-session)))
    (let ((response (debug-command session "break 0x103")))
      (fiveam:is (search "Breakpoint" response)))
    (let ((response (debug-command session "continue")))
      (fiveam:is (search "breakpoint" response)))))

(fiveam:test debug-command-unknown-command-returns-text-not-signal
  (let ((session (%dbg-session)))
    (fiveam:is (search "Unknown command" (debug-command session "frobnicate")))))

(fiveam:test debug-command-quit-signals-second-value
  (let ((session (%dbg-session)))
    (multiple-value-bind (text quit-p) (debug-command session "quit")
      (declare (ignore text))
      (fiveam:is (eq t quit-p)))))

(fiveam:test debug-command-info-sym-without-assembly-degrades-gracefully
  (let ((session (make-debug-session (make-machine 'emu-test-machine))))
    (fiveam:is (search "No assembly attached" (debug-command session "info sym")))))

(fiveam:test debug-command-print-register
  (let ((session (%dbg-session)))
    (debug-step session 1) ; ldx #3
    (fiveam:is (search "3" (debug-command session "print x")))))

;;; Watchpoints
;;;
;;; The fixture runs: ldx #3 (1), dex (2), bne (3, taken), dex, bne, dex, bne
;;; (7, not taken), sta $10 (8), hlt.

(fiveam:test debug-watch-register-write-stops-after-the-writing-instruction
  (let ((session (%dbg-session)))
    (debug-watch session "x")
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 0 (watch-hit-old hit)))
      (fiveam:is (= 3 (watch-hit-new hit))))
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 3 (watch-hit-old hit)))
      (fiveam:is (= 2 (watch-hit-new hit))))))

(fiveam:test debug-watch-write-ignores-reads
  (let ((session (%dbg-session)))
    (debug-watch session "z" :access :read)
    ;; ldx only writes z; bne reads it
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 3 steps)))))

(fiveam:test debug-watch-read-write-fires-on-both
  (let ((session (%dbg-session)))
    (debug-watch session "z" :access :read-write)
    (fiveam:is (= 1 (nth-value 1 (debug-continue session))))))

(fiveam:test debug-watch-memory-by-address-and-by-label
  (let ((session (%dbg-session)))
    (debug-watch session #x10)
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 8 steps))
      (fiveam:is (= 0 (watch-hit-new hit)))))
  (let* ((a (assemble "start: ldx #1
data:  hlt" :machine 'emu-test-machine :origin #x100))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (let ((wp (debug-watch (make-debug-session m :assembly a) "data" :access :read)))
      (fiveam:is (= #x102 (watchpoint-address wp))))))

(fiveam:test debug-watch-ignores-instruction-fetch-and-pc-bookkeeping
  (let ((session (%dbg-session)))
    (debug-watch session #x100 :access :read-write)
    (fiveam:is (eq :trap (debug-continue session))))
  (let ((session (%dbg-session)))
    (debug-watch session "pc")
    ;; only the taken branch writes pc from semantics
    (fiveam:is (= 3 (nth-value 1 (debug-continue session))))))

(fiveam:test debug-step-stops-early-on-a-watchpoint
  (let ((session (%dbg-session)))
    (debug-watch session "x")
    (multiple-value-bind (reason steps) (debug-step session 5)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps)))))

(fiveam:test debug-continue-to-stops-on-a-watchpoint-first
  (let ((session (%dbg-session)))
    (debug-watch session "x")
    (fiveam:is (eq :watchpoint (debug-continue-to session #x106)))))

(fiveam:test debug-inspection-never-trips-watchpoints
  (let ((session (%dbg-session)))
    (debug-watch session "x" :access :read-write)
    (debug-command session "print x")
    (debug-command session "info reg")
    (debug-state-text session)
    (fiveam:is (null (debug-session-watch-hit session)))
    (fiveam:is (null (machine-access-hook (debug-session-machine session))))))

(fiveam:test debug-watch-removes-the-hook-after-a-run
  (let ((session (%dbg-session)))
    (debug-watch session "x")
    (debug-continue session)
    (fiveam:is (null (machine-access-hook (debug-session-machine session))))))

(fiveam:test debug-watch-banked-register-cell
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m))
         (wp (debug-watch session "v" :index 2)))
    (fiveam:is (= 2 (watchpoint-index wp)))
    (fiveam:is (string= "v[2]" (watchpoint-label wp)))
    (funcall (%watch-hook session) m 'v 1 :write 5)
    (fiveam:is (null (debug-session-watch-hit session)))
    (funcall (%watch-hook session) m 'v 2 :write 5)
    (fiveam:is (= 5 (watch-hit-new (debug-session-watch-hit session))))
    (fiveam:signals error (debug-watch session "v"))
    (fiveam:signals error (debug-watch session "v" :index 9))))

(fiveam:test debug-watch-register-alias
  (let* ((session (%dbg-alias-session))
         (m (debug-session-machine session))
         (wp (debug-watch session "b")))
    (fiveam:is (= 1 (watchpoint-index wp)))
    (fiveam:is (string= "b" (watchpoint-label wp)))
    (funcall (%watch-hook session) m 'reg 1 :write 9)
    (let ((hit (debug-session-watch-hit session)))
      (fiveam:is (= 7 (watch-hit-old hit)))
      (fiveam:is (= 9 (watch-hit-new hit))))))

(fiveam:test debug-watch-rejects-bad-targets
  (let ((session (%dbg-session)))
    (fiveam:signals error (debug-watch session "nonesuch"))
    (fiveam:signals error (debug-watch session "ram"))
    (fiveam:signals error (debug-watch session "x" :access :sideways))))

(fiveam:test debug-unwatch-removes-by-id-and-shares-ids-with-breakpoints
  (let* ((session (%dbg-session))
         (bp (debug-break session #x103))
         (wp (debug-watch session "x")))
    (fiveam:is (/= (breakpoint-id bp) (watchpoint-id wp)))
    (fiveam:is (debug-unwatch session (watchpoint-id wp)))
    (fiveam:is (null (debug-unwatch session (watchpoint-id wp))))
    (fiveam:is (null (debug-watchpoints session)))))

(fiveam:test debug-command-watch-and-delete
  (let ((session (%dbg-session)))
    (fiveam:is (search "Watchpoint 1 (w) at x" (debug-command session "watch x")))
    (fiveam:is (search "(rw) at 0010" (debug-command session "watch 0x10 rw")))
    (fiveam:is (search "1: watch (w) x" (debug-command session "info break")))
    (fiveam:is (search "Watchpoint 1 (w) x: 0 -> 3" (debug-command session "continue")))
    (fiveam:is (search "Deleted watchpoint 1" (debug-command session "delete 1")))
    (fiveam:is (search "no such" (debug-command session "delete 1")))
    (fiveam:is (search "Error" (debug-command session "watch nonesuch")))))

(fiveam:test debug-command-watch-register-cell-syntax
  (let ((session (make-debug-session (make-machine 'dbg-bank-test-machine))))
    (fiveam:is (search "at v[2]" (debug-command session "watch v[2] r")))))

;;; Conditional breakpoints

(fiveam:test debug-break-condition-on-a-register
  (let ((session (%dbg-session)))
    (debug-break session #x102 :condition "x == 1")
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 5 steps))
      (fiveam:is (= 1 (sref (debug-session-machine session) 'x))))))

(fiveam:test debug-break-condition-never-true-runs-to-the-trap
  (let ((session (%dbg-session)))
    (debug-break session #x102 :condition "x == 9")
    (fiveam:is (eq :trap (debug-continue session)))))

(fiveam:test debug-break-condition-on-flag-label-equ-and-pc
  (let* ((a (assemble ".equ limit, 2
count: ldx #3
.loop: dex
       bne .loop
       hlt" :machine 'emu-test-machine :origin #x100))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (let ((session (make-debug-session m :assembly a)))
      (debug-break session ".loop" :scope "count"
                           :condition "x == limit && z == 0 && * == 0x102")
      (multiple-value-bind (reason steps) (debug-continue session)
        (fiveam:is (eq :breakpoint reason))
        (fiveam:is (= 3 steps))))
    (let ((session (make-debug-session (make-machine 'emu-test-machine) :assembly a)))
      (fiveam:is (breakpoint-p (debug-break session ".loop" :scope "count"
                                                      :condition ".loop == 0x102"))))))

(fiveam:test debug-break-condition-on-register-alias
  (let ((session (%dbg-alias-session)))
    (fiveam:is (%breakpoint-triggered-p session (debug-break session 0 :condition "b == 7")))
    (fiveam:is (not (%breakpoint-triggered-p session (debug-break session 0 :condition "b == 8"))))))

(fiveam:test debug-break-rejects-bad-conditions-at-set-time
  (let ((session (%dbg-session)))
    (fiveam:signals error (debug-break session #x102 :condition "x =="))
    (fiveam:signals error (debug-break session #x102 :condition "x 1"))
    (fiveam:signals error (debug-break session #x102 :condition "x == 1 || bogus"))
    (fiveam:signals error (debug-break session #x102 :condition "defined(x)"))
    (fiveam:signals error (debug-break session #x102 :condition "bank(x)"))
    (fiveam:signals error (debug-break session #x102 :condition "ram"))
    (fiveam:is (null (debug-breakpoints session)))))

(fiveam:test debug-break-condition-error-at-run-time-stops
  (let ((session (%dbg-session)))
    (debug-break session #x102 :condition "1 / (x - x) == 0")
    (multiple-value-bind (reason steps condition) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (typep condition 'error)))
    (fiveam:is (search "condition error" (debug-command session "continue")))))

(fiveam:test debug-command-break-if
  (let ((session (%dbg-session)))
    (fiveam:is (search "Breakpoint 1 at 0102"
                       (debug-command session "break 0x102 if x == 1")))
    (fiveam:is (search "if x == 1" (debug-command session "info break")))
    (fiveam:is (search "steps=5" (debug-command session "continue")))
    (fiveam:is (search "Error" (debug-command session "break 0x103 if")))
    (fiveam:is (search "Error" (debug-command session "break 0x103 if nonesuch == 1")))))

;;; Local labels by qualified name and scope form

(fiveam:test debug-resolves-qualified-local-names
  (let ((session (%dbg-session)))
    (fiveam:is (= #x102 (breakpoint-address (debug-break session "count.loop"))))
    (fiveam:is (= #x102 (watchpoint-address (debug-watch session "count.loop"))))
    (multiple-value-bind (reason) (debug-continue-to session "count.loop")
      (fiveam:is (eq :until reason)))
    (fiveam:is (breakpoint-p (debug-break session #x103 :condition "count.loop == 0x102")))
    (fiveam:signals error (debug-break session "count.nonesuch"))))

(fiveam:test debug-commands-accept-qualified-and-scoped-locals
  (let ((session (%dbg-session)))
    (fiveam:is (search "Breakpoint 1 at 0102" (debug-command session "break count.loop")))
    (fiveam:is (search "Breakpoint 2 at 0102" (debug-command session "break .loop in count")))
    (fiveam:is (search "Breakpoint 3 at 0102"
                       (debug-command session "break .loop in count if x == 1")))
    (fiveam:is (search "Watchpoint 4 (rw) at .loop" (debug-command session "watch .loop in count rw")))
    (fiveam:is (search "Stopped: until" (debug-command session "until .loop in count")))
    (fiveam:is (search "Error" (debug-command session "break .loop in nonesuch")))))

;;; mem() in conditions

(fiveam:test debug-break-condition-reads-memory
  (let ((session (%dbg-session)))
    (setf (mref (debug-session-machine session) 'ram #x200) 3)
    (debug-break session #x102 :condition "mem(0x200) == 3")
    (multiple-value-bind (reason steps) (debug-continue session)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 1 steps))))
  (let ((session (%dbg-session)))
    (debug-break session #x102 :condition "mem(0x200 + 1) == 3")
    (fiveam:is (eq :trap (debug-continue session)))))

(fiveam:test debug-condition-memory-read-does-not-trigger-watchpoints
  (let ((session (%dbg-session)))
    (debug-watch session #x200 :access :read-write)
    (debug-break session #x102 :condition "mem(0x200) == 0")
    (fiveam:is (eq :breakpoint (debug-continue session)))))

;;; Stack watchpoints

(defun %dbg-stack-session ()
  (let ((m (make-machine 'stack-test-machine)))
    (load-program m (list #x01 5 #x01 7 #x04 #x00)) ; psh 5, psh 7, add, hlt
    (make-debug-session m)))

(fiveam:test debug-watch-stack-write-reports-slot-values
  (let ((session (%dbg-stack-session)))
    (let ((wp (debug-watch session "ds")))
      (fiveam:is (string= "ds" (watchpoint-label wp))))
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (null (watch-hit-old hit)))
      (fiveam:is (= 5 (watch-hit-new hit))))
    (fiveam:is (search "Watchpoint 1 (w) ds: - -> 7" (debug-command session "continue")))))

(fiveam:test debug-watch-stack-read-fires-on-pop
  (let ((session (%dbg-stack-session)))
    (debug-watch session "ds" :access :read)
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 3 steps))
      (fiveam:is (= 7 (watch-hit-new hit))))))

(fiveam:test debug-watch-stack-slot-ignores-other-slots
  (let ((session (%dbg-stack-session)))
    (let ((wp (debug-watch session "ds" :index 1)))
      (fiveam:is (string= "ds[1]" (watchpoint-label wp))))
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (= 7 (watch-hit-new hit))))
    (fiveam:is (eq :trap (debug-continue session)))))

(fiveam:test debug-watch-stack-command-and-bad-slot
  (let ((session (%dbg-stack-session)))
    (fiveam:is (search "Watchpoint 1 (w) at ds[1]" (debug-command session "watch ds[1]")))
    (fiveam:is (search "Error" (debug-command session "watch ds[99]")))
    (fiveam:signals error (debug-break session 0 :condition "ds == 1"))))

(fiveam:test debug-watch-stack-pointer-write
  (let ((session (%dbg-stack-session)))
    (let ((m (debug-session-machine session)))
      (debug-watch session "ds")
      (%arm session)
      (setf (stack-pointer m 'ds) 0)
      (%disarm session)
      (let ((hit (debug-session-watch-hit session)))
        (fiveam:is (= 0 (watch-hit-new hit)))
        (fiveam:is (= 0 (watch-hit-old hit))))))
  (let ((session (%dbg-stack-session)))
    (debug-watch session "ds" :index 0)
    (%arm session)
    (setf (stack-pointer (debug-session-machine session) 'ds) 0)
    (%disarm session)
    (fiveam:is (null (debug-session-watch-hit session)))))

(fiveam:test debug-command-print-expression
  (let ((session (%dbg-session)))
    (setf (mref (debug-session-machine session) 'ram #x200) 3)
    (fiveam:is (search "mem(0x200) + 1 = 4" (debug-command session "print mem(0x200) + 1")))
    (fiveam:is (search "count.loop = 258" (debug-command session "print count.loop")))
    (fiveam:is (search "Error" (debug-command session "print 1 +")))))
