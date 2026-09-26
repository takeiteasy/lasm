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

;;; Step back

(defun %dbg-history-session (&key (history 100))
  (let* ((a (%dbg-assembly))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (make-debug-session m :assembly a :history history)))

(defun %dbg-snapshot (session)
  (machine-snapshot (debug-session-machine session)))

(fiveam:test debug-step-back-restores-the-previous-state
  (let ((session (%dbg-history-session)))
    (debug-step session 2)
    (let ((before (%dbg-snapshot session)))
      (debug-step session 3)
      (multiple-value-bind (reason undone) (debug-step-back session 3)
        (fiveam:is (eq :back reason))
        (fiveam:is (= 3 undone))
        (fiveam:is (equal before (%dbg-snapshot session)))))))

(fiveam:test debug-step-back-defaults-to-one-step
  (let ((session (%dbg-history-session)))
    (debug-step session 2)
    (let ((before (%dbg-snapshot session)))
      (debug-step session 1)
      (debug-step-back session)
      (fiveam:is (equal before (%dbg-snapshot session))))))

(fiveam:test debug-step-back-crosses-checkpoints
  (let ((*debug-checkpoint-interval* 2)
        (session (%dbg-history-session)))
    (debug-step session 1)
    (let ((before (%dbg-snapshot session)))
      (fiveam:is (eq :trap (debug-continue session)))
      (fiveam:is (eq :back (debug-step-back session 8)))
      (fiveam:is (equal before (%dbg-snapshot session))))))

(fiveam:test debug-step-back-undoes-a-trapping-step
  (let ((session (%dbg-history-session)))
    (debug-step session 8)
    (let ((before (%dbg-snapshot session)))
      (fiveam:is (eq :trap (debug-step session 1)))
      (debug-step-back session 1)
      (fiveam:is (equal before (%dbg-snapshot session))))))

(fiveam:test debug-step-back-past-the-start-reports-history-start
  (let* ((session (%dbg-history-session))
         (initial (%dbg-snapshot session)))
    (debug-step session 2)
    (multiple-value-bind (reason undone) (debug-step-back session 5)
      (fiveam:is (eq :history-start reason))
      (fiveam:is (= 2 undone))
      (fiveam:is (equal initial (%dbg-snapshot session))))))

(fiveam:test debug-step-back-with-nothing-run-undoes-nothing
  (multiple-value-bind (reason undone) (debug-step-back (%dbg-history-session))
    (fiveam:is (eq :history-start reason))
    (fiveam:is (= 0 undone))))

(fiveam:test debug-step-back-history-limits-how-far-back-it-goes
  (let ((session (%dbg-history-session :history 3)))
    (dotimes (i 10) (debug-step session 1))
    (multiple-value-bind (reason undone) (debug-step-back session 100)
      (fiveam:is (eq :history-start reason))
      (fiveam:is (<= 3 undone (+ 3 *debug-anchor-interval*))))))

(fiveam:test debug-step-forward-after-back-is-deterministic
  (let ((session (%dbg-history-session)))
    (debug-step session 5)
    (let ((expected (%dbg-snapshot session)))
      (debug-step-back session 3)
      (debug-step session 3)
      (fiveam:is (equal expected (%dbg-snapshot session))))))

(fiveam:test debug-step-back-repeats
  (let ((session (%dbg-history-session)))
    (debug-step session 2)
    (let ((two (%dbg-snapshot session)))
      (debug-step session 4)
      (debug-step-back session 2)
      (debug-step-back session 2)
      (fiveam:is (equal two (%dbg-snapshot session))))))

(fiveam:test debug-step-back-needs-history
  (fiveam:signals error (debug-step-back (%dbg-session)))
  (fiveam:is (search "history is off" (debug-command (%dbg-session) "back"))))

(fiveam:test debug-step-back-refuses-a-device-without-save
  (let ((session (%dbg-history-session)))
    (attach-device (debug-session-machine session) 'plain :id 9)
    (debug-step session 1)
    (fiveam:signals error (debug-step-back session 1))
    (fiveam:is (search "no :save" (debug-command session "back")))))

(fiveam:test debug-command-back
  (let ((session (%dbg-history-session)))
    (debug-command session "step 3")
    (fiveam:is (search "Stopped: back  steps=2" (debug-command session "back 2")))
    (fiveam:is (search "bad count" (debug-command session "back 0")))
    (fiveam:is (search "bad count" (debug-command session "back 2 cycles")))))

;;; Diff checkpoints

(fiveam:test debug-step-back-restores-memory-across-delta-and-anchor-checkpoints
  (let ((*debug-checkpoint-interval* 1)
        (*debug-anchor-interval* 2)
        (session (%dbg-history-session)))
    (let ((snapshots (loop repeat 9
                           collect (%dbg-snapshot session)
                           do (debug-step session 1))))
      (loop for expected in (reverse snapshots)
            do (debug-step-back session 1)
               (fiveam:is (equal expected (%dbg-snapshot session)))))))

(fiveam:test debug-delta-checkpoint-stores-only-the-changed-cells
  (let ((*debug-anchor-interval* 16)
        (session (%dbg-history-session)))
    (debug-step session 1)
    (debug-set session #x300 7)
    (debug-step session 1)
    (let ((newest (first (debug-session-checkpoints session))))
      (fiveam:is (not (checkpoint-anchor-p newest)))
      (fiveam:is (equalp '((:memory ram) (#x300 . #(7)))
                        (first (checkpoint-diff newest))))
      (fiveam:is (null (search ":RUNS" (prin1-to-string (checkpoint-snapshot newest))))))))

(fiveam:test debug-step-back-trims-history-to-an-anchor
  (let ((*debug-checkpoint-interval* 1)
        (*debug-anchor-interval* 3)
        (session (%dbg-history-session :history 4)))
    (dotimes (i 8) (debug-step session 1))
    (fiveam:is (checkpoint-anchor-p (car (last (debug-session-checkpoints session)))))
    (let ((expected (%dbg-snapshot session)))
      (debug-step session 1)
      (debug-step-back session 1)
      (fiveam:is (equal expected (%dbg-snapshot session))))))

;;; Reverse continue

(defun %dbg-snapshots-at (session steps)
  "Run SESSION forward to its last step in STEPS, returning (STEP . SNAPSHOT) for each."
  (let ((now 0))
    (loop for step in steps
          do (debug-step session (- step now))
             (setf now step)
          collect (cons step (%dbg-snapshot session)))))

(fiveam:test debug-reverse-continue-visits-each-earlier-breakpoint-hit
  (let* ((session (%dbg-history-session))
         (states (%dbg-snapshots-at session '(2 4 6))))
    (debug-step session 3)
    (debug-break session #x103)
    (loop for (step . snapshot) in (reverse states)
          do (multiple-value-bind (reason undone) (debug-reverse-continue session)
               (fiveam:is (eq :breakpoint reason))
               (fiveam:is (= step (debug-session-step-count session)))
               (fiveam:is (plusp undone))
               (fiveam:is (equal snapshot (%dbg-snapshot session)))))
    (multiple-value-bind (reason undone) (debug-reverse-continue session)
      (fiveam:is (eq :history-start reason))
      (fiveam:is (= 2 undone))
      (fiveam:is (= 0 (debug-session-step-count session))))))

(fiveam:test debug-reverse-continue-crosses-checkpoint-segments
  (let* ((*debug-checkpoint-interval* 2)
         (*debug-anchor-interval* 2)
         (session (%dbg-history-session))
         (states (%dbg-snapshots-at session '(2 4 6))))
    (debug-step session 3)
    (debug-break session #x103)
    (dolist (state (reverse states))
      (debug-reverse-continue session)
      (fiveam:is (equal (cdr state) (%dbg-snapshot session))))))

(fiveam:test debug-reverse-continue-does-not-stop-where-it-starts
  (let ((session (%dbg-history-session)))
    (debug-break session #x103)
    (debug-continue session)
    (multiple-value-bind (reason undone) (debug-reverse-continue session)
      (fiveam:is (eq :history-start reason))
      (fiveam:is (= 2 undone)))))

(fiveam:test debug-reverse-continue-skips-a-false-condition
  (let* ((session (%dbg-history-session))
         (states (%dbg-snapshots-at session '(2 4 6))))
    (debug-step session 3)
    (debug-break session #x103 :condition "x == 1")
    (fiveam:is (eq :breakpoint (debug-reverse-continue session)))
    (fiveam:is (= 4 (debug-session-step-count session)))
    (fiveam:is (equal (cdr (assoc 4 states)) (%dbg-snapshot session)))))

(fiveam:test debug-reverse-continue-stops-after-a-watchpoint-access
  (let ((session (%dbg-history-session)))
    (debug-step session 9)
    (let ((expected (progn (debug-step-back session 1) (%dbg-snapshot session))))
      (debug-step session 1)
      (debug-watch session #x10)
      (multiple-value-bind (reason undone hit) (debug-reverse-continue session)
        (fiveam:is (eq :watchpoint reason))
        (fiveam:is (= 1 undone))
        (fiveam:is (= #x10 (watchpoint-address (watch-hit-watchpoint hit))))
        (fiveam:is (= 0 (watch-hit-old hit)))
        (fiveam:is (= 8 (debug-session-step-count session)))
        (fiveam:is (equal expected (%dbg-snapshot session)))))))

(fiveam:test debug-reverse-continue-to-stops-at-the-address
  (let ((session (%dbg-history-session)))
    (debug-step session 9)
    (multiple-value-bind (reason undone) (debug-reverse-continue-to session ".loop" :scope "count")
      (fiveam:is (eq :until reason))
      (fiveam:is (= 4 undone))
      (fiveam:is (= 5 (debug-session-step-count session))))
    (debug-reverse-continue-to session #x100)
    (fiveam:is (= 0 (debug-session-step-count session)))
    (fiveam:is (eq :history-start (debug-reverse-continue-to session #x999)))))

(fiveam:test debug-reverse-continue-then-continue-reaches-the-same-hit
  (let ((session (%dbg-history-session)))
    (debug-break session #x103)
    (debug-continue session)
    (debug-continue session)
    (let ((expected (%dbg-snapshot session)))
      (debug-continue session)
      (debug-reverse-continue session)
      (fiveam:is (equal expected (%dbg-snapshot session)))
      (fiveam:is (= 4 (debug-session-step-count session))))))

(fiveam:test debug-reverse-continue-needs-history
  (fiveam:signals error (debug-reverse-continue (%dbg-session)))
  (fiveam:is (search "history is off" (debug-command (%dbg-session) "rc"))))

(fiveam:test debug-reverse-continue-with-nothing-run-stops-at-the-start
  (multiple-value-bind (reason undone) (debug-reverse-continue (%dbg-history-session))
    (fiveam:is (eq :history-start reason))
    (fiveam:is (= 0 undone))))

(fiveam:test debug-command-reverse-continue-and-until
  (let ((session (%dbg-history-session)))
    (debug-command session "break 0x103")
    (debug-command session "step 6")
    (fiveam:is (search "Stopped: breakpoint  steps=2" (debug-command session "reverse-continue")))
    (fiveam:is (search "Stopped: breakpoint  steps=2" (debug-command session "rc")))
    (fiveam:is (search "Stopped: history-start" (debug-command session "rc")))
    (debug-command session "step 4")
    (fiveam:is (search "Stopped: until  steps=1" (debug-command session "reverse-until .loop in count")))
    (fiveam:is (search "missing address" (debug-command session "reverse-until")))))

;;; Cycle budgets

(defun %dbg-program-session (source)
  (let* ((a (assemble source :machine 'emu-test-machine :origin #x100))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (make-debug-session m :assembly a)))

(fiveam:test debug-step-cycles-stops-when-the-budget-is-spent
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-step-cycles session 3)
      (fiveam:is (eq :step reason))
      (fiveam:is (= 3 steps))
      (fiveam:is (= 3 (machine-cycles (debug-session-machine session)))))))

(fiveam:test debug-step-cycles-overshoots-by-at-most-one-instruction
  (let ((session (%dbg-program-session "pen
        pen
        hlt")))
    (multiple-value-bind (reason steps) (debug-step-cycles session 1)
      (fiveam:is (eq :step reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 5 (machine-cycles (debug-session-machine session)))))))

(fiveam:test debug-step-cycles-ignores-breakpoints
  (let ((session (%dbg-session)))
    (debug-break session "count.loop")
    (fiveam:is (= 3 (nth-value 1 (debug-step-cycles session 3))))))

(fiveam:test debug-step-cycles-stops-on-a-trap
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-step-cycles session 100)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 9 steps)))))

(fiveam:test debug-step-cycles-stops-on-a-watchpoint
  (let ((session (%dbg-session)))
    (debug-watch session "x")
    (multiple-value-bind (reason steps) (debug-step-cycles session 5)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps)))))

(fiveam:test debug-step-cycles-guards-runaway-programs
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-step-cycles session 100 :max-steps 2)
      (fiveam:is (eq :max-steps reason))
      (fiveam:is (= 2 steps)))))

(fiveam:test debug-continue-cycles-stops-at-the-budget
  (let ((session (%dbg-session)))
    (multiple-value-bind (reason steps) (debug-continue session :cycles 3)
      (fiveam:is (eq :max-cycles reason))
      (fiveam:is (= 3 steps)))))

(fiveam:test debug-continue-cycles-yields-to-a-breakpoint
  (let ((session (%dbg-session)))
    (debug-break session "count.loop")
    (multiple-value-bind (reason steps) (debug-continue session :cycles 10)
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (= 1 steps)))))

(fiveam:test debug-continue-cycles-runs-an-idle-machine-to-the-budget
  (let ((session (%dbg-program-session "slp
        hlt")))
    (fiveam:is (eq :max-cycles (debug-continue session :cycles 5)))
    (fiveam:is (eq :idle (debug-continue (%dbg-program-session "slp
        hlt"))))))

(fiveam:test cycle-budgets-need-declared-costs
  (let ((session (make-debug-session (make-machine 'dbg-bank-test-machine))))
    (fiveam:signals error (debug-step-cycles session 5))
    (fiveam:signals error (debug-continue session :cycles 5))
    (fiveam:is (search "declares no (cycles n)" (debug-command session "step 5 cycles")))))

(fiveam:test debug-command-cycle-budgets
  (let ((session (%dbg-session)))
    (fiveam:is (search "steps=3" (debug-command session "step 3 cycles")))
    (fiveam:is (search "Stopped: max-cycles" (debug-command session "continue 2 cycles")))))

(fiveam:test debug-command-rejects-a-bad-step-count
  (let ((session (%dbg-session)))
    (dolist (line '("step -3" "step 0" "step foo" "step 2 foo" "continue 3" "continue foo cycles"))
      (fiveam:is (search "bad count" (debug-command session line)) "~A" line))
    (fiveam:is (= 0 (machine-cycles (debug-session-machine session))))))

;;; Writable inspection

(fiveam:test debug-set-writes-registers-and-flags
  (let* ((session (%dbg-session))
         (m (debug-session-machine session)))
    (fiveam:is (= 5 (debug-set session "x" 5)))
    (fiveam:is (= 5 (sref m 'x)))
    (fiveam:is (= 1 (debug-set session "x" 257)))
    (debug-set session "z" 1)
    (fiveam:is (= 1 (flag m 'z)))
    (debug-set session "z" 0)
    (fiveam:is (= 0 (flag m 'z)))
    (fiveam:signals error (debug-set session "nonesuch" 1))
    (fiveam:signals error (debug-set session "x" nil))))

(fiveam:test debug-set-writes-memory
  (let* ((session (%dbg-session))
         (m (debug-session-machine session)))
    (fiveam:is (= 7 (debug-set session #x200 7)))
    (fiveam:is (= 7 (mpeek m 'ram #x200)))
    (debug-set session "count.loop" #xEA)
    (fiveam:is (= #xEA (mpeek m 'ram #x102)))
    (debug-set session ".loop" #xCA :scope "count")
    (fiveam:is (= #xCA (mpeek m 'ram #x102)))
    (fiveam:signals error (debug-set session "count.nonesuch" 1))))

(fiveam:test debug-set-banked-register-alias-and-stack
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (debug-set session "v" 9 :index 2)
    (fiveam:is (= 9 (regref m 'v 2)))
    (fiveam:signals error (debug-set session "v" 1))
    (fiveam:signals error (debug-set session "v" 1 :index 9)))
  (let ((session (%dbg-alias-session)))
    (debug-set session "b" 3)
    (fiveam:is (= 3 (regref (debug-session-machine session) 'reg 1))))
  (let* ((session (%dbg-stack-session))
         (m (debug-session-machine session)))
    (debug-step session 2)
    (debug-set session "ds" 9 :index 0)
    (fiveam:is (= 9 (stack-ref m 'ds 1)))
    (fiveam:is (= 7 (stack-ref m 'ds 0)))
    (fiveam:signals error (debug-set session "ds" 1 :index 5))
    (fiveam:signals error (debug-set session "ds" 1))))

(fiveam:test debug-set-does-not-trigger-watchpoints
  (let ((session (%dbg-session)))
    (debug-watch session "x" :access :read-write)
    (debug-watch session #x200 :access :read-write)
    (%arm session)
    (debug-set session "x" 1)
    (debug-set session #x200 1)
    (%disarm session)
    (fiveam:is (null (debug-session-watch-hit session)))))

(defmachine dbg-region-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
              (region rom #x000 #x0FF :kind :rom)
              (region io #x100 #x1FF :kind :device)))

(fiveam:test debug-set-memory-regions
  (let* ((m (make-machine 'dbg-region-test-machine))
         (session (make-debug-session m)))
    (debug-set session #x10 42)
    (fiveam:is (= 42 (mpeek m 'ram #x10)))
    (fiveam:signals error (debug-set session #x100 1))))

(fiveam:test debug-command-set
  (let* ((session (%dbg-session))
         (m (debug-session-machine session)))
    (fiveam:is (search "x = 5" (debug-command session "set x = 5")))
    (fiveam:is (= 5 (sref m 'x)))
    (debug-command session "set x = x + 1")
    (fiveam:is (= 6 (sref m 'x)))
    (debug-command session "set z = 1")
    (fiveam:is (= 1 (flag m 'z)))
    (debug-command session "set z = 0")
    (fiveam:is (= 0 (flag m 'z)))
    (fiveam:is (search "pc = 258" (debug-command session "set pc = count.loop")))
    (fiveam:is (= #x102 (%pc session)))
    (fiveam:is (search "$200 = 7" (debug-command session "set $200 = 7")))
    (fiveam:is (= 7 (mpeek m 'ram #x200)))
    (debug-command session "set .loop in count = 0xCA")
    (fiveam:is (= #xCA (mpeek m 'ram #x102)))
    (fiveam:is (search "usage" (debug-command session "set x")))
    (fiveam:is (search "usage" (debug-command session "set = 1")))
    (fiveam:is (search "Error" (debug-command session "set nonesuch = 1")))
    (fiveam:is (search "Error" (debug-command session "set x = nonesuch")))))

(fiveam:test debug-command-set-indexed-targets
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (debug-command session "set v[2] = $ff")
    (fiveam:is (= #xff (regref m 'v 2))))
  (let ((session (%dbg-stack-session)))
    (debug-step session 2)
    (debug-command session "set ds[1] = 3")
    (fiveam:is (= 3 (stack-ref (debug-session-machine session) 'ds 0)))))

(fiveam:test debug-set-is-undone-by-step-back
  (let* ((a (%dbg-assembly))
         (m (make-machine 'emu-test-machine)))
    (load-program m a)
    (let ((session (make-debug-session m :assembly a :history 100)))
      (debug-step session 2)
      (debug-command session "set x = 9")
      (debug-step session 1)
      (debug-step-back session 1)
      (fiveam:is (= 9 (sref m 'x)))
      (debug-step-back session 2)
      (fiveam:is (= 0 (sref m 'x))))))

;;; Stack depth and contents

(fiveam:test debug-set-stack-depth
  (let* ((session (%dbg-stack-session))
         (m (debug-session-machine session)))
    (debug-step session 2)
    (fiveam:is (= 1 (debug-set session "ds" 1 :index :depth)))
    (fiveam:is (= 1 (stack-depth m 'ds)))
    (fiveam:is (= 4 (debug-set session "ds" 4 :index :depth)))
    (fiveam:is (= 7 (stack-ref m 'ds 2))) ; a grown stack uncovers the old cells
    (debug-set session "ds" 0 :index :depth)
    (fiveam:is (= 0 (stack-depth m 'ds)))
    (fiveam:signals stack-pointer-out-of-range (debug-set session "ds" 33 :index :depth))
    (fiveam:signals stack-pointer-out-of-range (debug-set session "ds" -1 :index :depth))
    (fiveam:signals error (debug-set session "ds" nil :index :depth))
    (fiveam:signals error (debug-set session "pc" 1 :index :depth))
    (fiveam:signals error (debug-set session #x10 1 :index :depth))))

(fiveam:test debug-set-stack-contents
  (let* ((session (%dbg-stack-session))
         (m (debug-session-machine session)))
    (fiveam:is (equal '(1 2 255) (debug-set session "ds" '(1 2 511))))
    (fiveam:is (= 3 (stack-depth m 'ds)))
    (fiveam:is (= 255 (stack-ref m 'ds 0)))
    (fiveam:is (= 1 (stack-ref m 'ds 2)))
    (fiveam:signals error (debug-set session "ds" (make-list 33 :initial-element 0)))
    (fiveam:signals error (debug-set session "ds" '(1 :two)))
    (fiveam:is (= 3 (stack-depth m 'ds)))
    (fiveam:is (= 1 (stack-ref m 'ds 2)))
    (fiveam:signals error (debug-set session "ds" '(1) :index 0))
    (fiveam:is (null (debug-set session "ds" '())))
    (fiveam:is (= 0 (stack-depth m 'ds)))
    (fiveam:signals error (debug-set session "pc" '(1)))))

(fiveam:test debug-set-stack-does-not-trigger-watchpoints
  (let ((session (%dbg-stack-session)))
    (debug-watch session "ds" :access :read-write)
    (%arm session)
    (debug-set session "ds" '(1 2))
    (debug-set session "ds" 1 :index :depth)
    (%disarm session)
    (fiveam:is (null (debug-session-watch-hit session)))))

(fiveam:test debug-command-set-stack
  (let* ((session (%dbg-stack-session))
         (m (debug-session-machine session)))
    (fiveam:is (search "ds = [1, 5, 3]" (debug-command session "set ds = [1, 2 + 3, 0x103]")))
    (fiveam:is (= 3 (stack-depth m 'ds)))
    (fiveam:is (= 3 (stack-ref m 'ds 0)))
    (fiveam:is (search "ds.depth = 1" (debug-command session "set ds.depth = 1")))
    (fiveam:is (= 1 (stack-depth m 'ds)))
    (fiveam:is (search "DS.DEPTH = 2" (debug-command session "set DS.DEPTH = 1 + 1")))
    (fiveam:is (search "ds = []" (debug-command session "set ds = []")))
    (fiveam:is (= 0 (stack-depth m 'ds)))
    (fiveam:is (search "Error" (debug-command session "set ds.depth = 99")))
    (fiveam:is (search "Error" (debug-command session "set pc.depth = 1")))
    (fiveam:is (search "Error" (debug-command session "set ds = [1, nonesuch]")))
    (fiveam:is (search "Error" (debug-command session "set pc = [1]")))
    (fiveam:is (search "usage" (debug-command session "set ds = ")))))

(fiveam:test debug-set-register-stack-pointer-machine
  (let* ((m (make-machine 'interrupt-pointer-stack-test-machine))
         (session (make-debug-session m)))
    (debug-command session "set sp = $100")
    (debug-command session "set $100 = 42")
    (fiveam:is (= #x100 (sref m 'sp)))
    (fiveam:is (= 42 (sp-pop m 'sp 'ram :down)))
    (fiveam:is (= #x101 (sref m 'sp)))
    (fiveam:is (search "Error" (debug-command session "set sp.depth = 1")))))

;;; CPU-faithful writes

(fiveam:test debug-write-follows-region-policy
  (let* ((m (make-machine 'region-test-machine))
         (session (make-debug-session m))
         (*device-log* nil))
    (fiveam:is (= 5 (debug-write session #x10 5)))
    (fiveam:is (= 5 (mpeek m 'ram #x10)))
    (multiple-value-bind (cell stored-p) (debug-write session #x20 9)
      (fiveam:is (= 0 cell))
      (fiveam:is (null stored-p)))
    (fiveam:is (= 0 (mpeek m 'ram #x20)))
    (fiveam:signals memory-write-protected (debug-write session #x30 1))
    (multiple-value-bind (cell stored-p) (debug-write session #x41 #x105)
      (fiveam:is (= 5 cell))
      (fiveam:is (eq t stored-p)))
    (fiveam:is (equal (list (list :write #x41 5)) *device-log*))
    (debug-write session #x50 1)
    (fiveam:signals error (debug-write session #x10 nil))))

(fiveam:test debug-write-banks
  (let* ((m (make-machine 'bank-test-machine))
         (session (make-debug-session m)))
    (setf (current-bank m 'bram) 1)
    (debug-write session 16 7 :bank 1)
    (fiveam:is (= 7 (bank-peek m 'bram 1 16)))
    (fiveam:signals error (debug-write session 16 7 :bank 2))
    (fiveam:is (= 0 (bank-peek m 'bram 2 16)))))

(fiveam:test debug-write-does-not-call-the-access-hook
  (let* ((m (make-machine 'region-test-machine))
         (session (make-debug-session m))
         (calls 0))
    (setf (machine-access-hook m) (lambda (&rest args) (declare (ignore args)) (incf calls)))
    (debug-write session #x10 1)
    (fiveam:is (= 0 calls))
    (fiveam:is (functionp (machine-access-hook m)))))

(fiveam:test debug-write-rejects-storage-targets
  (let ((session (%dbg-session)))
    (fiveam:signals error (debug-write session "x" 1))
    (fiveam:signals error (debug-write session "z" 1))))

(fiveam:test debug-command-write
  (let* ((m (make-machine 'region-test-machine))
         (session (make-debug-session m))
         (*device-log* nil))
    (fiveam:is (search "$10 = 5" (debug-command session "write $10 = 5")))
    (fiveam:is (= 5 (mpeek m 'ram #x10)))
    (fiveam:is (search "(dropped)" (debug-command session "write $20 = 5")))
    (fiveam:is (search "Error" (debug-command session "write $30 = 5")))
    (fiveam:is (search "$40 = 7" (debug-command session "write $40 = 3 + 4")))
    (fiveam:is (equal (list (list :write #x40 7)) *device-log*))
    (fiveam:is (search "usage" (debug-command session "write $10")))
    (fiveam:is (search "Error" (debug-command session "write $10 = [1]"))))
  (let ((session (%dbg-session)))
    (fiveam:is (search "$200 = 7" (debug-command session "write $200 = 7")))
    (fiveam:is (search "count.loop = 202" (debug-command session "write count.loop = 0xCA")))
    (let ((reply (debug-command session "write x = 5")))
      (fiveam:is (search "Error" reply))
      (fiveam:is (search "use set" reply)))))

(fiveam:test documented-debugger-api-is-exported
  (dolist (name '("DEBUG-STEP" "DEBUG-STEP-CYCLES" "DEBUG-STEP-BACK" "DEBUG-CONTINUE"
                  "DEBUG-CONTINUE-TO" "DEBUG-REVERSE-CONTINUE" "DEBUG-REVERSE-CONTINUE-TO"
                  "DEBUG-SET" "DEBUG-WRITE" "DEBUG-SET-BANK" "DEBUGGER-REPL"))
    (fiveam:is (eq :external (nth-value 1 (find-symbol name :lasm))) "~A" name)))

;;; Indexed print and stack depth reads

(fiveam:test debug-command-print-banked-register-cell
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (setf (regref m 'v 2) 9)
    (fiveam:is (search "v[2] = 9" (debug-command session "print v[2]")))
    (fiveam:is (search "v[0x2] = 9" (debug-command session "print v[0x2]")))
    (fiveam:is (search "index 4 is out of range" (debug-command session "print v[4]")))
    (fiveam:is (search "Error" (debug-command session "print pc[0]")))
    (fiveam:is (search "v[1] + 1 = 1" (debug-command session "print v[1] + 1")))
    (fiveam:is (search "Error" (debug-command session "print nonesuch[1]")))))

(fiveam:test debug-command-print-aliased-bank-cell
  (let ((session (%dbg-alias-session)))
    (fiveam:is (search "reg[1] = 7" (debug-command session "print reg[1]")))
    (fiveam:is (search "Error" (debug-command session "print b[0]")))))

(fiveam:test debug-command-print-stack-slot
  (let ((session (%dbg-stack-session)))
    (debug-step session 2)
    (fiveam:is (search "ds[0] = 5" (debug-command session "print ds[0]")))
    (fiveam:is (search "ds[1] = 7" (debug-command session "print ds[1]")))
    (fiveam:is (search "no live slot 2" (debug-command session "print ds[2]")))
    (fiveam:is (search "out of range" (debug-command session "print ds[99]")))))

(fiveam:test debug-command-print-stack-depth
  (let ((session (%dbg-stack-session)))
    (fiveam:is (search "ds.depth = 0" (debug-command session "print ds.depth")))
    (debug-step session 2)
    (fiveam:is (search "ds.depth = 2" (debug-command session "print ds.depth")))
    (fiveam:is (search "ds.depth + 1 = 3" (debug-command session "print ds.depth + 1")))
    (fiveam:is (search "DS.DEPTH = 2" (debug-command session "print DS.DEPTH")))
    (fiveam:is (search "not a fixed stack" (debug-command session "print pc.depth")))))

(fiveam:test debug-break-condition-reads-stack-depth
  (let ((session (%dbg-stack-session)))
    (debug-break session 4 :condition "ds.depth == 2")
    (fiveam:is (eq :breakpoint (debug-continue session)))
    (fiveam:is (= 2 (stack-depth (debug-session-machine session) 'ds)))))

;;; Indexed cells inside expressions

(fiveam:test debug-command-print-indexed-in-expression
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (setf (regref m 'v 1) 1 (regref m 'v 2) 9)
    (fiveam:is (search "v[2] * 2 = 18" (debug-command session "print v[2] * 2")))
    (fiveam:is (search "v[v[1]] = 1" (debug-command session "print v[v[1]]")))
    (fiveam:is (search "v[1 + 1] = 9" (debug-command session "print v[1 + 1]")))
    (fiveam:is (search "index 4 is out of range" (debug-command session "print v[1] + v[4]")))
    (fiveam:is (search "index 9 is out of range" (debug-command session "print v[v[2]]")))))

(fiveam:test debug-command-print-stack-slots-in-expression
  (let ((session (%dbg-stack-session)))
    (debug-step session 2)
    (fiveam:is (search "ds[0] + ds[1] = 12" (debug-command session "print ds[0] + ds[1]")))
    (fiveam:is (search "no live slot 2" (debug-command session "print ds[0] + ds[2]")))))

(fiveam:test debug-break-condition-reads-stack-slot
  (let ((session (%dbg-stack-session)))
    (debug-break session 4 :condition "ds[0] == 5 && ds[1] == 7")
    (fiveam:is (eq :breakpoint (debug-continue session)))
    (fiveam:is (= 2 (stack-depth (debug-session-machine session) 'ds))))
  (let ((session (%dbg-stack-session)))
    (debug-break session 4 :condition "ds[0] == 6")
    (fiveam:is (eq :trap (debug-continue session)))))

(fiveam:test debug-break-condition-indexed-cell-checked-when-set
  (let ((session (%dbg-stack-session)))
    (fiveam:signals error (debug-break session 4 :condition "ds[99] == 1"))
    (fiveam:signals error (debug-break session 4 :condition "pc[0] == 1"))
    (fiveam:signals error (debug-break session 4 :condition "ds.depth[0] == 1"))
    (fiveam:signals error (debug-break session 4 :condition "nonesuch[1] == 1"))
    (fiveam:signals error (debug-break session 4 :condition "ds[1 == 1"))))

(fiveam:test debug-break-condition-dead-slot-error-stops
  (let ((session (%dbg-stack-session)))
    (debug-break session 4 :condition "ds[ds.depth] == 1")
    (fiveam:is (eq :breakpoint (debug-continue session)))
    (fiveam:is (search "no live slot" (princ-to-string (debug-session-condition-error session))))))

(fiveam:test debug-break-condition-reads-bank-cell
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (setf (regref m 'v 2) 9)
    (fiveam:is (%breakpoint-triggered-p session (debug-break session 0 :condition "v[2] == 9")))
    (fiveam:is (not (%breakpoint-triggered-p session (debug-break session 0 :condition "v[2] == 8"))))
    (fiveam:is (null (debug-session-condition-error session)))))

;;; Computed indices in set and watch targets

(fiveam:test debug-command-set-and-watch-computed-index
  (let* ((m (make-machine 'dbg-bank-test-machine))
         (session (make-debug-session m)))
    (setf (regref m 'v 0) 2)
    (fiveam:is (search "v[v[0]] = 5" (debug-command session "set v[v[0]] = 5")))
    (fiveam:is (= 5 (regref m 'v 2)))
    (fiveam:is (search "v[1 + 1] = 6" (debug-command session "set v[1 + 1] = 6")))
    (fiveam:is (= 6 (regref m 'v 2)))
    (fiveam:is (search "at v[2]" (debug-command session "watch v[v[0]] r")))
    (fiveam:is (search "out of range" (debug-command session "set v[v[0] + 9] = 1")))
    (fiveam:is (search "Error" (debug-command session "watch v[nonesuch]")))))

;;; Stack depth watchpoints

(fiveam:test debug-watch-stack-depth-fires-on-push-and-pop
  (let ((session (%dbg-stack-session)))
    (let ((wp (debug-watch session "ds.depth")))
      (fiveam:is (string= "ds.depth" (watchpoint-label wp))))
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 0 (watch-hit-old hit)))
      (fiveam:is (= 1 (watch-hit-new hit))))
    (fiveam:is (search "Watchpoint 1 (w) ds.depth: 1 -> 2" (debug-command session "continue")))
    (multiple-value-bind (reason steps hit) (debug-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (= 2 (watch-hit-old hit)))
      (fiveam:is (= 1 (watch-hit-new hit))))))

(fiveam:test debug-watch-stack-depth-by-index-and-command
  (let ((session (%dbg-stack-session)))
    (fiveam:is (string= "ds.depth" (watchpoint-label (debug-watch session "ds" :index :depth :access :read-write)))))
  (let ((session (%dbg-stack-session)))
    (fiveam:is (search "Watchpoint 1 (w) at ds.depth" (debug-command session "watch ds.depth")))
    (fiveam:is (search "Watchpoint 2 (rw) at ds.depth" (debug-command session "watch ds.depth rw")))))

(fiveam:test debug-watch-stack-depth-rejects-unwatchable-targets
  (let ((session (%dbg-stack-session)))
    (fiveam:signals error (debug-watch session "ds.depth" :access :read))
    (fiveam:signals error (debug-watch session "ds" :index :depth :access :read))
    (fiveam:signals error (debug-watch session #x10 :index :depth))
    (fiveam:signals error (debug-watch session "pc.depth"))
    (fiveam:is (search "Error" (debug-command session "watch ds.depth r")))
    (fiveam:is (null (debug-watchpoints session)))))

(fiveam:test debug-watch-stack-slot-ignores-depth-changes
  (let ((session (%dbg-stack-session)))
    (debug-watch session "ds" :index 1)
    (fiveam:is (= 2 (nth-value 1 (debug-continue session))))))

(fiveam:test where-shows-the-line-from-the-included-file
  (let* ((a (assemble-file (asdf:system-relative-pathname
                            :lasm "tests/fixtures/include/where.asm")
                           :machine 'instr-test-machine))
         (m (make-machine 'instr-test-machine)))
    (load-program m a)
    (let ((session (make-debug-session m :assembly a)))
      (setf (sref m 'pc) 1)
      (let ((text (debug-where-text session)))
        (fiveam:is (search "where-lib.asm:2:nop" text))
        (fiveam:is (not (search ".include" text)))))))

(fiveam:test session-defaults-to-the-machines-retained-program
  (let ((m (make-machine 'emu-test-machine))
        (a (%dbg-assembly)))
    (load-program m a)
    (fiveam:is (eq a (debug-session-assembly (make-debug-session m))))))

(fiveam:test where-follows-a-relocated-program
  (let ((m (make-machine 'emu-test-machine))
        (a (%dbg-assembly)))
    (load-program m a :origin #x300)
    (let ((session (make-debug-session m)))
      (setf (sref m 'pc) #x302)
      (fiveam:is (search "2:.loop:  dex" (debug-where-text session))))))

(fiveam:test where-names-the-nearest-label-and-offset
  (let ((m (make-machine 'emu-test-machine))
        (a (%dbg-assembly)))
    (load-program m a)
    (let ((session (make-debug-session m)))
      (setf (sref m 'pc) #x103)
      (fiveam:is (search "pc = 0103 <count.loop+1>" (debug-where-text session))))))

(fiveam:test debug-break-condition-rejects-a-string-literal
  (let ((session (%dbg-session)))
    (fiveam:signals usage-error (debug-break session #x102 :condition "x == \"a\""))))

;;; Qualified-name index

(fiveam:test debug-session-indexes-qualified-names-once
  (let ((session (%dbg-session)))
    (fiveam:is (null (debug-session-qualified-names session)))
    (let ((info (%session-symbol session "count.loop" nil)))
      (fiveam:is (= #x102 (symbol-info-value info)))
      (fiveam:is (eq (debug-session-qualified-names session) (debug-session-qualified-names session)))
      (fiveam:is (eq info (%session-symbol session "count.loop" nil))))
    (fiveam:is (null (%session-symbol session "count.nowhere" nil)))))

;;; Dirty-page tracking

(defun %dirty-pages (machine array)
  "The sorted dirty page numbers MACHINE's tracker holds for ARRAY."
  (sort (loop for (a . page) in (dirty-pages-queue (machine-dirty machine))
              when (eq a array) collect page)
        #'<))

(fiveam:test dirty-pages-record-only-cell-writes
  (let* ((m (make-machine 'bank-test-machine))
         (ram (gethash 'ram (machine-slots m)))
         (bank (svref (cdr (gethash 'bram (machine-banks m))) 1)))
    (setf (machine-dirty m) (make-dirty-pages))
    (setf (mref m 'ram 3) 1)
    (setf (mref m 'ram 4) 1)
    (%poke m 'ram 130 1)
    (fiveam:is (equal '(0 2) (%dirty-pages m ram)))
    (ignore-errors (setf (mref m 'ram 32) 1))    ; :rom bank
    (setf (mref m 'ram 48) 1)                    ; device
    (fiveam:is (equal '(0 2) (%dirty-pages m ram)))
    (setf (bank-peek m 'bram 1 20) 5)
    (fiveam:is (equal '(0) (%dirty-pages m bank)))
    (setf (current-bank m 'bram) 1)
    (setf (mref m 'ram 21) 6)
    (fiveam:is (= 1 (count bank (dirty-pages-queue (machine-dirty m)) :key #'car)))
    (fiveam:is (not (dirty-pages-all (machine-dirty m))))))

(fiveam:test dirty-pages-bulk-writes-mark-everything
  (let ((m (make-machine 'emu-test-machine)))
    (setf (machine-dirty m) (make-dirty-pages))
    (reset m)
    (fiveam:is (dirty-pages-all (machine-dirty m)))
    (setf (dirty-pages-all (machine-dirty m)) nil)
    (restore-snapshot m (machine-snapshot m))
    (fiveam:is (dirty-pages-all (machine-dirty m)))))

(fiveam:test dirty-pages-survive-reset
  (let ((m (make-machine 'emu-test-machine))
        (dirty (make-dirty-pages)))
    (setf (machine-dirty m) dirty)
    (reset m)
    (fiveam:is (eq dirty (machine-dirty m)))))

;;; A loop that dirties three memory pages every iteration

(defparameter +dbg-loop-source+
  "        ldx #40
loop:   sta $10
        sta $150
        sta $2a0
        dex
        bne loop
        hlt")

(defun %dbg-loop-machine ()
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (assemble +dbg-loop-source+ :machine 'emu-test-machine :origin #x100))
    m))

(defun %dbg-loop-session (&key (history 1000) machine)
  (let ((m (or machine (%dbg-loop-machine))))
    (make-debug-session m :assembly (machine-program m) :history history)))

(defun %dbg-loop-trace ()
  "For a plain run of the loop program to its trap, a vector of (PC X SNAPSHOT)
after 0, 1, ... steps."
  (let ((session (%dbg-loop-session :history nil)))
    (flet ((state () (list (%pc session) (sref (debug-session-machine session) 'x)
                           (%dbg-snapshot session))))
      (let ((states '()))
        (loop (cl:push (state) states)
              (when (member (debug-step session 1) '(:trap :decode-failure))
                (cl:push (state) states)
                (return)))
        (coerce (nreverse states) 'vector)))))

(defun %dbg-run-to-trap (session)
  (loop for reason = (debug-continue session)
        until (member reason '(:trap :decode-failure))))

(fiveam:test debug-step-back-matches-every-recorded-state
  (let ((trace (%dbg-loop-trace)))
    (dolist (interval '(8 5 1))
      (let ((*debug-checkpoint-interval* interval)
            (*debug-anchor-interval* 3)
            (session (%dbg-loop-session)))
        (%dbg-run-to-trap session)
        (let ((end (debug-session-step-count session)))
          (fiveam:is (= end (1- (length trace))))
          (loop for target in (list (- end 1) (- end 2) (- end 9) (- end 30) (- end 31) (- end 100) 3 0)
                do (debug-step-back session (- (debug-session-step-count session) target))
                   (fiveam:is (equal (third (aref trace target)) (%dbg-snapshot session))
                              "interval ~D target ~D" interval target)))))))

(fiveam:test debug-step-back-catches-writes-that-skip-the-access-hook
  (let ((*debug-checkpoint-interval* 4)
        (*debug-anchor-interval* 50)
        (session (%dbg-loop-session)))
    (let ((m (debug-session-machine session)))
      (debug-step session 6)
      (debug-set session #x300 7)
      (debug-write session #x310 8)
      (%poke m 'ram #x320 9)
      (let ((edited (%dbg-snapshot session)))
        (debug-step session 3)
        (debug-step session 3)
        (debug-step-back session 6)
        (fiveam:is (equal edited (%dbg-snapshot session)))
        (fiveam:is (= 7 (mpeek m 'ram #x300)))))))

(fiveam:test debug-step-back-copes-with-a-second-history-session
  (let* ((m (%dbg-loop-machine))
         (first (%dbg-loop-session :machine m))
         (trace (%dbg-loop-trace))
         (*debug-checkpoint-interval* 4))
    (debug-step first 10)
    (%dbg-loop-session :machine m)
    (debug-step first 10)
    (debug-step-back first 15)
    (fiveam:is (equal (third (aref trace 5)) (%dbg-snapshot first)))
    (debug-step first 10)
    (debug-step-back first 3)
    (fiveam:is (equal (third (aref trace 12)) (%dbg-snapshot first)))))

;;; Reverse continue over a long history

(defun %dbg-reverse-trail (session next-hit)
  "Run reverse-continue by NEXT-HIT (a function of SESSION) until it stops
at the history start, as a list of (REASON STEP)."
  (loop for (reason) = (multiple-value-list (funcall next-hit session))
        collect (list reason (debug-session-step-count session))
        until (eq reason :history-start)))

(defun %dbg-expected-trail (trace now hit-p reason)
  "The trail a reverse run from step NOW should give, the steps below it where
HIT-P holds the first time, ending at step 0."
  (let ((steps (loop for k from (1- now) downto 0
                     when (funcall hit-p (aref trace k) k) collect k)))
    (append (loop for k in steps collect (list reason k))
            (list (list :history-start 0)))))

(defun %dbg-trail-with-states (session trace)
  "Like %DBG-REVERSE-TRAIL over DEBUG-REVERSE-CONTINUE, also checking each stop's state."
  (loop for (reason) = (multiple-value-list (debug-reverse-continue session))
        collect (list reason (debug-session-step-count session))
        do (fiveam:is (equal (third (aref trace (debug-session-step-count session)))
                             (%dbg-snapshot session)))
        until (eq reason :history-start)))

(fiveam:test debug-reverse-continue-agrees-with-the-trace
  (let ((trace (%dbg-loop-trace)))
    (dolist (interval '(8 5))
      (let ((*debug-checkpoint-interval* interval)
            (*debug-anchor-interval* 3))
        (dolist (when-set '(:before :after :mixed))
          (let ((session (%dbg-loop-session)))
            (when (member when-set '(:before :mixed)) (debug-break session #x10b))
            (when (eq when-set :mixed) (debug-step session 60))
            (%dbg-run-to-trap session)
            (unless (eq when-set :before) (debug-break session "loop"))
            (let ((addresses (if (eq when-set :before) '(#x10b) '(#x10b #x102))))
              (when (eq when-set :after) (debug-unbreak session #x10b))
              (when (eq when-set :after) (setf addresses '(#x102)))
              (fiveam:is (equal (%dbg-expected-trail
                                 trace (debug-session-step-count session)
                                 (lambda (entry k) (declare (ignore k)) (member (first entry) addresses))
                                 :breakpoint)
                                (%dbg-trail-with-states session trace))
                         "interval ~D ~S" interval when-set))))))))

(fiveam:test debug-reverse-continue-agrees-with-the-trace-for-conditions
  (let ((trace (%dbg-loop-trace))
        (*debug-checkpoint-interval* 8)
        (*debug-anchor-interval* 3))
    (dolist (when-set '(:before :after))
      (let ((session (%dbg-loop-session)))
        (when (eq when-set :before) (debug-break session "loop" :condition "x % 3 == 0"))
        (%dbg-run-to-trap session)
        (when (eq when-set :after) (debug-break session "loop" :condition "x % 3 == 0"))
        (fiveam:is (equal (%dbg-expected-trail
                           trace (debug-session-step-count session)
                           (lambda (entry k) (declare (ignore k))
                             (and (= (first entry) #x102) (zerop (mod (second entry) 3))))
                           :breakpoint)
                          (%dbg-trail-with-states session trace))
                   "~S" when-set)))))

(fiveam:test debug-reverse-continue-agrees-with-the-trace-for-watchpoints
  (let ((trace (%dbg-loop-trace))
        (*debug-checkpoint-interval* 8)
        (*debug-anchor-interval* 3))
    (dolist (when-set '(:before :after))
      (let ((session (%dbg-loop-session)))
        (when (eq when-set :before) (debug-watch session #x10))
        (%dbg-run-to-trap session)
        (when (eq when-set :after) (debug-watch session #x10))
        (fiveam:is (equal (%dbg-expected-trail
                           trace (debug-session-step-count session)
                           (lambda (entry k) (declare (ignore entry))
                             (and (plusp k) (= (first (aref trace (1- k))) #x102)))
                           :watchpoint)
                          (%dbg-trail-with-states session trace))
                   "~S" when-set)))))

(fiveam:test debug-reverse-until-agrees-with-the-trace
  (let ((trace (%dbg-loop-trace))
        (*debug-checkpoint-interval* 8)
        (*debug-anchor-interval* 3)
        (session (%dbg-loop-session)))
    (%dbg-run-to-trap session)
    (fiveam:is (equal (%dbg-expected-trail
                       trace (debug-session-step-count session)
                       (lambda (entry k) (declare (ignore k)) (= (first entry) #x10b))
                       :until)
                      (%dbg-reverse-trail session (lambda (s) (debug-reverse-continue-to s #x10b)))))))

(fiveam:test debug-reverse-continue-reports-a-failing-condition-either-way
  (let ((*debug-checkpoint-interval* 8)
        (results '()))
    (dolist (when-set '(:before :after))
      (let ((session (%dbg-loop-session)))
        (when (eq when-set :before) (debug-break session "loop" :condition "1 / (x - 30) == 0"))
        (%dbg-run-to-trap session)
        (when (eq when-set :after) (debug-break session "loop" :condition "1 / (x - 30) == 0"))
        (cl:push (loop repeat 6
                       collect (multiple-value-bind (reason undone condition) (debug-reverse-continue session)
                                 (list reason undone (and condition t))))
                 results)))
    (fiveam:is (equal (first results) (second results)))))

;;; Reverse continue does not replay the whole history

(defmacro %counting-replayed-steps ((var) &body body)
  `(let ((,var 0))
     (%call-wrapped '%replay
                    (lambda (function session checkpoint target &rest args)
                      (incf ,var (- target (checkpoint-step checkpoint)))
                      (apply function session checkpoint target args))
                    (lambda () ,@body))))

(fiveam:test debug-reverse-continue-jumps-to-a-recorded-hit
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-loop-session)))
    (debug-break session #x100)
    (%dbg-run-to-trap session)
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :breakpoint (debug-reverse-continue session)))
      (fiveam:is (= 0 (debug-session-step-count session)))
      (fiveam:is (< replayed 8)))))

(fiveam:test debug-reverse-continue-skips-segments-that-never-reached-the-stop
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-loop-session)))
    (%dbg-run-to-trap session)
    (debug-break session #x100)
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :breakpoint (debug-reverse-continue session)))
      (fiveam:is (< replayed 8)))
    (debug-step session 200)
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :until (debug-reverse-continue-to session #x100)))
      (fiveam:is (< replayed 8)))))

(defparameter +dbg-phase-source+
  "        ldx #40
loop:   sta $10
        dex
        bne loop
        sta $2a0
        hlt")

(defun %dbg-phase-session ()
  (let ((m (make-machine 'emu-test-machine)))
    (load-program m (assemble +dbg-phase-source+ :machine 'emu-test-machine :origin #x100))
    (make-debug-session m :assembly (machine-program m) :history 1000)))

(fiveam:test debug-reverse-continue-skips-segments-that-never-wrote-a-watched-page
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-phase-session)))
    (%dbg-run-to-trap session)
    (debug-watch session #x2a0)
    (%counting-replayed-steps (replayed)
      (multiple-value-bind (reason undone) (debug-reverse-continue session)
        (fiveam:is (eq :watchpoint reason))
        (fiveam:is (= 1 undone)))
      (fiveam:is (< replayed 20)))
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :history-start (debug-reverse-continue session)))
      (fiveam:is (< replayed 20)))))

(fiveam:test debug-reverse-continue-to-skips-segments-unless-they-wrote-a-watched-page
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-phase-session)))
    (%dbg-run-to-trap session)
    (debug-watch session #x10)
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :watchpoint (debug-reverse-continue-to session #x300)))
      (fiveam:is (< replayed 20)))))

(fiveam:test debug-reverse-continue-finds-a-write-after-a-step-back
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-phase-session)))
    (debug-step session 60)
    (debug-step-back session 2)
    (%dbg-run-to-trap session)
    (debug-watch session #x2a0)
    (multiple-value-bind (reason undone) (debug-reverse-continue session)
      (fiveam:is (eq :watchpoint reason))
      (fiveam:is (= 1 undone)))))

(fiveam:test debug-reverse-continue-with-a-later-watchpoint-matches-an-earlier-one
  (let ((*debug-checkpoint-interval* 8)
        (trails '()))
    (dolist (watch '((#x150 :write) (#x150 :read) (#x2a0 :write) (#x300 :write) ("x" :write) ("x" :read) ("z" :read-write)))
      (let ((results '()))
        (dolist (when-set '(:before :after))
          (let ((session (%dbg-loop-session)))
            (when (eq when-set :before) (apply #'debug-watch session (first watch) (list :access (second watch))))
            (%dbg-run-to-trap session)
            (debug-step-back session 60)
            (when (eq when-set :after) (apply #'debug-watch session (first watch) (list :access (second watch))))
            (cl:push (%dbg-reverse-trail session #'debug-reverse-continue) results)))
        (cl:push (equal (first results) (second results)) trails)))
    (fiveam:is (every #'identity trails))))

(fiveam:test debug-reverse-continue-skips-segments-that-never-touched-a-watched-register
  (let ((*debug-checkpoint-interval* 8)
        (m (make-machine 'emu-test-machine)))
    (load-program m (assemble "        ldx #1
loop:   sta $10
        bne loop" :machine 'emu-test-machine :origin #x100))
    (let ((session (make-debug-session m :assembly (machine-program m) :history 1000)))
      (debug-step session 100)
      (debug-watch session "x" :access :write)
      (%counting-replayed-steps (replayed)
        (multiple-value-bind (reason undone) (debug-reverse-continue session)
          (fiveam:is (eq :watchpoint reason))
          (fiveam:is (= 99 undone)))
        (fiveam:is (< replayed 20))))))

(fiveam:test debug-reverse-continue-skips-segments-that-never-read-a-watched-page
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-phase-session)))
    (%dbg-run-to-trap session)
    (debug-watch session #x2a0 :access :read)
    (%counting-replayed-steps (replayed)
      (fiveam:is (eq :history-start (debug-reverse-continue session)))
      (fiveam:is (< replayed 20)))))

(defun %dbg-count-condition-evaluations (history)
  (let ((session (%dbg-loop-session :history history))
        (evaluations 0))
    (debug-break session "loop" :condition "x == 3")
    (%call-wrapped '%breakpoint-triggered-p
                   (lambda (function &rest args)
                     (incf evaluations)
                     (apply function args))
                   (lambda () (debug-continue session)))
    evaluations))

(fiveam:test debug-continue-evaluates-a-breakpoint-condition-once-per-step
  (let ((plain (%dbg-count-condition-evaluations nil)))
    (fiveam:is (plusp plain))
    (fiveam:is (= plain (%dbg-count-condition-evaluations 1000)))))

(fiveam:test debug-continue-reports-a-failing-condition-with-history
  (let ((session (%dbg-loop-session)))
    (debug-break session "loop" :condition "1 / (x - 39) == 5")
    (multiple-value-bind (reason steps condition) (debug-continue session)
      (declare (ignore steps))
      (fiveam:is (eq :breakpoint reason))
      (fiveam:is (typep condition 'error)))))

(fiveam:test debug-recorded-hits-follow-breakpoint-and-timeline-changes
  (let ((*debug-checkpoint-interval* 8)
        (session (%dbg-loop-session)))
    (debug-break session #x10b)
    (debug-step session 30)
    (fiveam:is (debug-session-hits session))
    (debug-break session #x100)
    (fiveam:is (null (debug-session-hits session)))
    (fiveam:is (null (debug-session-hits-from session)))
    (debug-step session 10)
    (fiveam:is (= 31 (debug-session-hits-from session)))
    (debug-step-back session 5)
    (fiveam:is (every (lambda (hit) (<= (car hit) 35)) (debug-session-hits session)))
    (debug-step-back session 20)
    (fiveam:is (= 15 (debug-session-step-count session)))
    (fiveam:is (null (debug-session-hits-from session)))
    (debug-set session #x300 1)
    (fiveam:is (null (debug-session-hits session)))))

(fiveam:test debug-recorded-hits-keep-a-condition-error-off-the-session
  (let ((session (%dbg-loop-session)))
    (debug-break session "loop" :condition "1 / (x - 30) == 0")
    (debug-step session 51)
    (fiveam:is (null (debug-session-condition-error session)))
    (fiveam:is (some (lambda (hit) (typep (third hit) 'error)) (debug-session-hits session)))))

;;; save / load commands

(fiveam:test debug-command-save-and-load-round-trip-state
  (uiop:with-temporary-file (:pathname path :type "snap")
    (let ((session (%dbg-session))
          (file (namestring path)))
      (debug-step session 2)
      (let ((pc (%pc session)))
        (fiveam:is (search "saved" (debug-command session (format nil "save ~A" file))))
        (debug-continue session)
        (fiveam:is (/= pc (%pc session)))
        (fiveam:is (search "loaded" (debug-command session (format nil "load ~A" file))))
        (fiveam:is (= pc (%pc session)))
        (fiveam:is (eq :trap (debug-continue session)))))))

(fiveam:test debug-command-load-reports-bad-files-as-text
  (let ((session (%dbg-session)))
    (fiveam:is (search "Error:" (debug-command session "load /nonexistent/lasm.snap")))
    (uiop:with-temporary-file (:pathname path :type "snap")
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-string "garbage (" out))
      (fiveam:is (search "Error:" (debug-command session (format nil "load ~A" (namestring path))))))
    (fiveam:is (search "missing path" (debug-command session "save")))
    (fiveam:is (search "missing path" (debug-command session "load")))))

(fiveam:test debug-load-keeps-step-back-working
  (uiop:with-temporary-file (:pathname path :type "snap")
    (let* ((a (%dbg-assembly))
           (m (make-machine 'emu-test-machine))
           (file (namestring path)))
      (load-program m a)
      (let ((session (make-debug-session m :assembly a :history 100)))
        (debug-step session 3)
        (debug-command session (format nil "save ~A" file))
        (debug-step session 2)
        (debug-command session (format nil "load ~A" file))
        (let ((pc (%pc session)))
          (debug-step session 2)
          (debug-step-back session 2)
          (fiveam:is (= pc (%pc session))))))))

;;; Snapshot commands

(defun %dbg-file-session ()
  (let* ((a (%assemble-include-fixture "nested.asm"))
         (m (make-machine 'instr-test-machine)))
    (load-program m a)
    (make-debug-session m :assembly a)))

(fiveam:test debug-save-writes-sexp-by-default-and-binary-on-request
  (uiop:with-temporary-file (:pathname path :type "snap")
    (let ((session (%dbg-file-session))
          (name (namestring path)))
      (fiveam:is (search "saved" (debug-command session (format nil "save ~A" name))))
      (fiveam:is (not (%binary-snapshot-file-p path)))
      (fiveam:is (search "saved" (debug-command session (format nil "save ~A binary" name))))
      (fiveam:is (%binary-snapshot-file-p path))
      (fiveam:is (getf (cdr (read-snapshot path)) :program))
      (fiveam:is (search "loaded" (debug-command session (format nil "load ~A" name)))))))

(fiveam:test debug-save-binary-needs-a-path
  (let ((session (%dbg-file-session)))
    (fiveam:is (search "missing path" (debug-command session "save")))
    (multiple-value-bind (path format) (%split-save-arguments "binary")
      (fiveam:is (equal '("binary" :sexp) (list path format))))
    (multiple-value-bind (path format) (%split-save-arguments "a b.snap  BINARY")
      (fiveam:is (equal '("a b.snap" :binary) (list path format))))))
