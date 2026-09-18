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

(fiveam:test debug-memory-text-hex-width-follows-cell-width
  ;; #87 regression guard -- a 16-bit-cell machine's dump must not truncate
  ;; to 2 hex digits.
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
