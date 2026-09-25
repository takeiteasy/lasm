;;;; examples/cycles.lisp
;;;;
;;;; #75: cycle-cost model, clock speed, and cycle-accurate execution. The
;;;; same counter-loop program as examples/counter.lisp, but SIXTYFOO2
;;;; declares a (clock-speed n) and each instruction its own (cycles n) --
;;;; run-for-cycles, run-for-duration, and machine-elapsed-seconds then give
;;;; three different ways to bound/measure the same run. See
;;;; docs/emulator.md ("Cycle-cost model, clock speed, and cycle-accurate
;;;; execution") and docs/instructions.md ("(cycles n)").
;;;;
;;;; Run with:  sbcl --script examples/cycles.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(deflexer sixtyfoo2-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))

(defparameter *source*
  "count:  ldx #10        ; x = 10
.loop:  dex             ; x -= 1
        bne .loop       ; loop while x != 0
        sta $1000
        hlt             ; stop the emulator loop (see docs/emulator.md)")

;; A 1 MHz fantasy CPU -- 1 cycle = 1 microsecond -- with roughly 6502-shaped
;; per-instruction costs (the real 6502's LDX #imm/DEX/BNE/STA abs/HLT-ish
;; BRK costs, for flavor; this ISA is otherwise SIXTYFOO from
;; examples/counter.lisp).
(defmachine sixtyfoo2
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z)
  (clock-speed 1000000))

(definstruction sixtyfoo2 ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x))))
  (cycles 2))

(definstruction sixtyfoo2 dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x))))
  (cycles 2))

(definstruction sixtyfoo2 bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand))))
  (cycles 3)) ; branch-taken/page-cross penalties are a follow-up (#75's own)

(definstruction sixtyfoo2 sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x))
  (cycles 4))

(definstruction sixtyfoo2 hlt
  (encoding (opcode #x00))
  (semantics (trap :halt))
  (cycles 7))

(let ((assembly (assemble *source* :lexer 'sixtyfoo2-syntax :machine 'sixtyfoo2)))

  (format t "~&== run: no budget, just report cycles afterward ==~%")
  (let ((m (make-machine 'sixtyfoo2)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P, ~D cycles (~,1F us at 1 MHz)~%"
              reason steps (machine-cycles m) (* 1.0d6 (machine-elapsed-seconds m)))))

  (format t "~%== run-for-cycles: stop partway through, budget 10 cycles ==~%")
  (let ((m (make-machine 'sixtyfoo2)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run-for-cycles m 10)
      ;; :max-cycles may overshoot 10 by at most one instruction's own cost
      ;; (docs/emulator.md) -- budget checks happen after each step executes,
      ;; since a step's cost isn't known until it has already run.
      (format t "  stopped: ~A after ~D step~:P, ~D cycles consumed (budget was 10)~%"
              reason steps (machine-cycles m))))

  (format t "~%== run-for-duration: stop after 15 simulated microseconds ==~%")
  (let ((m (make-machine 'sixtyfoo2)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run-for-duration m 15.0d-6)
      (format t "  stopped: ~A after ~D step~:P, ~,1F us simulated~%"
              reason steps (* 1.0d6 (machine-elapsed-seconds m))))))

;;; #164: an idle step costs (idle :cycles n), default 1. SLEEPY is a machine
;;; whose sleep instruction parks the CPU with a 4-cycle idle step.

(defmachine sleepy
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8)
  (idle :cycles 4))

(definstruction sleepy slp (encoding (opcode #x01)) (semantics (idle)) (cycles 1))

(let ((m (make-machine 'sleepy)))
  (load-program m (list #x01) :origin 0)
  (step-machine m)                    ; slp itself: 1 cycle
  (multiple-value-bind (result cost) (step-machine m)
    (format t "~%Idle step: ~S costs ~D cycles (total ~D)~%" result cost (machine-cycles m))
    (assert (eq :idle result))
    (assert (= 4 cost))
    (assert (= 5 (machine-cycles m)))))
