;;;; examples/chip8.lisp
;;;;
;;;; #54 (M4): a CHIP8-shaped machine -- LASM-plan.md sec. 3.7's mockup --
;;;; validating the M4 target's first half: per-register :width that
;;;; differs across elements, with a *banked* register in the mix (V0-VF,
;;;; #13's indexed regref/(setf regref) and its (v idx) semantics form).
;;;; CHIP8FOO has an 8-bit banked V register (16 elements) and a 12-bit I
;;;; register sharing one machine, so an instruction moving a value from one
;;;; into the other has to show the width difference: V wraps at 8 bits, I
;;;; at 12.
;;;;
;;;; This deliberately uses the ordinary opcode-plus-operand-cells encoding
;;;; (M1-M3), not #20's (instruction-word ...) clause -- #54's own
;;;; deliverable is non-uniform *register* widths, not instruction
;;;; encoding. Real CHIP8 opcodes are themselves non-uniform nibble layouts
;;;; (6XNN is 4/4/8, 1NNN is 4/12), which a single machine-level
;;;; instruction-word field layout cannot express; that is a separate,
;;;; further ticket (per-instruction word layouts).
;;;;
;;;; Run with:  sbcl --script examples/chip8.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

;;; Machine: PC (convention), a banked 8-bit V register (16 elements,
;;; V0-VF), a scalar 12-bit I (index) register, and byte-addressed RAM
;;; sized to CHIP8's 12-bit address space.

(defmachine chip8foo
  (register pc :width 12)
  (register v :width 8 :count 16)
  (register i :width 12)
  (memory ram :width 8 :addr-width 12))

;; V-IMM: "V 0, #10" -- a V-register index and an immediate byte, for
;; LDV/ADDV. V-ONLY: "V 1" -- just a V-register index, for ADDI.
(defmode v-imm "V" expr "," "#" expr)
(defmode v-only "V" expr)

;; LDV Vx, #nn -- load an immediate byte into bank X of V.
(definstruction chip8foo ldv
  (modes v-imm)
  (encoding (opcode 1) (operand x :width 1) (operand nn :width 1))
  (semantics (set! (v x) nn)))

;; ADDV Vx, #nn -- add an immediate byte into bank X of V, wrapping at V's
;; own 8-bit width.
(definstruction chip8foo addv
  (modes v-imm)
  (encoding (opcode 2) (operand x :width 1) (operand nn :width 1))
  (semantics (set! (v x) (wrap-value (+ (v x) nn) 8))))

;; LDI #nnn -- load a 12-bit immediate into I. :WIDTH 2 overrides
;; IMMEDIATE's default (1 cell, only 8 bits) since I's range needs two.
(definstruction chip8foo ldi
  (modes immediate)
  (encoding (opcode 3) (operand :width 2))
  (semantics (set! i operand)))

;; ADDI Vx -- I += V[x], wrapping at I's own 12-bit width, not V's 8-bit
;; one. The discriminating case for this example: an 8-bit source added
;; into a 12-bit destination.
(definstruction chip8foo addi
  (modes v-only)
  (encoding (opcode 4) (operand x :width 1))
  (semantics (set! i (wrap-value (+ i (v x)) 12))))

;; JP addr -- unconditional jump. ABSOLUTE's default width (no explicit
;; :mode override needed) is %DEFAULT-ADDRESS-WIDTH's ceiling(12/8) = 2
;; bytes, not RAM's own 12-bit address width.
(definstruction chip8foo jp
  (modes absolute)
  (encoding (opcode 5) (operand :mode))
  (semantics (set! pc operand)))

(definstruction chip8foo hlt
  (encoding (opcode 6))
  (semantics (trap :halt)))

;; V0/V1 prove banked cells are independent; ADDV V0 wraps at 8 bits; LDI/
;; ADDI push I past 255 and then past 4095, proving I's own 12-bit wrap;
;; JP skips a marker write to V2 that would prove JP failed if it ran.
(defparameter *source*
  "ldv V 0, #$fa    ; V0 = 250
ldv V 1, #5      ; V1 = 5
addv V 0, #10    ; V0 = 260 -> wraps mod 256 = 4
ldi #$ffe        ; I = 4094 (needs 12 bits -- V's own width can't hold it)
addi V 1         ; I = 4094 + V1(5) = 4099 -> wraps mod 4096 = 3
jp skip
ldv V 2, #99     ; unreached if JP works -- would prove otherwise
skip: hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'chip8foo)))
  (format t "  bytes:  ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  length: ~D bytes~%" (length (assembly-cells assembly)))
  (assert (= 21 (length (assembly-cells assembly))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'chip8foo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  V0 = ~D (expected 4, wraps at 8 bits)~%" (regref m 'v 0))
      (format t "  V1 = ~D (expected 5)~%" (regref m 'v 1))
      (format t "  V2 = ~D (expected 0 -- JP skipped this write)~%" (regref m 'v 2))
      (format t "  I  = ~D (expected 3, wraps at 12 bits, not V's 8)~%" (sref m 'i))
      (assert (eq :trap reason))
      (assert (= 7 steps))
      (assert (= 4 (regref m 'v 0)))
      (assert (= 5 (regref m 'v 1)))
      (assert (= 0 (regref m 'v 2)))
      (assert (= 3 (sref m 'i)))
      (format t "~%All assertions passed.~%"))))
