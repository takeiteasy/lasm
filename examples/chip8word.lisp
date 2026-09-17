;;;; examples/chip8word.lisp
;;;;
;;;; #64 (M4): per-instruction, non-uniform instruction-word layouts. Real
;;;; CHIP8 opcodes are themselves non-uniform nibble layouts -- 1NNN (JP) is
;;;; 4/12, 6XNN (LD Vx,byte) is 4/4/8, DXYN (DRW) is 4/4/4/4 -- which a
;;;; single machine-level (instruction-word ...) field layout (#20) cannot
;;;; express in one machine. CHIP8WORDFOO declares two named alternates
;;;; alongside the default layout, sharing the word's own :width and OPCODE
;;;; field:
;;;;
;;;;   (instruction-word :width 16
;;;;     (field opcode 4) (field x 4) (field y 4) (field n 4)   ; default: 4/4/4/4
;;;;     (layout xnn (field opcode 4) (field x 4) (field nn 8)) ; 4/4/8
;;;;     (layout nnn (field opcode 4) (field nnn 12)))          ; 4/12
;;;;
;;;; and each DEFINSTRUCTION names which one it encodes against via its own
;;;; (layout NAME) encoding subclause.
;;;;
;;;; See examples/chip8.lisp (#54) for the *other* half of CHIP8-shaped M4
;;;; coverage -- non-uniform *register* widths (banked 8-bit V, 12-bit I).
;;;; That example deliberately sidesteps instruction-word entirely; this one
;;;; is the encoding side it named as a separate ticket.
;;;;
;;;; Scope: layout selection only, covering CHIP8's layout-only opcode
;;;; families -- JP/CALL (1NNN/2NNN), LD I,addr (ANNN), LD/ADD/SE Vx,byte
;;;; (6XNN/7XNN/3XNN), DRW (DXYN). Two departures from real CHIP8, both
;;;; because the remaining opcode families (00E0/00EE/8XY_/5XY0/9XY0/EX__/
;;;; FX__) need a field pinned to a literal value with no operand hole --
;;;; a mechanism this ticket does not add (see its follow-up on the
;;;; tracker):
;;;;
;;;; - HLT stands in for 0NNN/00E0/00EE at opcode 0 (the only descriptor
;;;;   there, so it decodes unambiguously).
;;;; - RET (needed to make CALL round-trip) has no real CHIP8 opcode of its
;;;;   own here -- it sits at the otherwise-unused opcode 9.
;;;; - DRW does not draw -- its semantics are a stand-in RAM write, since
;;;;   this example has no display to draw to. Its job is to exercise the
;;;;   default 4/4/4/4 (three-hole) layout.
;;;;
;;;; Run with:  sbcl --script examples/chip8word.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: PC/I at 16 bits (headroom, not CHIP8's real 12 -- this example's
;;; point is the word layouts, not the address space; see chip8.lisp for a
;;; 12-bit PC/I), a banked 8-bit V register (16 elements), byte-addressed
;;; RAM, and a call stack for CALL/RET.

(defmachine chip8wordfoo
  (register pc :width 16)
  (register v :width 8 :count 16)
  (register i :width 16)
  (memory ram :width 8 :addr-width 16)
  (stack cs :width 16 :depth 16)
  (instruction-word :width 16
    (field opcode 4) (field x 4) (field y 4) (field n 4)
    (layout xnn (field opcode 4) (field x 4) (field nn 8))
    (layout nnn (field opcode 4) (field nnn 12))))

;; WNNN: a bare 12-bit address, for JP/CALL/LD I. WXIMM: "V 1, #5" -- a V
;; register index and an immediate byte, for LD/ADD/SE. WXYN: "V 0, V 1, 3"
;; -- two V register indices and a nibble, for DRW.
(defmode wnnn expr)
(defmode wximm "V" expr "," "#" expr)
(defmode wxyn "V" expr "," "V" expr "," expr)

;; HLT -- opcode 0, no operand, default layout (irrelevant with no fields).
(definstruction chip8wordfoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

;; JP nnn -- 1NNN, the 4/12 layout.
(definstruction chip8wordfoo jp
  (modes wnnn)
  (encoding (opcode 1) (layout nnn)
    (operand addr :field nnn))
  (semantics (set! pc addr)))

;; CALL nnn -- 2NNN, the 4/12 layout again. Pushes the return address (PC
;; has already advanced past this instruction by the time semantics run,
;; #16) onto CS.
(definstruction chip8wordfoo call
  (modes wnnn)
  (encoding (opcode 2) (layout nnn)
    (operand addr :field nnn))
  (semantics (push pc cs) (set! pc addr)))

;; SE Vx, #nn -- 3XNN, the 4/4/8 layout: skip the next instruction (2 bytes,
;; this machine's own fixed instruction size) if V[x] equals the immediate.
(definstruction chip8wordfoo se
  (modes wximm)
  (encoding (opcode 3) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (when (= (v x) nn) (set! pc (+ pc 2)))))

;; RET -- no real CHIP8 opcode here (needs 00EE's constant fields, out of
;; scope); a stand-in at the otherwise-unused opcode 9.
(definstruction chip8wordfoo ret
  (encoding (opcode 9))
  (semantics (set! pc (pop cs))))

;; LD Vx, #nn -- 6XNN, the 4/4/8 layout.
(definstruction chip8wordfoo ld
  (modes wximm)
  (encoding (opcode 6) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (set! (v x) nn)))

;; ADD Vx, #nn -- 7XNN, the 4/4/8 layout, wrapping at V's own 8-bit width.
(definstruction chip8wordfoo add
  (modes wximm)
  (encoding (opcode 7) (layout xnn)
    (operand x :field x)
    (operand nn :field nn))
  (semantics (set! (v x) (wrap-value (+ (v x) nn) 8))))

;; LD I, nnn -- ANNN, the 4/12 layout again.
(definstruction chip8wordfoo ldi
  (modes wnnn)
  (encoding (opcode 10) (layout nnn)
    (operand addr :field nnn))
  (semantics (set! i addr)))

;; DRW Vx, Vy, n -- DXYN, the *default* 4/4/4/4 layout (no (layout ...)
;; subclause needed). Does not draw -- writes V[x] to RAM[I] as a stand-in,
;; so the instruction still does something observable. Y is unused, kept
;; only to exercise a genuine three-hole word mode.
(definstruction chip8wordfoo drw
  (modes wxyn)
  (encoding (opcode #xd)
    (operand x :field x)
    (operand y :field y)
    (operand n :field n))
  (semantics (declare (ignore y n)) (setf (mref machine 'ram i) (v x))))

;; Exercises all three layouts: LD/ADD/SE on XNN, JP/CALL/LDI on NNN, DRW on
;; the default XYN. DOUBLE doubles V0 via ADD then RETs; the main body
;; loads I, calls DOUBLE, draws (the stand-in RAM write), then uses SE to
;; skip a marker LD that would prove SE failed if it ran, before JPing to
;; HLT.
(defparameter *source*
  "  ld    V 0, #21     ; V0 = 21
  ldi   $100        ; I = 256
  call  double      ; V0 = 42
  drw   V 0, V 1, 3 ; stand-in: RAM[I] = V0
  se    V 0, #42    ; V0 = 42, so skip the next instruction
  ld    V 0, #99    ; unreached if SE works -- would prove otherwise
  jp    done
double:
  add   V 0, #21     ; V0 += 21
  ret
done:
  hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'chip8wordfoo)))
  (format t "  bytes:  ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  length: ~D bytes~%" (length (assembly-cells assembly)))
  (assert (= 20 (length (assembly-cells assembly))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'chip8wordfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  V0 = ~D (expected 42, doubled via CALL/RET's ADD)~%" (regref m 'v 0))
      (format t "  I  = ~D (expected 256)~%" (sref m 'i))
      (format t "  RAM[256] = ~D (expected 42, DRW's stand-in write)~%" (mref m 'ram 256))
      (format t "  stack depth = ~D (expected 0 -- CALL/RET balanced)~%" (stack-depth m 'cs))
      (assert (eq :trap reason))
      (assert (= 42 (regref m 'v 0)))
      (assert (= 256 (sref m 'i)))
      (assert (= 42 (mref m 'ram 256)))
      (assert (zerop (stack-depth m 'cs)))
      (format t "~%All assertions passed.~%"))))
