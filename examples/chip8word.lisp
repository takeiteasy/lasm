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
;;;; #136 adds the other half a nibble-faithful CHIP8 needs: a
;;;; (field-value FIELD-NAME n) encoding subclause pinning a field to a
;;;; literal with no operand hole at all, to discriminate opcode families
;;;; that share their OPCODE field but otherwise carry no operand
;;;; distinguishing them -- CHIP8's 8XY0-8XYE (ALU ops, discriminated by N),
;;;; 5XY0/9XY0 (SE/SNE Vx,Vy, N pinned to 0), EX9E/EXA1 (SKP/SKNP Vx, NN
;;;; pinned), FX__ (misc ops on Vx, NN pinned), and 00E0/00EE (CLS/RET,
;;;; every field below OPCODE pinned) all need it. Before #136 these could
;;;; not be expressed at all: (opcode n :sub s) is byte-machine-only (#125),
;;;; and a word-encoded operand field always came from a mode's own EXPR
;;;; hole.
;;;;
;;;; See examples/chip8.lisp (#54) for the *other* half of CHIP8-shaped M4
;;;; coverage -- non-uniform *register* widths (banked 8-bit V, 12-bit I).
;;;; That example deliberately sidesteps instruction-word entirely; this one
;;;; is the encoding side it named as a separate ticket.
;;;;
;;;; Scope: nibble-faithful for every opcode family below, with one
;;;; deliberate gap -- 0NNN (SYS addr) cannot coexist with 00E0/00EE here.
;;;; A catch-all NNN hole at opcode 0 would overlap both pinned values under
;;;; any layout choice, which %HOLE-DISJOINT-P (instruction.lisp) rejects as
;;;; :INDISTINGUISHABLE; telling them apart needs priority/ordering
;;;; semantics this ticket does not add (tracked separately, #139).
;;;; HLT stands in for the one address 0NNN this example does use (0x000),
;;;; the same role SYS addr's "ignored by modern interpreters" already
;;;; plays in real CHIP8 -- repurposed here as an explicit, deterministic
;;;; stop for the demo program, since CHIP8 itself has no halt opcode.
;;;; SKP/SKNP (no keyboard on this machine) are stubbed deterministically:
;;;; SKP never skips (no key is ever "pressed"), SKNP always does.
;;;;
;;;; Run with:  sbcl --script examples/chip8word.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: PC/I at 16 bits (headroom, not CHIP8's real 12 -- this example's
;;; point is the word layouts, not the address space; see chip8.lisp for a
;;; 12-bit PC/I), a banked 8-bit V register (16 elements, V[15] doubling as
;;; the VF flag register exactly as real CHIP8 uses it), byte-addressed RAM,
;;; and a call stack for CALL/RET.

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
;; register index and an immediate byte. WXY: "V 0, V 1" -- two V register
;; indices, for the 8XY_/5XY0/9XY0 register-register families. WX: "V 0" --
;; one V register index, for the EX__/FX__ families. WXYN: "V 0, V 1, 3" --
;; two V register indices and a nibble, for DRW.
(defmode wnnn expr)
(defmode wximm "V" expr "," "#" expr)
(defmode wxy "V" expr "," "V" expr)
(defmode wx "V" expr)
(defmode wxyn "V" expr "," "V" expr "," expr)

;;; Opcode 0 (NNN layout): SYS/CLS/RET's family. Every field below OPCODE is
;;; pinned via (field-value ...) -- no operand at all, the exact shape #136
;;; adds. CLS (00E0) is semantically a no-op here (no display); RET (00EE)
;;; is CHIP8's real return-from-subroutine; HLT stands in for 0x000 (see
;;; header).

(definstruction chip8wordfoo cls
  (encoding (opcode 0) (layout nnn) (field-value nnn #xe0))
  (semantics nil))

(definstruction chip8wordfoo ret
  (encoding (opcode 0) (layout nnn) (field-value nnn #xee))
  (semantics (set! pc (pop cs))))

(definstruction chip8wordfoo hlt
  (encoding (opcode 0) (layout nnn) (field-value nnn 0))
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

;;; SE/ADD/LD each merge their immediate (3XNN/7XNN/6XNN) and register
;;; (5XY0/8XY4/8XY0) forms under one mnemonic, real CHIP8's own convention --
;;; one (modes ...) clause dispatching on which addressing mode matched, each
;;; variant its own opcode, layout, and semantics.

;; SE Vx, #nn (3XNN, XNN layout) / SE Vx, Vy (5XY0, default layout, N
;; pinned 0) -- skip the next instruction (2 bytes, this machine's own fixed
;; instruction size) if the comparison holds.
(definstruction chip8wordfoo se
  (modes (wximm (opcode 3) (layout xnn)
           (operand x :field x)
           (operand nn :field nn)
           (semantics (when (= (v x) nn) (set! pc (+ pc 2)))))
         (wxy (opcode 5) (field-value n 0)
           (operand x :field x)
           (operand y :field y)
           (semantics (when (= (v x) (v y)) (set! pc (+ pc 2)))))))

;; ADD Vx, #nn (7XNN, XNN layout, no flag) / ADD Vx, Vy (8XY4, default
;; layout, N pinned 4, VF set to the carry) -- both wrap at V's 8-bit width.
(definstruction chip8wordfoo add
  (modes (wximm (opcode 7) (layout xnn)
           (operand x :field x)
           (operand nn :field nn)
           (semantics (set! (v x) (wrap-value (+ (v x) nn) 8))))
         (wxy (opcode 8) (field-value n 4)
           (operand x :field x)
           (operand y :field y)
           (semantics (let ((sum (+ (v x) (v y))))
                        (set! (v 15) (if (> sum 255) 1 0))
                        (set! (v x) (wrap-value sum 8)))))))

;; LD Vx, #nn (6XNN, XNN layout) / LD Vx, Vy (8XY0, default layout, N
;; pinned 0).
(definstruction chip8wordfoo ld
  (modes (wximm (opcode 6) (layout xnn)
           (operand x :field x)
           (operand nn :field nn)
           (semantics (set! (v x) nn)))
         (wxy (opcode 8) (field-value n 0)
           (operand x :field x)
           (operand y :field y)
           (semantics (set! (v x) (v y))))))

;;; The rest of the 8XY_ ALU family -- default layout, N pinned per
;;; operation, sharing opcode 8 with LD/ADD above via #136's constant
;;; discriminator. VF is V[15], exactly as real CHIP8 uses it.

(definstruction chip8wordfoo or
  (modes wxy)
  (encoding (opcode 8) (field-value n 1)
    (operand x :field x)
    (operand y :field y))
  (semantics (set! (v x) (logior (v x) (v y)))))

(definstruction chip8wordfoo and
  (modes wxy)
  (encoding (opcode 8) (field-value n 2)
    (operand x :field x)
    (operand y :field y))
  (semantics (set! (v x) (logand (v x) (v y)))))

(definstruction chip8wordfoo xor
  (modes wxy)
  (encoding (opcode 8) (field-value n 3)
    (operand x :field x)
    (operand y :field y))
  (semantics (set! (v x) (logxor (v x) (v y)))))

;; SUB Vx, Vy -- 8XY5: Vx -= Vy, VF = 1 when there's no borrow (Vx >= Vy).
(definstruction chip8wordfoo sub
  (modes wxy)
  (encoding (opcode 8) (field-value n 5)
    (operand x :field x)
    (operand y :field y))
  (semantics (let ((vx (v x)) (vy (v y)))
               (set! (v 15) (if (>= vx vy) 1 0))
               (set! (v x) (wrap-value (- vx vy) 8)))))

;; SHR Vx {, Vy} -- 8XY6: VF = Vx's LSB, Vx >>= 1. Y is unused (modern
;; interpreter behaviour), kept only because 8XY6's own encoding has the
;; hole.
(definstruction chip8wordfoo shr
  (modes wxy)
  (encoding (opcode 8) (field-value n 6)
    (operand x :field x)
    (operand y :field y))
  (semantics (declare (ignore y))
             (let ((vx (v x)))
               (set! (v 15) (logand vx 1))
               (set! (v x) (ash vx -1)))))

;; SUBN Vx, Vy -- 8XY7: Vx = Vy - Vx, VF = 1 when there's no borrow (Vy >= Vx).
(definstruction chip8wordfoo subn
  (modes wxy)
  (encoding (opcode 8) (field-value n 7)
    (operand x :field x)
    (operand y :field y))
  (semantics (let ((vx (v x)) (vy (v y)))
               (set! (v 15) (if (>= vy vx) 1 0))
               (set! (v x) (wrap-value (- vy vx) 8)))))

;; SHL Vx {, Vy} -- 8XYE: VF = Vx's MSB, Vx <<= 1 (wrapped to 8 bits). Y
;; unused, same rationale as SHR.
(definstruction chip8wordfoo shl
  (modes wxy)
  (encoding (opcode 8) (field-value n #xe)
    (operand x :field x)
    (operand y :field y))
  (semantics (declare (ignore y))
             (let ((vx (v x)))
               (set! (v 15) (ldb (byte 1 7) vx))
               (set! (v x) (wrap-value (ash vx 1) 8)))))

;; SNE Vx, Vy -- 9XY0, default layout, N pinned 0: skip the next instruction
;; if Vx and Vy differ.
(definstruction chip8wordfoo sne
  (modes wxy)
  (encoding (opcode 9) (field-value n 0)
    (operand x :field x)
    (operand y :field y))
  (semantics (when (/= (v x) (v y)) (set! pc (+ pc 2)))))

;; LD I, nnn -- ANNN, the 4/12 layout again.
(definstruction chip8wordfoo ldi
  (modes wnnn)
  (encoding (opcode 10) (layout nnn)
    (operand addr :field nnn))
  (semantics (set! i addr)))

;;; SKP/SKNP Vx -- EX9E/EXA1, XNN layout: X is the only hole, NN pinned to
;;; the sub-opcode. This machine has no keyboard, so both are stubbed
;;; deterministically rather than dropped: SKP never skips (no key is ever
;;; "pressed"), SKNP always does (every key always reads "not pressed").
(definstruction chip8wordfoo skp
  (modes wx)
  (encoding (opcode #xe) (layout xnn) (field-value nn #x9e)
    (operand x :field x))
  (semantics (declare (ignore x))))

(definstruction chip8wordfoo sknp
  (modes wx)
  (encoding (opcode #xe) (layout xnn) (field-value nn #xa1)
    (operand x :field x))
  (semantics (declare (ignore x)) (set! pc (+ pc 2))))

;;; FX__ -- XNN layout, X is the only hole, NN pinned per sub-operation.
;;; Scoped to the subset expressible with this machine's own storage: no
;;; delay/sound timers, keyboard, or font table, so FX07/FX0A/FX15/FX18/
;;; FX29 are out of scope here.

;; ADD I, Vx -- FX1E.
(definstruction chip8wordfoo addi
  (modes wx)
  (encoding (opcode #xf) (layout xnn) (field-value nn #x1e)
    (operand x :field x))
  (semantics (set! i (wrap-value (+ i (v x)) 16))))

;; LD B, Vx -- FX33: store Vx's three decimal digits at RAM[I], RAM[I+1],
;; RAM[I+2], most significant first.
(definstruction chip8wordfoo ldbcd
  (modes wx)
  (encoding (opcode #xf) (layout xnn) (field-value nn #x33)
    (operand x :field x))
  (semantics (let ((val (v x)))
               (setf (mref machine 'ram i) (truncate val 100))
               (setf (mref machine 'ram (+ i 1)) (mod (truncate val 10) 10))
               (setf (mref machine 'ram (+ i 2)) (mod val 10)))))

;; LD [I], Vx -- FX55: store V0..Vx into RAM starting at I.
(definstruction chip8wordfoo ldmem
  (modes wx)
  (encoding (opcode #xf) (layout xnn) (field-value nn #x55)
    (operand x :field x))
  (semantics (loop for k from 0 to x
                    do (setf (mref machine 'ram (+ i k)) (v k)))))

;; LD Vx, [I] -- FX65: load V0..Vx from RAM starting at I.
(definstruction chip8wordfoo ldreg
  (modes wx)
  (encoding (opcode #xf) (layout xnn) (field-value nn #x65)
    (operand x :field x))
  (semantics (loop for k from 0 to x
                    do (set! (v k) (mref machine 'ram (+ i k))))))

;; DRW Vx, Vy, n -- DXYN, the *default* 4/4/4/4 layout (no (layout ...)
;; subclause needed, and no field-value pin either -- every field here is a
;; genuine operand hole). Does not draw -- writes V[x] to RAM[I] as a
;; stand-in, so the instruction still does something observable. Y is
;; unused, kept only to exercise a genuine three-hole word mode.
(definstruction chip8wordfoo drw
  (modes wxyn)
  (encoding (opcode #xd)
    (operand x :field x)
    (operand y :field y)
    (operand n :field n))
  (semantics (declare (ignore y n)) (setf (mref machine 'ram i) (v x))))

;;; Exercises every family above: immediate and register forms of LD/ADD/SE,
;;; the rest of the 8XY_ ALU ops, SNE, CALL/RET (00EE), SKP/SKNP, the FX__
;;; memory ops, DRW's default layout, and CLS/HLT (00E0/0x000) to finish.
(defparameter *source*
  "  ld     V 0, #5      ; V0 = 5
  ld     V 1, #3      ; V1 = 3
  add    V 0, #2      ; V0 = 7 (immediate)
  add    V 0, V 1     ; V0 = 10, VF = 0 (8XY4)
  or     V 2, V 0     ; V2 = 0 | 10 = 10
  and    V 2, V 1     ; V2 = 10 & 3 = 2
  xor    V 2, V 1     ; V2 = 2 ^ 3 = 1
  sub    V 0, V 1     ; V0 = 10 - 3 = 7, VF = 1 (no borrow)
  shr    V 0, V 0     ; VF = LSB(7) = 1, V0 = 3
  shl    V 0, V 0     ; VF = MSB(3) = 0, V0 = 6
  subn   V 1, V 0     ; V1 = V0 - V1 = 6 - 3 = 3, VF = 1 (no borrow)
  sne    V 0, V 1     ; V0 = 6, V1 = 3: not equal -- skip the next instruction
  ld     V 3, #99     ; unreached if SNE works -- would prove otherwise
  ld     V 0, V 2     ; V0 = V2 = 1 (8XY0)
  se     V 0, #1      ; V0 == 1 -- skip the next instruction
  ld     V 3, #77     ; unreached if SE (immediate) works
  se     V 0, V 1     ; V0 = 1, V1 = 3: not equal -- no skip
  ld     V 4, #55     ; runs -- SE (register) correctly did not skip
  ldi    $200         ; I = 0x200 = 512
  addi   V 1          ; I = 512 + V1(3) = 515
  ldbcd  V 1          ; RAM[515..517] = 0, 0, 3 (BCD of 3) -- ldmem below overwrites this
  ld     V 5, #9
  ld     V 6, #8
  ld     V 7, #7
  ldmem  V 7          ; RAM[515..522] = V0..V7 = 1,3,1,0,55,9,8,7
  ld     V 0, #0
  ld     V 1, #0
  ld     V 7, #0
  ldreg  V 7          ; V0..V7 restored from RAM[515..522]
  call   double       ; V0 += 21
  drw    V 0, V 1, 3  ; stand-in: RAM[I] = V0, overwriting RAM[515] again
  skp    V 0          ; no key ever pressed -- never skips
  ld     V 9, #11     ; runs -- SKP correctly did not skip
  sknp   V 0          ; every key reads not-pressed -- always skips
  ld     V 9, #22     ; unreached if SKNP works
  jp     done
double:
  add    V 0, #21
  ret
done:
  cls                 ; 00E0 -- no-op here, no display
  hlt                 ; 0x000, this example's stand-in halt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'chip8wordfoo)))
  (format t "  bytes:  ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  length: ~D bytes~%" (length (assembly-cells assembly)))

  (format t "~%Running:~%")
  (let ((m (make-machine 'chip8wordfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  V0..V9 = ~S~%" (loop for k below 10 collect (regref m 'v k)))
      (format t "  I  = ~D (expected 515 -- LDI $200 then ADDI V1)~%" (sref m 'i))
      (format t "  RAM[515..522] = ~S (LDMEM's write, RAM[515] then overwritten again by DRW)~%"
              (loop for a from 515 to 522 collect (mref m 'ram a)))
      (format t "  stack depth = ~D (expected 0 -- CALL/RET balanced)~%" (stack-depth m 'cs))
      (assert (eq :trap reason))
      (assert (equal '(22 3 1 0 55 9 8 7 0 11) (loop for k below 10 collect (regref m 'v k))))
      (assert (= 515 (sref m 'i)))
      (assert (equal '(22 3 1 0 55 9 8 7) (loop for a from 515 to 522 collect (mref m 'ram a))))
      (assert (zerop (stack-depth m 'cs)))
      (format t "~%All assertions passed.~%"))))
