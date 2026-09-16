;;;; examples/hybrid.lisp
;;;;
;;;; The M3 milestone's second validation case: "A hybrid machine (e.g.
;;;; accumulator + index registers + implicit call stack)." HYBRIDFOO below
;;;; is a 6502-flavoured machine with an
;;;; accumulator, two index registers, and one stack doing double duty as
;;;; both a data stack and a call stack -- JSR/RTS push/pop the PC register
;;;; onto it exactly like any other value, no new semantics primitive
;;;; needed (see docs/semantics.md, docs/emulator.md). See
;;;; docs/machine-model.md and docs/modes.md for STACK-REF and the
;;;; STACK-RELATIVE addressing mode this example exercises.
;;;;
;;;; Two things worth calling out that this example pins down:
;;;;
;;;; - JSR's semantics are (push pc s) (set! pc operand). This only pushes
;;;;   the *correct* return address because STEP-MACHINE (emulator.lisp)
;;;;   advances PC past the whole instruction *before* running its
;;;;   semantics -- so PC already points at the instruction after JSR by
;;;;   the time (push pc s) runs, the same convention STACKFOO's JZ/JMP
;;;;   (examples/stack.lisp) rely on to override PC in the other direction.
;;;; - DOUBLE shares S with the caller: PHA pushes an argument, then JSR
;;;;   pushes a return address on top of it, so DOUBLE's argument sits one
;;;;   below the top of the stack it's running on -- exactly the case
;;;;   STACK-RELATIVE addressing (`1,S`) exists for. RTS pops only the
;;;;   return address (offset 0), leaving DOUBLE's result behind at what
;;;;   was offset 1 -- now the top -- for the caller's PLA to collect.
;;;;
;;;; Run with:  sbcl --script examples/hybrid.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: accumulator, two index registers, PC (convention), one stack
;;; used as both data stack and call stack, and RAM.

(defmachine hybridfoo
  (register a :width 8)
  (register x :width 8)
  (register y :width 8)
  (register pc :width 16)
  (stack s :width 16 :depth 64)
  (memory ram :width 8 :addr-width 16)
  (flags z))

;; LDA: three modes sharing one mnemonic (docs/instructions.md) -- IMMEDIATE
;; and STACK-RELATIVE each need their own semantics (an immediate operand is
;; a literal value; a stack-relative operand is an offset into S), while
;; ABSOLUTE falls back to the shared top-level semantics below.
(definstruction hybridfoo lda
  (modes
    (immediate      (opcode #xA9) (semantics (set! a operand)))
    (stack-relative (opcode #xA3) (semantics (set! a (stack-ref machine 's operand))))
    (absolute       (opcode #xAD)))
  (semantics (set! a (mref machine 'ram operand))))

(definstruction hybridfoo sta
  (modes
    (stack-relative (opcode #x83) (semantics (setf (stack-ref machine 's operand) a)))
    (absolute       (opcode #x8D)))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction hybridfoo sty
  (modes absolute)
  (encoding (opcode #x8C) (operand :mode))
  (semantics (setf (mref machine 'ram operand) y)))

(definstruction hybridfoo pha
  (encoding (opcode #x48))
  (semantics (push a s)))

(definstruction hybridfoo pla
  (encoding (opcode #x68))
  (semantics (set! a (pop s))))

(definstruction hybridfoo asl
  (encoding (opcode #x0A))
  (semantics (set! a (wrap-value (* a 2) 8))))

(definstruction hybridfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction hybridfoo dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

;; INY counts DOUBLE's invocations (see *SOURCE* below) -- it isn't needed
;; for JSR/RTS/STACK-RELATIVE to work, just a second use of Y beyond
;; declaring it, and a cheap way to assert the subroutine actually ran
;; three times rather than the loop merely appearing to.
(definstruction hybridfoo iny
  (encoding (opcode #xC8))
  (semantics (set! y (wrap-value (1+ y) 8))))

(definstruction hybridfoo bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

;; JSR/RTS: subroutine call/return built entirely from PUSH/POP of PC onto
;; S, per #52 -- no dedicated call-stack primitive.
(definstruction hybridfoo jsr
  (modes absolute)
  (encoding (opcode #x20) (operand :mode))
  (semantics (push pc s) (set! pc operand)))

(definstruction hybridfoo rts
  (encoding (opcode #x60))
  (semantics (set! pc (pop s))))

(definstruction hybridfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; Calls DOUBLE three times, each call doubling ram[$1000] in place:
;; 1 -> 2 -> 4 -> 8. Y counts the calls (final value 3, stashed at
;; ram[$1001]) and S is empty again at the end (stack-balanced).
(defparameter *source*
  "        ldx #3
        lda #1
        sta $1000
loop:   lda $1000       ; s: []
        pha              ; s: [arg]
        jsr double       ; s: [arg, retaddr] -> double
        pla              ; s: [] (double left its result at the old arg slot)
        sta $1000
        dex
        bne loop
        sty $1001
        hlt

double: iny
        lda 1,S          ; the argument, one below the return address at 0,S
        asl
        sta 1,S          ; result written back in place, ready for RTS+PLA
        rts")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'hybridfoo)))
  (format t "  bytes:   ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  symbols: ~{~A=$~4,'0X~^, ~}~%"
          (loop for k being the hash-keys of (assembly-symbols assembly)
                  using (hash-value v)
                collect k collect v))

  (format t "~%Running:~%")
  (let ((m (make-machine 'hybridfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  RAM[$1000] (doubled 3x) = ~D~%" (mref m 'ram #x1000))
      (format t "  RAM[$1001] (call count) = ~D~%" (mref m 'ram #x1001))
      (format t "  stack depth = ~D (0 => program is stack-balanced)~%"
              (stack-depth m 's)))))
