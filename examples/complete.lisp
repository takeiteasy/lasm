;;;; examples/complete.lisp
;;;;
;;;; The M2 milestone's newer directives, folded into one 6502-shaped
;;;; program alongside the multi-mode addressing examples/modes.lisp already
;;;; covers: `.equ` named constants (#35, docs/directives.md#equ),
;;;; `.macro`/`.endm` expansion (#33, docs/macros.md), and a forced
;;;; addressing-mode suffix (#40, docs/modes.md#forcing-a-mode-with-a-
;;;; mnemonic-suffix) that overrides LDA/STA's normal zero-page-vs-absolute
;;;; choice. Each feature has its own narrower example elsewhere
;;;; (examples/directives.lisp, examples/macros.lisp) -- this one shows them
;;;; working together in a single assembly.
;;;;
;;;; Run with:  sbcl --script examples/complete.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

(defmachine sixtyfoo-complete
  (register a :width 8)
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

;; LDA/STA: ZERO-PAGE and ABSOLUTE share bare-expr syntax, disambiguated by
;; operand value unless a mnemonic suffix (.z/.w, built into those two modes
;; -- mode.lisp) forces one explicitly.
(definstruction sixtyfoo-complete lda
  (modes
    (immediate (opcode #xA9) (semantics (set! a operand)))
    (zero-page (opcode #xA5))
    (absolute  (opcode #xAD)))
  (semantics (set! a (mref machine 'ram operand))))

(definstruction sixtyfoo-complete sta
  (modes
    (zero-page (opcode #x85))
    (absolute  (opcode #x8D)))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction sixtyfoo-complete adc
  (modes immediate)
  (encoding (opcode #x69) (operand :mode))
  (semantics
    (let ((r (+ a c operand)))
      (set! a (wrap-value r 8))
      (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction sixtyfoo-complete ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo-complete dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo-complete bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction sixtyfoo-complete hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "        .equ step, 5        ; a named constant, no address of its own (#35)
        .equ base, $10      ; likewise -- BASE is a zero-page address

.macro addto dst, k         ; dst = *dst + k, expanded inline at each call
    lda dst
    adc #k
    sta dst
.endm

start:  lda #10             ; A = 10 -- IMMEDIATE
        sta base            ; RAM[$10] = 10 -- ZERO-PAGE, chosen by value
        addto base, step    ; RAM[$10] = 10 + 5 -- macro expansion (#33)
        sta.w base          ; RAM[$10] = 15 again, but forced ABSOLUTE this
                             ; time (#40) even though ZERO-PAGE still fits
        lda.z base          ; A = RAM[$10] = 15, forced ZERO-PAGE explicitly
        ldx #3               ; loop 3 times
loop:   dex
        bne loop
        hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'sixtyfoo-complete)))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-cells assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo-complete)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = ~D, X = ~D, RAM[$10] = ~D~%"
              (sref m 'a) (sref m 'x) (mref m 'ram #x10)))))
