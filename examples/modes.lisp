;;;; examples/modes.lisp
;;;;
;;;; The M2 milestone target (LASM-plan.md sec. 2): "something 6502-shaped --
;;;; enough addressing modes ... to assemble a small non-trivial program."
;;;; Unlike examples/counter.lisp (M1: exactly one mode per instruction),
;;;; LDA and ADC below each declare several modes (docs/modes.md,
;;;; docs/instructions.md) and the assembler (docs/assembler.md) picks which
;;;; one a given operand actually uses -- by syntax, then by whether the
;;;; value fits.
;;;;
;;;; Run with:  sbcl --script examples/modes.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

(defmachine sixtyfoo-m2
  (register a :width 8)
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

(definstruction sixtyfoo-m2 ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

;; LDA: three modes sharing one mnemonic, each with its own opcode and (for
;; IMMEDIATE) its own semantics -- ZERO-PAGE and ABSOLUTE share the shared
;; default below since both address RAM the same way, differing only in
;; operand width, which the assembler picks based on the operand's value
;; (docs/assembler.md).
(definstruction sixtyfoo-m2 lda
  (modes
    (immediate (opcode #xA9) (semantics (set! a operand)))
    (zero-page (opcode #xA5))
    (absolute  (opcode #xAD))
    ;; INDEXED-X ("expr , X") is unambiguous against the other three's bare
    ;; `expr` syntax, so it needs no value-based disambiguation.
    (indexed-x (opcode #xBD) (semantics (set! a (mref machine 'ram (wrap-value (+ operand x) 16))))))
  (semantics (set! a (mref machine 'ram operand))))

;; %ADC is not part of the semantics vocabulary (semantics.lisp) -- it is
;; just an ordinary function called from within a WITH-MACHINE-BINDINGS body,
;; taking the already-evaluated MACHINE and OPERAND value ADC's semantics
;; are bound to. This keeps the three ADC variants' shared arithmetic in one
;; place instead of repeating it per mode.
(defun %adc (machine value)
  (with-machine-bindings (machine sixtyfoo-m2)
    (let ((r (+ a value c)))
      (set! a (wrap-value r 8))
      (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction sixtyfoo-m2 adc
  (modes
    (immediate (opcode #x69) (semantics (%adc machine operand)))
    (zero-page (opcode #x65))
    (absolute  (opcode #x6D)))
  (semantics (%adc machine (mref machine 'ram operand))))

(definstruction sixtyfoo-m2 sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction sixtyfoo-m2 hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "        lda #10        ; A = 10 -- IMMEDIATE (#xA9)
        sta $10        ; RAM[$0010] = 10
        ldx #1         ; X = 1, for the indexed-X line below
        lda $10        ; A = RAM[$0010] = 10 -- ZERO-PAGE (#xA5): same
                       ; syntax as ABSOLUTE below, chosen because $10 fits
                       ; a one-byte operand
        adc #5         ; A += 5 -- IMMEDIATE ADC (#x69)
        sta $2000      ; RAM[$2000] = A -- ABSOLUTE (two-byte operand)
        lda $2000      ; A = RAM[$2000] -- ABSOLUTE (#xAD): same syntax as
                       ; the zero-page LDA above, chosen because $2000 does
                       ; not fit one byte
        lda $f,X       ; A = RAM[$0F + X] = RAM[$10] -- INDEXED-X (#xBD)
        hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'sixtyfoo-m2)))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-bytes assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo-m2)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = ~D, RAM[$0010] = ~D, RAM[$2000] = ~D~%"
              (sref m 'a) (mref m 'ram #x10) (mref m 'ram #x2000)))))
