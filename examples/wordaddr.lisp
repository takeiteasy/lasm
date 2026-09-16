;;;; examples/wordaddr.lisp
;;;;
;;;; #53 (M4): word-addressed memory as an alternative to byte-addressed --
;;;; a machine whose sole memory element declares :CELL-WIDTH 16, so its
;;;; assembled output is a vector of 16-bit cells rather than 8-bit bytes,
;;;; and each address names one 16-bit cell rather than one byte. Unlike
;;;; examples/word.lisp's WORDFOO (#20's bitfield/variant instruction-word
;;;; encoding, still byte-addressed underneath), this machine uses the
;;;; ordinary opcode-plus-operand-cells encoding every earlier example
;;;; already uses -- word addressing and bitfield instruction words are two
;;;; independent axes, and this example deliberately isolates the first one.
;;;;
;;;; This is intentionally small and not DCPU-16-shaped -- see
;;;; examples/dcpu16.lisp for a real DCPU-16-shaped machine combining
;;;; this word-addressed memory with #20's variant encoding.
;;;;
;;;; Run with:  sbcl --script examples/wordaddr.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: two general registers, PC, and one word-addressed memory
;;; element -- ADDR-WIDTH 12 (4096 cells) each 16 bits wide, so RAM holds
;;; 4096 16-bit words rather than 4096 bytes.

(defmachine wordaddrfoo
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 16 :addr-width 12 :cell-width 16))

(definstruction wordaddrfoo lda
  (modes immediate)
  (encoding (opcode 1) (operand :mode))
  (semantics (set! a operand)))

(definstruction wordaddrfoo ldb
  (modes immediate)
  (encoding (opcode 2) (operand :mode))
  (semantics (set! b operand)))

(definstruction wordaddrfoo add
  (encoding (opcode 3))
  (semantics (set! a (wrap-value (+ a b) 16))))

;; STA's operand is an ordinary ABSOLUTE-mode address -- %DEFAULT-ADDRESS-
;; WIDTH (instruction.lisp) rounds RAM's 12-bit address space up to a whole
;; number of RAM's own 16-bit cells (1), not a whole number of 8-bit bytes
;; (which would wrongly say 2) -- the discriminating case #53 fixes.
(definstruction wordaddrfoo sta
  (modes absolute)
  (encoding (opcode 4) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction wordaddrfoo hlt
  (encoding (opcode 5))
  (semantics (trap :halt)))

(defparameter *source*
  "lda #10
ldb #20
add          ; A = A + B = 30
sta result
hlt
result: .word 0")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'wordaddrfoo)))
  (format t "  cells:      ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  cell-width: ~D bits~%" (assembly-cell-width assembly))
  (format t "  length:     ~D cells (2 each for LDA/LDB/STA, 1 each for ADD/HLT, ~
2 for RESULT's .word -- .WORD always means 2 of this machine's own cells, #53)~%"
          (length (assembly-cells assembly)))
  (assert (= 16 (assembly-cell-width assembly)))
  (assert (= 10 (length (assembly-cells assembly))))
  (assert (equal '(unsigned-byte 16) (array-element-type (assembly-cells assembly))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'wordaddrfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = ~D (expected 30)~%" (sref m 'a))
      (format t "  RAM[result] = ~D (expected 30)~%"
              (mref m 'ram (gethash "result" (assembly-symbols assembly))))
      (assert (eq :trap reason))
      (assert (= 5 steps))
      (assert (= 30 (sref m 'a)))
      (assert (= 30 (mref m 'ram (gethash "result" (assembly-symbols assembly)))))
      (format t "~%All assertions passed.~%"))))
