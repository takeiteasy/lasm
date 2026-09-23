;;;; Run with: sbcl --script examples/macros.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine sixtyfoo-macros
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

(definstruction sixtyfoo-macros lda
  (modes (immediate (opcode #xA9) (semantics (set! a operand)))
         (absolute  (opcode #xAD) (semantics (set! a (mref machine 'ram operand))))))

(definstruction sixtyfoo-macros adc
  (modes immediate)
  (encoding (opcode #x69) (operand :mode))
  (semantics (let ((r (+ a c operand)))
               (set! a (wrap-value r 8))
               (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction sixtyfoo-macros sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction sixtyfoo-macros hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  ".macro addconst dst, k=1
    .equ .amount, k
    lda dst
    adc #.amount
    sta dst
.endm

first:  addconst cell1, 5    ; cell1 = 10 + 5
        addconst cell1       ; cell1 = 15 + 1
second: addconst cell2, 3    ; cell2 = 20 + 3
        hlt
cell1:  .byte 10
cell2:  .byte 20")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'sixtyfoo-macros)))
  (format t "  origin: $~4,'0X~%" (assembly-origin assembly))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-cells assembly) 'list))
  (format t "~%Expansion locations:~%")
  (dolist (entry (assembly-listing assembly))
    (when (listing-line-definition-line entry)
      (format t "  $~4,'0X: call line ~D, body line ~D~%"
              (listing-line-address entry)
              (listing-line-line entry)
              (listing-line-definition-line entry))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo-macros)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  RAM[cell1] = ~D~%" (mref m 'ram (gethash "cell1" (assembly-symbols assembly))))
      (format t "  RAM[cell2] = ~D~%" (mref m 'ram (gethash "cell2" (assembly-symbols assembly)))))))
