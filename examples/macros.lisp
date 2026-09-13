;;;; examples/macros.lisp
;;;;
;;;; .macro/.endm expansion (macro.lisp, #33): a two-parameter macro adds a
;;;; constant to a memory location, expanded twice against two different
;;;; call sites before layout ever sees the statement list. Each call site
;;;; carries its own global label, per docs/macros.md's no-hygiene caveat --
;;;; a macro body's local labels are scoped to whichever global label
;;;; precedes the call, so two invocations under the *same* global label
;;;; would collide.
;;;;
;;;; Run with:  sbcl --script examples/macros.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

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
  ".macro addconst dst, k     ; dst = *dst + k
    lda dst
    adc #k
    sta dst
.endm

first:  addconst cell1, 5    ; cell1 = 10 + 5
second: addconst cell2, 3    ; cell2 = 20 + 3
        hlt
cell1:  .byte 10
cell2:  .byte 20")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'sixtyfoo-macros)))
  (format t "  origin: $~4,'0X~%" (assembly-origin assembly))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-bytes assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo-macros)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  RAM[cell1] = ~D~%" (mref m 'ram (gethash "cell1" (assembly-symbols assembly))))
      (format t "  RAM[cell2] = ~D~%" (mref m 'ram (gethash "cell2" (assembly-symbols assembly)))))))
