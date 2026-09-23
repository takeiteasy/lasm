;;;; Run with: sbcl --script examples/hole-attributes.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))
(in-package #:lasm)

(defmachine hole-demo
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode signed-byte "#" (expr :signed t) :width 1)
(defmode two-branches (expr :relative t) "," (expr :relative t) :width 1)

(definstruction hole-demo addi
  (modes signed-byte)
  (encoding (opcode #x01) (operand :mode))
  (semantics (set! a (wrap-value (+ a operand) 8))))

(definstruction hole-demo choose
  (modes two-branches)
  (encoding (opcode #x02) (operand when-zero :mode) (operand otherwise :mode))
  (semantics (set! pc (+ pc (if (zerop a) when-zero otherwise)))))

(definstruction hole-demo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(let* ((source "addi #-1
choose zero, nonzero
zero: hlt
nonzero: hlt")
       (assembly (assemble source :machine 'hole-demo))
       (machine (make-machine 'hole-demo)))
  (load-program machine assembly)
  (format t "Cells: ~S~%" (coerce (assembly-cells assembly) 'list))
  (multiple-value-bind (reason steps) (run machine)
    (format t "Stopped: ~A after ~D steps; A = ~D~%" reason steps (sref machine 'a))))
