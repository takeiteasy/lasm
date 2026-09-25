;;;; examples/word-endian.lisp
;;;;
;;;; #216: cell order inside a word-encoded instruction. Memory is
;;;; little-endian 8-bit cells, but this machine stores its 16-bit
;;;; instruction word big-endian (:ENDIAN on INSTRUCTION-WORD), and one extra
;;;; word little-endian again (:ENDIAN on the EXTRA-WORD variant). Data
;;;; directives keep following the memory's order.
;;;;
;;;; Run with:  sbcl --script examples/word-endian.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine wendfoo
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16 :endian :big
    (field opcode 4)
    (field src 12)))

(defmode wendimm "#" expr)

(definstruction wendfoo seta
  (modes wendimm)
  (encoding (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #xfff :cells 2 :endian :little))))
  (semantics (set! a value)))

(let* ((assembly (assemble "seta #5
seta #$1234
.word $1234" :machine 'wendfoo))
       (cells (coerce (assembly-cells assembly) 'list)))
  (format t "cells: ~S~%" cells)
  (assert (equal '(#x10 #x06  #x1f #xff #x34 #x12  #x34 #x12) cells))
  (multiple-value-bind (descriptor values size)
      (decode-instruction-at (lambda (address) (nth address cells)) 2 'wendfoo)
    (assert (string-equal "SETA" (instruction-descriptor-name descriptor)))
    (assert (equal '(#x1234) values))
    (assert (= 4 size)))
  (format t "All assertions passed.~%"))
