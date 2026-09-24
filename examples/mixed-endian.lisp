;;;; examples/mixed-endian.lisp
;;;;
;;;; #145: a PDP-endian machine -- :ENDIAN (:BIG :LITTLE 2) lays a value's
;;;; two-cell groups down high group first, each group low cell first, so
;;;; $0A0B0C0D is stored 0B 0A 0D 0C. .BEWORD overrides that for one directive.
;;;;
;;;; Run with:  sbcl --script examples/mixed-endian.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine pdpfoo
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16 :endian (:big :little 2)))

(defdirective ".long" (&rest values) (emit 4 values))
(defdirective ".beword" (&rest values) (emit 2 values :endian :big))

(definstruction pdpfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(let ((assembly (assemble ".long $0A0B0C0D
.word $1234
.beword $1234
hlt" :machine 'pdpfoo)))
  (format t "cells: ~S~%" (coerce (assembly-cells assembly) 'list))
  (assert (equal '(#x0B #x0A #x0D #x0C  #x34 #x12  #x12 #x34  #x00)
                 (coerce (assembly-cells assembly) 'list)))
  (format t "All assertions passed.~%"))
