;;;; examples/cli/twelve.lisp
;;;; A 12-bit-cell machine for trying non-byte cell packing (see
;;;; docs/binary-output.md):
;;;;
;;;;   lasm assemble twelve.asm -m twelve.lisp --packing bits

(deflexer twelve-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (string-delim "\"")
  (ident-chars :alnum "_."))

(defmachine twelve
  (register acc :width 12)
  (register pc :width 12)
  (memory ram :width 12 :addr-width 12 :cell-width 12 :endian :big)
  (flags z))

(definstruction twelve lda
  (modes immediate)
  (encoding (opcode #xA01) (operand :mode))
  (semantics (set! acc operand) (set-flags! (z (zero? acc)))))

(definstruction twelve sta
  (modes absolute)
  (encoding (opcode #xB02) (operand :mode))
  (semantics (setf (mref machine 'ram operand) acc)))

(definstruction twelve hlt
  (encoding (opcode #x000))
  (semantics (trap :halt)))
