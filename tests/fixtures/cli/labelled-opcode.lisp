(defmachine labelled-opcode-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(definstruction labelled-opcode-test-machine nop
  (encoding (opcode #x01))
  (semantics nil))
