(defmachine undefined-opcode-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (undefined-opcode :trap))

(definstruction undefined-opcode-test-machine nop
  (encoding (opcode #x01))
  (semantics nil))
