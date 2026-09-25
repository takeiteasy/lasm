(defmachine nibble-test-machine
  (register pc :width 8)
  (memory ram :width 4 :addr-width 8 :cell-width 4 :endian :big))

(definstruction nibble-test-machine nop
  (encoding (opcode 1))
  (semantics nil))

(definstruction nibble-test-machine hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))
