(defmachine storage-fault-test-machine
  (register pc :width 16)
  (stack ds :width 8 :depth 1)
  (memory ram :width 8 :addr-width 16))

(definstruction storage-fault-test-machine popempty
  (encoding (opcode #x01))
  (semantics (pop ds)))
