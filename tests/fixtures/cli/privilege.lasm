(defmachine privilege-cli-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags s)
  (privilege :level s :levels (user supervisor) :on-violation :trap))

(definstruction privilege-cli-machine rte
  (privilege supervisor)
  (encoding (opcode #x01))
  (semantics nil))
