(defmachine cli-ambi
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode cli-ambi-a expr :width 1)
(defmode cli-ambi-b expr :width 1)

(definstruction cli-ambi ambi
  (modes
    (cli-ambi-a (opcode #x01) (semantics (set! a operand)))
    (cli-ambi-b (opcode #x02) (semantics (set! a operand)))))
