(defmachine banked-test-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region romx #x4000 #x40FF :kind :rom :banks 4)))

(definstruction banked-test-machine nop
  (encoding (opcode #x00))
  (semantics (set! pc pc)))

(definstruction banked-test-machine hlt
  (encoding (opcode #x01))
  (semantics (trap :halt)))
