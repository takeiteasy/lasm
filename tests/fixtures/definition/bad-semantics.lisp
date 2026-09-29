(in-package #:lasm)

(definstruction instr-test-machine compile-file-bad-semantics
  (encoding (opcode #x7E))
  (semantics (interrupt-return)))
