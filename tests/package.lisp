;;;; tests/package.lisp
;;;; Regression test for #:lasm's export list: every test file here is
;;;; (in-package #:lasm), so an internal symbol resolves identically to an
;;;; external one and a dropped export is otherwise invisible to the suite.

(in-package #:lasm)

(fiveam:def-suite package :in lasm)
(fiveam:in-suite package)

;; #106: REGREF and REGISTER-INDEX-OUT-OF-RANGE were documented as public
;; API (docs/machine-model.md) but missing from the export list;
;; MACHINE-PEEK-READER (docs/disassembler.md) and OPCODE-CONFLICT
;; (docs/diagnostics.md, docs/instructions.md) had the same gap.
(fiveam:test documented-api-symbols-are-external
  (dolist (name '("REGREF" "REGISTER-INDEX-OUT-OF-RANGE"
                  "MACHINE-PEEK-READER" "OPCODE-CONFLICT"))
    (fiveam:is (eq :external (nth-value 1 (find-symbol name '#:lasm)))
               "~A is not external in #:lasm" name)))

;; #170: a public condition type's slot readers are public too -- these 11
;; leaked internal despite their condition types being exported, forcing a
;; consumer to reach into lasm:: to read a caught condition's own slots.
(fiveam:test condition-readers-are-external
  (dolist (name '("STORAGE-ERROR-MACHINE" "STORAGE-ERROR-NAME"
                  "ADDRESS-OUT-OF-RANGE-ADDRESS"
                  "REGISTER-INDEX-OUT-OF-RANGE-INDEX"
                  "LASM-TRAP-TAG" "LASM-TRAP-DATA"
                  "OPCODE-CONFLICT-MACHINE" "OPCODE-CONFLICT-OPCODE"
                  "OPCODE-CONFLICT-MNEMONIC" "OPCODE-CONFLICT-OTHER-MNEMONIC"
                  "OPCODE-CONFLICT-REASON"))
    (fiveam:is (eq :external (nth-value 1 (find-symbol name '#:lasm)))
               "~A is not external in #:lasm" name)))
