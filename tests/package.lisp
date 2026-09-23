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
                  "STACK-POINTER" "STACK-POINTER-OUT-OF-RANGE"
                  "MACHINE-PEEK-READER" "OPCODE-CONFLICT"))
    (fiveam:is (eq :external (nth-value 1 (find-symbol name '#:lasm)))
               "~A is not external in #:lasm" name)))

;; #171: a public condition type's slot readers are public too (#106, #170
;; found this by hand, twice). Walk every external condition's slots via the
;; MOP and assert each reader/writer is external, instead of auditing by eye.
;; CLASS-SLOTS' effective slots have no applicable SLOT-DEFINITION-READERS
;; method on SBCL, so this walks direct slots up the precedence list; the
;; package filter drops inherited CL readers like SIMPLE-CONDITION-FORMAT-
;; CONTROL that would otherwise show up as false positives.
(fiveam:test condition-readers-are-external
  (let ((pkg (find-package '#:lasm))
        (condition-class (find-class 'condition))
        (types '())
        (reader-count 0))
    (do-external-symbols (name pkg)
      (let ((class (find-class name nil)))
        (when (and class (subtypep class condition-class))
          (cl:push name types))))
    ;; A walk that finds nothing passes trivially -- guard against that
    ;; instead of only checking for leaks.
    (fiveam:is (>= (length types) 20)
               "expected at least 20 external condition types, found ~D"
               (length types))
    (dolist (type types)
      (dolist (super (closer-mop:class-precedence-list (find-class type)))
        (dolist (slot (closer-mop:class-direct-slots super))
          (dolist (accessor (append (closer-mop:slot-definition-readers slot)
                                     (closer-mop:slot-definition-writers slot)))
            (let ((name (if (consp accessor) (second accessor) accessor)))
              (when (eq (symbol-package name) pkg)
                (incf reader-count)
                (fiveam:is (eq :external (nth-value 1 (find-symbol (symbol-name name) pkg)))
                           "~A is not external in #:lasm (reachable from ~A via ~A)"
                           name type super)))))))
    (fiveam:is (>= reader-count 60)
               "expected at least 60 reader/writer occurrences, found ~D"
               reader-count)))
