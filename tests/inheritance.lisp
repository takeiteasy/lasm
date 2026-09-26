;;;; tests/inheritance.lisp
;;;; fiveam tests for machine families: (defmachine (child (:extends parent)) ...).

(in-package #:lasm)

(fiveam:def-suite inheritance :in lasm)
(fiveam:in-suite inheritance)

;;; Byte-encoded family

(defmachine fam-base
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z)
  (properties :model "base" :rev 1))

(definstruction fam-base lda
  (modes immediate)
  (encoding (opcode #x10) (operand :mode))
  (semantics (set! a operand))
  (cycles 2))

(definstruction fam-base inc
  (encoding (opcode #x20))
  (semantics (set! a (wrap-value (1+ a) 8)))
  (cycles 2))

(definstruction fam-base hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defmachine (fam-lite (:extends fam-base))
  (without-instructions inc)
  (undefined-opcode :nop)
  (properties :model "lite"))

(defmachine (fam-turbo (:extends fam-base))
  (instruction-cycles (inc 1))
  (clock-speed 4000000)
  (flags c))

(defmachine (fam-lite-small (:extends fam-lite))
  (memory ram :addr-width 12)
  (undefined-opcode :trap))

(defun fam-run (machine-name bytes)
  (let ((m (make-machine machine-name)))
    (load-program m bytes)
    (multiple-value-bind (reason steps condition) (run m)
      (values m reason steps condition))))

(fiveam:test extends-records-parent-and-retargets-copies
  (fiveam:is (eq 'fam-base (machine-descriptor-parent (find-machine-descriptor 'fam-lite))))
  (fiveam:is (null (machine-descriptor-parent (find-machine-descriptor 'fam-base))))
  (dolist (mnemonic '("LDA" "INC" "HLT"))
    (fiveam:is (eq 'fam-base
                   (instruction-descriptor-machine (find-instruction 'fam-base mnemonic))))
    (fiveam:is (eq 'fam-turbo
                   (instruction-descriptor-machine (find-instruction 'fam-turbo mnemonic)))))
  (fiveam:is (not (eq (find-instruction 'fam-base "LDA") (find-instruction 'fam-turbo "LDA")))))

(fiveam:test child-runs-inherited-instructions
  (multiple-value-bind (m reason) (fam-run 'fam-turbo '(#x10 5 #x20 #x00))
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 6 (sref m 'a)))))

(fiveam:test instruction-cycles-override-inherited-cost
  (fiveam:is (= 5 (machine-cycles (fam-run 'fam-base '(#x10 5 #x20 #x00)))))
  (fiveam:is (= 4 (machine-cycles (fam-run 'fam-turbo '(#x10 5 #x20 #x00)))))
  (fiveam:is (= 2 (instruction-descriptor-cycles (find-instruction 'fam-base "INC"))))
  (fiveam:is (= 1 (instruction-descriptor-cycles (find-instruction 'fam-turbo "INC")))))

(fiveam:test clauses-merge-over-the-parent
  (let ((turbo (find-machine-descriptor 'fam-turbo))
        (small (find-machine-descriptor 'fam-lite-small)))
    (fiveam:is (null (machine-descriptor-clock-speed (find-machine-descriptor 'fam-base))))
    (fiveam:is (= 4000000 (machine-descriptor-clock-speed turbo)))
    (fiveam:is (member 'c (mapcar #'storage-element-name (machine-descriptor-elements turbo))))
    (fiveam:is (member 'z (mapcar #'storage-element-name (machine-descriptor-elements turbo))))
    (fiveam:is (= 12 (storage-element-addr-width (descriptor-element small 'ram))))
    (fiveam:is (= 8 (storage-element-width (descriptor-element small 'ram))))
    (fiveam:is (= 16 (storage-element-addr-width
                      (descriptor-element (find-machine-descriptor 'fam-base) 'ram))))))

(fiveam:test properties-merge-key-by-key
  (fiveam:is (equal "base" (machine-property 'fam-base :model)))
  (fiveam:is (equal "lite" (machine-property 'fam-lite :model)))
  (fiveam:is (= 1 (machine-property 'fam-lite :rev)))
  (fiveam:is (equal "lite" (machine-property 'fam-lite-small :model)))
  (fiveam:is (eq :none (machine-property 'fam-turbo :missing :none)))
  (fiveam:is (equal "lite" (machine-property (make-machine 'fam-lite) :model)))
  (fiveam:is (equal "lite" (machine-property (find-machine-descriptor 'fam-lite) :model))))

;;; Removal and undefined-opcode policy

(fiveam:test removed-mnemonic-is-unknown-to-the-assembler
  (fiveam:signals unknown-instruction (assemble "inc" :machine 'fam-lite))
  (fiveam:signals unknown-instruction (assemble "inc" :machine 'fam-lite-small))
  (fiveam:finishes (assemble "inc" :machine 'fam-base))
  (fiveam:finishes (assemble "lda #1" :machine 'fam-lite)))

(fiveam:test fault-policy-stops-on-an-unassigned-opcode
  (multiple-value-bind (m reason steps) (fam-run 'fam-base '(#x77))
    (declare (ignore m))
    (fiveam:is (eq :decode-failure reason))
    (fiveam:is (= 0 steps))))

(fiveam:test nop-policy-steps-over-a-removed-instruction
  (multiple-value-bind (m reason) (fam-run 'fam-lite '(#x10 5 #x20 #x00))
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 5 (sref m 'a)))
    (fiveam:is (= 4 (machine-cycles m)))))

(fiveam:test nop-policy-steps-over-an-unassigned-opcode-in-one-cell
  (multiple-value-bind (m reason) (fam-run 'fam-lite '(#x77 #x00))
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 2 (machine-cycles m)))
    (fiveam:is (= 2 (sref m 'pc)))))

(fiveam:test trap-policy-signals-without-moving-pc
  (multiple-value-bind (m reason steps condition) (fam-run 'fam-lite-small '(#x10 5 #x20 #x00))
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 2 steps))
    (fiveam:is (= 2 (sref m 'pc)))
    (fiveam:is (eq :undefined-opcode (lasm-trap-tag condition)))
    (fiveam:is (equal '(:pc 2 :opcode #x20) (lasm-trap-data condition)))))

(fiveam:test undefined-opcode-clause-is-validated
  (fiveam:signals machine-definition-error (eval '(defmachine fam-bad-policy
                                 (register pc :width 8) (memory ram :width 8 :addr-width 8)
                                 (undefined-opcode :panic))))
  (fiveam:signals machine-definition-error (eval '(defmachine fam-bad-policy
                                 (register pc :width 8) (memory ram :width 8 :addr-width 8)
                                 (undefined-opcode :nop) (undefined-opcode :trap)))))

;;; Definition errors

(fiveam:test extends-errors
  (fiveam:signals machine-definition-error (eval '(defmachine (fam-orphan (:extends fam-no-such-parent)))))
  (fiveam:signals machine-definition-error (eval '(defmachine (fam-self (:extends fam-self)))))
  (fiveam:signals machine-definition-error (macroexpand '(defmachine (fam-x (:bogus fam-base)))))
  (fiveam:signals machine-definition-error (macroexpand '(defmachine (fam-x (:extends fam-base) (:extends fam-lite))))))

(fiveam:test extends-cycle-is-rejected
  (fiveam:signals error (%define-machine 'fam-base 'fam-turbo nil))
  (fiveam:is (null (machine-descriptor-parent (find-machine-descriptor 'fam-base)))))

(fiveam:test child-only-clauses-need-a-parent
  (fiveam:signals machine-definition-error (eval '(defmachine fam-bare
                                 (register pc :width 8) (memory ram :width 8 :addr-width 8)
                                 (without-instructions foo))))
  (fiveam:signals machine-definition-error (eval '(defmachine fam-bare
                                 (register pc :width 8) (memory ram :width 8 :addr-width 8)
                                 (instruction-cycles (foo 1))))))

(fiveam:test incompatible-children-are-rejected
  (dolist (clauses '(((instruction-word :width 8 (field opcode 8)))
                     ((stack-pointer a :memory ram))
                     ((register a :width 8 :count 2))
                     ((memory ram :cell-width 16))
                     ((memory ram :endian :big))
                     ((memory ram2 :width 8 :addr-width 8))
                     ((memory ram :addr-width 32))
                     ((without-instructions inc) (instruction-cycles (inc 3)))))
    (fiveam:signals machine-definition-error (eval `(defmachine (fam-reject (:extends fam-base)) ,@clauses)))))

(fiveam:test failed-definition-keeps-the-existing-machine
  (fiveam:signals machine-definition-error (eval '(defmachine (fam-lite (:extends fam-base))
                                 (register a :width 8 :count 2))))
  (fiveam:is (eq 'fam-base (machine-descriptor-parent (find-machine-descriptor 'fam-lite))))
  (fiveam:is (find-instruction 'fam-lite "LDA")))

;;; Propagation of later parent definitions

(defmachine fam-p-base
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8))

(definstruction fam-p-base one (encoding (opcode 1)) (semantics nil))

(defmachine (fam-p-mid (:extends fam-p-base)) (instruction-cycles (late 4)))
(defmachine (fam-p-leaf (:extends fam-p-mid)))
(defmachine (fam-p-own (:extends fam-p-base)))
(defmachine (fam-p-removed (:extends fam-p-base)) (without-instructions late))

(definstruction fam-p-own late (encoding (opcode 9)) (semantics nil) (cycles 7))

(fiveam:test later-parent-definitions-reach-descendants
  (eval '(definstruction fam-p-base late (encoding (opcode 2)) (semantics nil) (cycles 2)))
  (fiveam:is (= 2 (instruction-descriptor-opcode (find-instruction 'fam-p-mid "LATE"))))
  (fiveam:is (eq 'fam-p-mid (instruction-descriptor-machine (find-instruction 'fam-p-mid "LATE"))))
  (fiveam:is (eq 'fam-p-leaf (instruction-descriptor-machine (find-instruction 'fam-p-leaf "LATE"))))
  (fiveam:is (= 4 (instruction-descriptor-cycles (find-instruction 'fam-p-mid "LATE"))))
  (fiveam:is (= 4 (instruction-descriptor-cycles (find-instruction 'fam-p-leaf "LATE"))))
  (fiveam:is (= 2 (instruction-descriptor-cycles (find-instruction 'fam-p-base "LATE")))))

(fiveam:test descendants-keep-their-own-definitions
  (eval '(definstruction fam-p-base late (encoding (opcode 2)) (semantics nil) (cycles 2)))
  (fiveam:is (= 9 (instruction-descriptor-opcode (find-instruction 'fam-p-own "LATE"))))
  (fiveam:is (= 7 (instruction-descriptor-cycles (find-instruction 'fam-p-own "LATE")))))

(fiveam:test descendants-that-removed-a-mnemonic-do-not-regain-it
  (eval '(definstruction fam-p-base late (encoding (opcode 2)) (semantics nil) (cycles 2)))
  (fiveam:signals unknown-instruction (find-instruction 'fam-p-removed "LATE"))
  (fiveam:signals unknown-instruction (find-instruction-by-opcode 'fam-p-removed 2)))

(fiveam:test redefining-a-mnemonic-replaces-it-in-descendants
  (eval '(definstruction fam-p-base late (encoding (opcode 3)) (semantics nil)))
  (fiveam:is (= 3 (instruction-descriptor-opcode (find-instruction 'fam-p-leaf "LATE"))))
  (fiveam:signals unknown-instruction (find-instruction-by-opcode 'fam-p-leaf 2)))

;;; Reloading a parent: stale inherited copies give way, a descendant's own
;;; mnemonics never do.

(defmachine fam-r-base
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8))

(definstruction fam-r-base foo (encoding (opcode 5)) (semantics nil))

(defmachine (fam-r-child (:extends fam-r-base)))

(definstruction fam-r-child mine (encoding (opcode 6)) (semantics nil))

(fiveam:test stale-inherited-copies-are-evicted
  (eval '(defmachine fam-r-base
          (register pc :width 8)
          (memory ram :width 8 :addr-width 8)))
  (eval '(definstruction fam-r-base bar (encoding (opcode 5)) (semantics nil)))
  (fiveam:is (string= "BAR" (instruction-descriptor-name
                             (find-instruction-by-opcode 'fam-r-child 5))))
  (fiveam:signals unknown-instruction (find-instruction 'fam-r-child "FOO")))

(fiveam:test conflicts-with-a-descendants-own-mnemonic-change-nothing
  (fiveam:signals opcode-conflict
    (eval '(definstruction fam-r-base qux (encoding (opcode 6)) (semantics nil))))
  (fiveam:signals unknown-instruction (find-instruction 'fam-r-base "QUX"))
  (fiveam:is (string= "MINE" (instruction-descriptor-name
                              (find-instruction-by-opcode 'fam-r-child 6)))))

;;; Regions

(defmachine fam-region-base
  (register pc :width 8)
  (memory ram :width 8 :addr-width 16
    (region boot #x0000 #x00ff :kind :rom)))

(defmachine (fam-region-keep (:extends fam-region-base))
  (memory ram :addr-width 16))

(defmachine (fam-region-replace (:extends fam-region-base))
  (memory ram (region vram #x8000 #x80ff)))

(defun fam-region-names (machine-name)
  (mapcar #'memory-region-name
          (storage-element-regions (descriptor-element (find-machine-descriptor machine-name) 'ram))))

(fiveam:test child-regions-replace-the-parents-wholesale
  (fiveam:is (equal '(boot) (fam-region-names 'fam-region-base)))
  (fiveam:is (equal '(boot) (fam-region-names 'fam-region-keep)))
  (fiveam:is (equal '(vram) (fam-region-names 'fam-region-replace))))

;;; Word-encoded family

(defmachine fam-word
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16 (field opcode 4) (field nnn 12)))

(defmode fam-word-nnn expr)

(definstruction fam-word fwsys
  (modes fam-word-nnn)
  (encoding (opcode 0) (fallback) (operand addr :field nnn))
  (semantics nil))

(definstruction fam-word fwcls
  (encoding (opcode 0) (field-value nnn #xe0))
  (semantics nil))

(defmachine (fam-word-lite (:extends fam-word))
  (without-instructions fwcls))

(defmachine (fam-word-trap (:extends fam-word-lite))
  (undefined-opcode :trap))

(defun fam-word-decoded-name (machine-name cells)
  (let ((descriptor (decode-instruction-at (vector-cell-reader cells) 0 machine-name)))
    (if (eq descriptor :decode-failure) descriptor (instruction-descriptor-name descriptor))))

(fiveam:test removed-word-instruction-does-not-decode-as-its-fallback
  (let ((cells (assembly-cells (assemble "fwcls" :machine 'fam-word))))
    (fiveam:is (string= "FWCLS" (fam-word-decoded-name 'fam-word cells)))
    (fiveam:is (eq :decode-failure (fam-word-decoded-name 'fam-word-lite cells)))
    (fiveam:is (eq :decode-failure (fam-word-decoded-name 'fam-word-trap cells)))))

(fiveam:test fallback-still-decodes-other-words-on-the-child
  (let ((cells (assembly-cells (assemble "fwsys $123" :machine 'fam-word-lite))))
    (fiveam:is (string= "FWSYS" (fam-word-decoded-name 'fam-word-lite cells)))))

(fiveam:test word-trap-carries-the-instruction-word
  (let ((m (make-machine 'fam-word-trap)))
    (load-program m (assemble "fwcls" :machine 'fam-word))
    (multiple-value-bind (reason steps condition) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 1 steps))
      (fiveam:is (equal '(:pc 0 :opcode #x00e0) (lasm-trap-data condition))))))

;;; Word family with an escaped operand: sibling indexes are rebuilt per child.

(defmachine fam-w8
  (register pc :width 8)
  (register a :width 8)
  (memory ram :width 8 :addr-width 8)
  (instruction-word :width 8 (field opcode 4) (field value 4)))

(defmode fam-w8-imm expr)

(definstruction fam-w8 loadv
  (modes fam-w8-imm)
  (encoding (opcode 1)
            (operand value :field value
              (variant (range 0 14) inline)
              (variant :else (extra-word :escape 15))))
  (semantics (set! a value))
  (cycles 1))

(definstruction fam-w8 stop
  (encoding (opcode 2) (field-value value 0))
  (semantics (trap :halt)))

(defmachine (fam-w8-child (:extends fam-w8))
  (instruction-cycles (loadv 3)))

(defmachine (fam-w8-nop (:extends fam-w8))
  (without-instructions loadv)
  (undefined-opcode :nop))

(fiveam:test word-siblings-decode-on-the-child
  (let ((m (make-machine 'fam-w8-child)))
    (load-program m (assemble "loadv 5
loadv 100
stop" :machine 'fam-w8-child))
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 3 steps))
      (fiveam:is (= 100 (sref m 'a)))
      (fiveam:is (= 7 (machine-cycles m)))))
  (dolist (descriptor (find-instruction-variants 'fam-w8-child "LOADV"))
    (fiveam:is (eq 'fam-w8-child (instruction-descriptor-machine descriptor)))
    (fiveam:is (not (member descriptor (find-instruction-variants 'fam-w8 "LOADV"))))))

(fiveam:test nop-skips-a-removed-word-instruction-with-its-extra-word
  (let ((m (make-machine 'fam-w8-nop)))
    (load-program m (assemble "loadv 100
stop" :machine 'fam-w8))
    (multiple-value-bind (reason steps) (run m)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 2 steps))
      (fiveam:is (= 0 (sref m 'a)))
      (fiveam:is (= 3 (machine-cycles m))))))

;;; A word instruction defined on the parent after its children exist reaches
;;; them with its decode order already computed.

(fiveam:test late-word-definitions-carry-a-computed-decode-order
  (eval '(definstruction fam-w8 loadw
          (modes fam-w8-imm)
          (encoding (opcode 4)
                    (operand value :field value
                      (variant (range 0 14) inline)
                      (variant :else (extra-word :escape 15))))
          (semantics (set! a value))))
  (let ((parent (find-instruction-variants 'fam-w8 "LOADW"))
        (child (find-instruction-variants 'fam-w8-child "LOADW")))
    (fiveam:is (= (length parent) (length child)))
    (fiveam:is (notany (lambda (d) (eq :dynamic (instruction-descriptor-word-decode-order d))) child))
    (fiveam:is (equal (mapcar #'instruction-descriptor-word-decode-order parent)
                      (mapcar #'instruction-descriptor-word-decode-order child)))))

(fiveam:test assembler-rejects-a-fallback-encoding-of-a-removed-instruction
  (fiveam:signals assembly-error (assemble "fwsys $0e0" :machine 'fam-word))
  (fiveam:signals assembly-error (assemble "fwsys $0e0" :machine 'fam-word-lite))
  (fiveam:finishes (assemble "fwsys $0e1" :machine 'fam-word-lite)))

;;; Snapshots

(fiveam:test snapshots-are-keyed-on-the-child
  (let ((m (fam-run 'fam-turbo '(#x10 5 #x00)))
        (fresh (make-machine 'fam-turbo)))
    (restore-snapshot fresh (machine-snapshot m))
    (fiveam:is (= 5 (sref fresh 'a)))
    (fiveam:signals error (restore-snapshot (make-machine 'fam-base) (machine-snapshot m)))))

;;; #29 -- a child machine inherits its parent's local modes and can shadow them.

(defmachine fam-modes-base (register pc :width 16) (memory ram :width 8 :addr-width 16))
(defmode (fam-mode (:machine fam-modes-base)) "(" expr ")" :width 1)
(defmachine (fam-modes-child (:extends fam-modes-base)) (clock-speed 2))
(defmachine (fam-modes-shadow (:extends fam-modes-base)) (clock-speed 3))
(defmode (fam-mode (:machine fam-modes-shadow)) "[" expr "]" :width 1)
(definstruction fam-modes-base fmv (modes fam-mode) (encoding (opcode 1) (operand :mode)) (semantics))
(definstruction fam-modes-shadow fmw (modes fam-mode) (encoding (opcode 2) (operand :mode)) (semantics))

(fiveam:test child-machine-assembles-inherited-instruction-with-parent-local-mode
  (fiveam:is (equalp #(1 5) (assembly-cells (assemble "fmv (5)" :machine 'fam-modes-child)))))

(fiveam:test child-shadowing-a-mode-leaves-inherited-instruction-on-the-parents-mode
  (fiveam:is (equalp #(1 5) (assembly-cells (assemble "fmv (5)" :machine 'fam-modes-shadow))))
  (fiveam:is (equalp #(2 5) (assembly-cells (assemble "fmw [5]" :machine 'fam-modes-shadow)))))

(defmachine fam-sp-base
  (register sp :width 16)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram :bounds (0 255)))

(fiveam:test child-must-keep-the-parents-stack-pointer-width-and-bounds
  (fiveam:signals machine-definition-error
    (eval '(defmachine (fam-sp-bounds (:extends fam-sp-base))
             (stack-pointer sp :memory ram :bounds (0 127)))))
  (fiveam:signals machine-definition-error
    (eval '(defmachine (fam-sp-width (:extends fam-sp-base))
             (stack-pointer sp :memory ram :bounds (0 255) :width 16)))))

;;; Removing storage and devices (#223), model reset PC (#226)

(defmachine rm-base
  (register a :width 8)
  (register b :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region kbd-port #xF000 #xF000 :kind :device :device keyboard))
  (flags z c)
  (reset-pc #x100)
  (device clock :id 1)
  (device keyboard :id 2)
  (device screen :id 3))

(definstruction rm-base seta
  (encoding (opcode #x10))
  (semantics (set! a 1))
  (cycles 1))

(definstruction rm-base setb
  (encoding (opcode #x11))
  (semantics (set! b 1))
  (cycles 1))

(defmachine (rm-cut (:extends rm-base))
  (without-instructions setb)
  (without-storage b c)
  (without-devices clock)
  (reset-pc #x200))

(defmachine (rm-cut-child (:extends rm-cut)))

(defun rm-device-ids (machine-name)
  (let ((m (make-machine machine-name)))
    (loop for i below (device-count m)
          collect (nth-value 0 (device-info m i)))))

(fiveam:test without-storage-removes-registers-and-flags
  (let ((names (mapcar #'storage-element-name
                       (machine-descriptor-elements (find-machine-descriptor 'rm-cut)))))
    (fiveam:is (not (intersection '(b c) names)))
    (fiveam:is (subsetp '(a pc ram z) names)))
  (fiveam:signals unknown-storage (sref (make-machine 'rm-cut) 'b))
  (fiveam:is (= 0 (sref (make-machine 'rm-cut) 'a))))

(fiveam:test removal-is-inherited-by-descendants
  (fiveam:is (equal (mapcar #'storage-element-name
                            (machine-descriptor-elements (find-machine-descriptor 'rm-cut)))
                    (mapcar #'storage-element-name
                            (machine-descriptor-elements (find-machine-descriptor 'rm-cut-child)))))
  (fiveam:is (equal '(2 3) (rm-device-ids 'rm-cut-child)))
  (fiveam:is (= #x200 (sref (make-machine 'rm-cut-child) 'pc))))

(fiveam:test without-devices-compacts-the-bus-and-region-indices
  (fiveam:is (equal '(1 2 3) (rm-device-ids 'rm-base)))
  (fiveam:is (equal '(2 3) (rm-device-ids 'rm-cut)))
  (let ((region (first (storage-element-regions
                        (gethash 'ram (machine-descriptor-table (find-machine-descriptor 'rm-cut)))))))
    (fiveam:is (= 0 (memory-region-device-index region)))))

(fiveam:test removing-a-device-a-region-uses-is-an-error
  (fiveam:signals machine-definition-error
    (eval '(defmachine (rm-no-keyboard (:extends rm-base)) (without-devices keyboard)))))

(fiveam:test removing-memory-is-an-error
  (fiveam:signals machine-definition-error
    (eval '(defmachine (rm-no-ram (:extends rm-base)) (without-storage ram)))))

(fiveam:test removing-and-redeclaring-a-name-is-an-error
  (fiveam:signals machine-definition-error
    (eval '(defmachine (rm-both (:extends rm-base))
            (without-storage b) (register b :width 8))))
  (fiveam:signals machine-definition-error
    (eval '(defmachine (rm-both-device (:extends rm-base))
            (without-devices clock) (device clock :id 9)))))

(fiveam:test removing-an-unknown-name-warns
  (let ((warned 0))
    (handler-bind ((style-warning (lambda (c) (incf warned) (muffle-warning c))))
      (eval '(defmachine (rm-unknown (:extends rm-base))
              (without-storage nope) (without-devices nope))))
    (fiveam:is (= 2 warned))
    (fiveam:is (eq 'rm-base (machine-descriptor-parent (find-machine-descriptor 'rm-unknown))))))

(fiveam:test removal-clauses-need-a-parent
  (fiveam:signals machine-definition-error
    (eval '(defmachine rm-orphan (register a :width 8) (without-storage a))))
  (fiveam:signals machine-definition-error
    (eval '(defmachine rm-orphan (register a :width 8) (without-devices a)))))

(defmachine rm-use-base
  (register a :width 8)
  (register b :width 8)
  (register pc :width 8)
  (memory ram :width 8 :addr-width 8))

(definstruction rm-use-base setb
  (encoding (opcode #x11))
  (semantics (set! b 1))
  (cycles 1))

(defmachine (rm-use-cut (:extends rm-use-base))
  (without-storage b))

(fiveam:test removing-a-register-an-inherited-instruction-uses-fails-at-run-time
  (let ((m (make-machine 'rm-use-cut)))
    (load-program m '(#x11))
    (fiveam:signals unknown-storage (step-machine m))))

(fiveam:test removing-a-register-interrupts-need-is-an-error
  (eval '(defmachine rm-irq-base
          (register a :width 8) (register vec :width 8) (register pc :width 8)
          (memory ram :width 8 :addr-width 8)
          (stack sk :width 8 :depth 4)
          (interrupts :vector vec :message a :save (pc) :stack sk)))
  (fiveam:signals machine-definition-error
    (eval '(defmachine (rm-irq-cut (:extends rm-irq-base)) (without-storage vec)))))

(fiveam:test reset-pc-is-inherited-and-overridden
  (fiveam:is (= #x100 (machine-descriptor-reset-pc (find-machine-descriptor 'rm-base))))
  (fiveam:is (= #x100 (machine-descriptor-reset-pc
                       (find-machine-descriptor
                        (progn (eval '(defmachine (rm-inherit (:extends rm-base)))) 'rm-inherit)))))
  (fiveam:is (= #x200 (machine-descriptor-reset-pc (find-machine-descriptor 'rm-cut)))))

(fiveam:test reset-pc-is-applied-by-make-machine-and-reset
  (let ((m (make-machine 'rm-base)))
    (fiveam:is (= #x100 (sref m 'pc)))
    (setf (sref m 'pc) 5 (sref m 'a) 7)
    (reset m)
    (fiveam:is (= #x100 (sref m 'pc)))
    (fiveam:is (= 0 (sref m 'a)))
    (load-program m '(#x10) :origin #x40)
    (fiveam:is (= #x40 (sref m 'pc)))))

(fiveam:test reset-pc-is-validated
  (dolist (form '((defmachine rm-pc-none (register a :width 8) (reset-pc 1))
                  (defmachine rm-pc-banked (register pc :width 8 :count 2) (reset-pc 1))
                  (defmachine rm-pc-wide (register pc :width 8) (reset-pc 256))
                  (defmachine rm-pc-negative (register pc :width 8) (reset-pc -1))
                  (defmachine rm-pc-twice (register pc :width 8) (reset-pc 1) (reset-pc 2))
                  (defmachine (rm-pc-narrowed (:extends rm-base)) (register pc :width 8))))
    (fiveam:signals machine-definition-error (eval form))))
