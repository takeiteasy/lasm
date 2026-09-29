;;;; tests/isa.lisp
;;;; fiveam tests for the ISA/CPU split: DEFISA, DEFCPU and the DEFMACHINE bridge.

(in-package #:lasm)

(fiveam:def-suite isa :in lasm)
(fiveam:in-suite isa)

(defisa isa-toy
  (register a :width 8)
  (register pc :width 16)
  (register sp :width 16)
  (memory ram :width 8 :addr-width 16)
  (stack-pointer sp :memory ram)
  (interrupts :vector a :message a :save (pc) :stack sp))

(definstruction isa-toy nop (encoding (opcode 0)) (semantics nil))
(definstruction isa-toy inc
  (encoding (opcode 1))
  (semantics (set! a (wrap-value (1+ a) 8)))
  (cycles 2))
(definstruction isa-toy dec
  (encoding (opcode 2))
  (semantics (set! a (wrap-value (1- a) 8)))
  (cycles 2))

(defcpu (isa-full (:isa isa-toy))
  (clock-speed 1000)
  (interrupts :queue 64))

(defcpu (isa-small (:isa isa-toy))
  (without-instructions dec)
  (undefined-opcode :nop)
  (instruction-cycles (inc 5))
  (memory ram :addr-width 12)
  (register pc :width 12)
  (clock-speed 4000000)
  (interrupts :queue 8))

(defcpu (isa-small-turbo (:extends isa-small))
  (instruction-cycles (inc 1))
  (clock-speed 8000000))

(fiveam:test cpus-of-one-isa-share-its-instructions
  (fiveam:is (eq (find-instruction 'isa-full "NOP") (find-instruction 'isa-toy "NOP")))
  (fiveam:is (eq (find-instruction 'isa-small "NOP") (find-instruction 'isa-toy "NOP")))
  (fiveam:is (eq 'isa-toy (instruction-descriptor-machine (find-instruction 'isa-small "INC")))))

(fiveam:test a-cpu-removes-and-retimes-without-touching-its-isa
  (fiveam:is (find-instruction 'isa-full "DEC"))
  (fiveam:signals unknown-instruction (find-instruction 'isa-small "DEC"))
  (fiveam:signals unknown-instruction (find-instruction-by-opcode 'isa-small 2))
  (fiveam:is (= 2 (instruction-descriptor-cycles (find-instruction 'isa-full "INC"))))
  (fiveam:is (= 5 (instruction-descriptor-cycles (find-instruction 'isa-small "INC"))))
  (fiveam:is (= 2 (instruction-descriptor-cycles (find-instruction 'isa-toy "INC")))))

(fiveam:test a-cpu-extending-a-cpu-inherits-removals-and-overrides-cycles
  (fiveam:is (eq 'isa-toy (machine-descriptor-isa (find-machine-descriptor 'isa-small-turbo))))
  (fiveam:is (eq 'isa-small (machine-descriptor-parent (find-machine-descriptor 'isa-small-turbo))))
  (fiveam:signals unknown-instruction (find-instruction 'isa-small-turbo "DEC"))
  (fiveam:is (= 1 (instruction-descriptor-cycles (find-instruction 'isa-small-turbo "INC"))))
  (fiveam:is (= 5 (instruction-descriptor-cycles (find-instruction 'isa-small "INC")))))

(fiveam:test cpu-side-settings-belong-to-the-cpu
  (fiveam:is (= 1000 (machine-descriptor-clock-speed (find-machine-descriptor 'isa-full))))
  (fiveam:is (= 4000000 (machine-descriptor-clock-speed (find-machine-descriptor 'isa-small))))
  (fiveam:is (= 64 (interrupt-descriptor-queue-depth
                    (machine-descriptor-interrupts (find-machine-descriptor 'isa-full)))))
  (fiveam:is (= 8 (interrupt-descriptor-queue-depth
                   (machine-descriptor-interrupts (find-machine-descriptor 'isa-small)))))
  (fiveam:is (= 16 (storage-element-width (descriptor-element (find-machine-descriptor 'isa-full) 'pc))))
  (fiveam:is (= 12 (storage-element-width (descriptor-element (find-machine-descriptor 'isa-small) 'pc))))
  (fiveam:is (= 12 (storage-element-addr-width (descriptor-element (find-machine-descriptor 'isa-small) 'ram)))))

(fiveam:test an-isa-alone-is-not-a-machine
  (fiveam:is (find-isa-descriptor 'isa-toy))
  (fiveam:signals unknown-machine (make-machine 'isa-toy))
  (fiveam:signals unknown-isa (find-isa-descriptor 'isa-full)))

(fiveam:test the-assembler-and-decoder-see-the-cpus-instructions
  (fiveam:is (equalp #(2) (assembly-cells (assemble "dec" :machine 'isa-full))))
  (fiveam:signals lasm-error (assemble "dec" :machine 'isa-small))
  (multiple-value-bind (size opcode removedp)
      (%undefined-opcode-extent (lambda (address) (if (zerop address) 2 0)) 0 'isa-small nil)
    (fiveam:is (= 1 size))
    (fiveam:is (= 2 opcode))
    (fiveam:is (eq t removedp))))

(fiveam:test a-removed-instruction-steps-over-as-a-nop
  (let ((m (make-machine 'isa-small)))
    (load-program m #(2 1))
    (step-machine m)
    (step-machine m)
    (fiveam:is (= 1 (sref m 'a)))
    (fiveam:is (= 2 (sref m 'pc)))))

(fiveam:test cycles-follow-the-cpu
  (dolist (case '((isa-full 2) (isa-small 5) (isa-small-turbo 1)))
    (let ((m (make-machine (first case))))
      (load-program m #(1))
      (step-machine m)
      (fiveam:is (= (second case) (machine-cycles m))))))

;;; An ISA extending an ISA

(defisa (isa-wide (:extends isa-toy))
  (register b :width 8))

(definstruction isa-wide swp
  (encoding (opcode 3))
  (semantics (rotatef a b)))

(defcpu (isa-wide-cpu (:isa isa-wide)))
(defcpu (isa-toy-on-wide (:isa isa-wide) (:extends isa-small)))

(fiveam:test a-sub-isa-adds-storage-and-instructions
  (fiveam:is (find-instruction 'isa-wide-cpu "SWP"))
  (fiveam:is (find-instruction 'isa-wide-cpu "INC"))
  (fiveam:signals unknown-instruction (find-instruction 'isa-full "SWP"))
  (fiveam:is (gethash 'b (machine-descriptor-table (find-machine-descriptor 'isa-wide-cpu))))
  (fiveam:is (null (gethash 'b (machine-descriptor-table (find-machine-descriptor 'isa-full))))))

(fiveam:test a-cpu-extending-a-cpu-may-move-to-a-descendant-isa
  (fiveam:is (eq 'isa-wide (machine-descriptor-isa (find-machine-descriptor 'isa-toy-on-wide))))
  (fiveam:signals unknown-instruction (find-instruction 'isa-toy-on-wide "DEC"))
  (fiveam:is (find-instruction 'isa-toy-on-wide "SWP")))

(fiveam:test a-cpu-cannot-move-to-an-unrelated-isa
  (eval '(defisa isa-other (register pc :width 8) (memory ram :width 8 :addr-width 8)))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (isa-stray (:isa isa-other) (:extends isa-small)))))
  (fiveam:is (null (gethash 'isa-stray *machines*))))

(fiveam:test a-later-instruction-reaches-every-cpu-of-the-isa-chain
  (eval '(definstruction isa-toy isa-late (encoding (opcode 9)) (semantics nil) (cycles 3)))
  (dolist (cpu '(isa-full isa-small isa-small-turbo isa-wide-cpu isa-toy-on-wide))
    (fiveam:is (= 9 (instruction-descriptor-opcode (find-instruction cpu "ISA-LATE")))))
  (fiveam:is (= 3 (instruction-descriptor-cycles (find-instruction 'isa-full "ISA-LATE")))))

(fiveam:test an-opcode-conflict-changes-nothing
  (fiveam:signals opcode-conflict
    (eval '(definstruction isa-wide clash (encoding (opcode 1)) (semantics nil))))
  (fiveam:signals unknown-instruction (find-instruction 'isa-wide-cpu "CLASH"))
  (fiveam:is (string= "INC" (instruction-descriptor-name (find-instruction-by-opcode 'isa-full 1)))))

;;; Modes

(defisa isa-moded
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode (isa-paren (:isa isa-moded)) "(" expr ")" :width 1)

(definstruction isa-moded ld
  (modes isa-paren)
  (encoding (opcode 4) (operand :mode))
  (semantics (set! a operand)))

(defcpu (isa-moded-cpu (:isa isa-moded)))

(fiveam:test isa-local-modes-apply-to-the-cpus-of-the-isa
  (fiveam:is (equalp #(4 7) (assembly-cells (assemble "ld (7)" :machine 'isa-moded-cpu))))
  (fiveam:signals unknown-mode (find-mode-descriptor 'isa-paren))
  (fiveam:signals mode-definition-error (eval '(defmode (isa-bad (:isa isa-moded-cpu)) "[" expr "]"))))

;;; The DEFMACHINE bridge

(defmachine isa-bridge
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region rom #x0000 #x00ff :kind :rom))
  (clock-speed 500)
  (properties :model "bridge"))

(defmachine (isa-bridge-child (:extends isa-bridge))
  (register b :width 8)
  (memory ram :addr-width 12)
  (clock-speed 900))

(fiveam:test defmachine-defines-an-isa-and-a-cpu-of-the-same-name
  (fiveam:is (eq 'isa-bridge (machine-descriptor-isa (find-machine-descriptor 'isa-bridge))))
  (fiveam:is (null (machine-descriptor-clock-speed (find-isa-descriptor 'isa-bridge))))
  (fiveam:is (= 500 (machine-descriptor-clock-speed (find-machine-descriptor 'isa-bridge))))
  (fiveam:is (equal "bridge" (machine-property 'isa-bridge :model)))
  (fiveam:is (null (storage-element-regions (descriptor-element (find-isa-descriptor 'isa-bridge) 'ram))))
  (fiveam:is (= 1 (length (storage-element-regions
                           (descriptor-element (find-machine-descriptor 'isa-bridge) 'ram))))))

(fiveam:test a-bridge-child-extends-both-halves
  (let ((isa (find-isa-descriptor 'isa-bridge-child))
        (cpu (find-machine-descriptor 'isa-bridge-child)))
    (fiveam:is (eq 'isa-bridge (machine-descriptor-parent isa)))
    (fiveam:is (eq 'isa-bridge (machine-descriptor-parent cpu)))
    (fiveam:is (eq 'isa-bridge-child (machine-descriptor-isa cpu)))
    (fiveam:is (gethash 'b (machine-descriptor-table isa)))
    (fiveam:is (null (gethash 'b (machine-descriptor-table (find-isa-descriptor 'isa-bridge)))))
    (fiveam:is (= 12 (storage-element-addr-width (descriptor-element cpu 'ram))))
    (fiveam:is (= 16 (storage-element-addr-width (descriptor-element isa 'ram))))
    (fiveam:is (= 900 (machine-descriptor-clock-speed cpu)))))

;;; Rejected definitions

(fiveam:test defisa-rejects-cpu-clauses
  (dolist (clause '((clock-speed 5) (device d) (undefined-opcode :trap) (properties :a 1)
                    (without-instructions inc) (interrupts :vector a :message a :save (pc) :queue 4)
                    (memory ram :width 8 :addr-width 8 (region r 0 1))))
    (fiveam:signals machine-definition-error
      (eval `(defisa isa-bad-isa (register a :width 8) (register pc :width 8)
               (memory ram :width 8 :addr-width 8) ,clause)))
    (fiveam:is (null (gethash 'isa-bad-isa *isas*)))))

(fiveam:test defcpu-rejects-isa-clauses
  (dolist (clause '((flags z) (instruction-word :width 8 (field opcode 8)) (stack-pointer sp)
                    (privilege :level a :levels (x)) (register extra :width 8)
                    (memory rom2 :width 8 :addr-width 8) (interrupts :vector a)))
    (fiveam:signals machine-definition-error (eval `(defcpu (isa-bad-cpu (:isa isa-toy)) ,clause)))
    (fiveam:is (null (gethash 'isa-bad-cpu *machines*)))))

(fiveam:test defcpu-needs-a-defined-isa
  (fiveam:signals machine-definition-error (eval '(defcpu isa-no-isa (clock-speed 5))))
  (fiveam:signals machine-definition-error (eval '(defcpu (isa-no-isa (:isa isa-undefined)))))
  (fiveam:signals machine-definition-error (eval '(defcpu (isa-no-isa (:extends isa-undefined)))))
  (fiveam:signals machine-definition-error (eval '(defcpu (isa-no-isa (:bogus isa-toy))))))

(fiveam:test a-failed-cpu-keeps-the-existing-one
  (fiveam:signals machine-definition-error
    (eval '(defcpu (isa-small (:isa isa-toy)) (register a :width 8 :count 2))))
  (fiveam:is (= 5 (instruction-descriptor-cycles (find-instruction 'isa-small "INC")))))

(fiveam:test a-failed-bridge-registers-neither-half
  (fiveam:signals machine-definition-error
    (eval '(defmachine isa-bridge-bad
            (register a :width 8) (register pc :width 8) (memory ram :width 8 :addr-width 8)
            (reset-pc 300))))
  (fiveam:is (null (gethash 'isa-bridge-bad *isas*)))
  (fiveam:is (null (gethash 'isa-bridge-bad *machines*))))
