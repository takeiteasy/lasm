;;;; examples/family.lisp
;;;;
;;;; A machine family: one base machine and children that add, remove and
;;;; re-time instructions, change memory size and clock rate, and choose what
;;;; happens on an opcode they don't implement. See docs/machine-families.md.
;;;;
;;;; Run with:  sbcl --script examples/family.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine tiny8
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z)
  (properties :model "tiny8" :rev 1))

(definstruction tiny8 lda
  (modes immediate)
  (encoding (opcode #x10) (operand :mode))
  (semantics (set! a operand))
  (cycles 2))

(definstruction tiny8 add
  (modes immediate)
  (encoding (opcode #x20) (operand :mode))
  (semantics (set! a (wrap-value (+ a operand) 8)))
  (cycles 3))

(definstruction tiny8 hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; Cheaper ADD, faster clock.
(defmachine (tiny8-turbo (:extends tiny8))
  (instruction-cycles (add 1))
  (clock-speed 4000000)
  (properties :model "tiny8-turbo"))

;; No ADD; an unimplemented opcode is skipped instead of stopping the run.
(defmachine (tiny8-lite (:extends tiny8))
  (without-instructions add)
  (undefined-opcode :nop)
  (memory ram :addr-width 12)
  (properties :model "tiny8-lite"))

;; Loses the Z flag and starts execution at $100.
(defmachine (tiny8-boot (:extends tiny8))
  (without-storage z)
  (reset-pc #x100))

;; Defined after the children; every child that still has it picks it up.
(definstruction tiny8 sub
  (modes immediate)
  (encoding (opcode #x21) (operand :mode))
  (semantics (set! a (wrap-value (- a operand) 8)))
  (cycles 3))

(defparameter *image* '(#x10 5 #x20 3 #x00)) ; lda #5 / add #3 / hlt

(dolist (name '(tiny8 tiny8-turbo tiny8-lite))
  (let ((m (make-machine name)))
    (load-program m *image*)
    (let ((reason (run m)))
      (format t "~&~12A model ~12S  A=~D  cycles=~D  stopped: ~S~%"
              name (machine-property m :model) (sref m 'a) (machine-cycles m) reason))))

(let ((base (make-machine 'tiny8))
      (turbo (make-machine 'tiny8-turbo))
      (lite (make-machine 'tiny8-lite)))
  (load-program base *image*)
  (load-program turbo *image*)
  (load-program lite *image*)
  (run base)
  (run turbo)
  (run lite)
  (assert (= 8 (sref base 'a)))
  (assert (= 8 (sref turbo 'a)))
  (assert (= 5 (sref lite 'a)))
  (assert (= 6 (machine-cycles base)))
  (assert (= 4 (machine-cycles turbo)))
  (assert (= 5 (machine-cycles lite))))   ; ADD's two cells are skipped at one cycle each

(let ((m (make-machine 'tiny8-boot)))
  (assert (= #x100 (sref m 'pc)))
  (assert (null (find 'z (machine-descriptor-elements (machine-descriptor m))
                      :key #'storage-element-name))))

(assert (find-instruction 'tiny8-turbo "SUB"))
(assert (find-instruction 'tiny8-lite "SUB"))
(assert (handler-case (progn (assemble "add #1" :machine 'tiny8-lite) nil)
          (unknown-instruction () t)))

;;; A word-encoded family: the child traps on an instruction it removed.

(defmachine w16
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16 (field opcode 4) (field nnn 12)))

(defmode w16-imm "#" expr)

(definstruction w16 ldi
  (modes w16-imm)
  (encoding (opcode 1) (operand value :field nnn))
  (semantics (set! a value)))

(definstruction w16 halt
  (encoding (opcode 0) (field-value nnn 0))
  (semantics (trap :halt)))

(defmachine (w16-min (:extends w16))
  (without-instructions ldi)
  (undefined-opcode :trap))

(let ((m (make-machine 'w16-min)))
  (load-program m (assemble "ldi #7" :machine 'w16))
  (multiple-value-bind (reason steps condition) (run m)
    (format t "~&w16-min: ~S after ~D step~:P, tag ~S~%"
            reason steps (lasm-trap-tag condition))
    (assert (eq :trap reason))
    (assert (eq :undefined-opcode (lasm-trap-tag condition)))))
