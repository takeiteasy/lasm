;;;; tests/instruction.lisp
;;;; fiveam tests for DEFINSTRUCTION and the M1 addressing-mode/encoding
;;;; pipeline (instruction.lisp).

(in-package #:lasm)

(fiveam:def-suite instruction :in lasm)
(fiveam:in-suite instruction)

;; A dedicated fixture (rather than reusing TEST-MACHINE from suites.lisp):
;; one memory element, so %DEFAULT-ABSOLUTE-WIDTH can resolve without
;; ambiguity, and a PC register per the "PC is a plain register" convention.
(defmachine instr-test-machine
  (register a :width 8)
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

(definstruction instr-test-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand)))

(definstruction instr-test-machine adc
  (modes absolute)
  (encoding (opcode #x6D) (operand :mode))
  (semantics
    (let ((r (+ a (mref machine 'ram operand) c)))
      (set! a (wrap-value r 8))
      (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction instr-test-machine bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics
    (when (zerop z)
      (set! pc operand))))

(definstruction instr-test-machine nop
  (encoding (opcode #xEA))
  (semantics nil))

(definstruction instr-test-machine jmpfar
  (modes absolute)
  (encoding (opcode #x4C) (operand :width 3))
  (semantics (set! pc operand)))

;; A second fixture with two memory elements, so (operand :mode) on an
;; ABSOLUTE instruction is genuinely ambiguous -- exercises the "more than
;; one memory element" branch of %DEFAULT-ABSOLUTE-WIDTH. An explicit
;; :width sidesteps the ambiguity, as STA-RAM below demonstrates.
(defmachine multi-memory-machine
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (memory io :width 8 :addr-width 8))

(definstruction multi-memory-machine sta-ram
  (modes absolute)
  (encoding (opcode #x8D) (operand :width 2))
  (semantics (setf (mref machine 'ram operand) a)))

;;; DEFINSTRUCTION registration

(fiveam:test definstruction-registers-by-mnemonic-and-opcode
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (string= "LDX" (instruction-descriptor-name ldx)))
    (fiveam:is (eq :immediate (instruction-descriptor-mode ldx)))
    (fiveam:is (= #xA2 (instruction-descriptor-opcode ldx)))
    (fiveam:is (eq ldx (find-instruction-by-opcode 'instr-test-machine #xA2)))))

(fiveam:test definstruction-redefinition-replaces
  (definstruction instr-test-machine ldx
    (modes immediate)
    (encoding (opcode #xA9) (operand :mode))
    (semantics (set! x operand)))
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (= #xA9 (instruction-descriptor-opcode ldx)))
    (fiveam:is (eq ldx (find-instruction-by-opcode 'instr-test-machine #xA9)))
    ;; the old opcode (#xA2) must no longer resolve to anything -- it was
    ;; superseded by this redefinition, not left dangling in the opcode table
    (fiveam:signals unknown-instruction (find-instruction-by-opcode 'instr-test-machine #xA2)))
  ;; restore for other tests in this file
  (definstruction instr-test-machine ldx
    (modes immediate)
    (encoding (opcode #xA2) (operand :mode))
    (semantics (set! x operand))))

(fiveam:test no-operand-instruction
  (let ((nop (find-instruction 'instr-test-machine 'nop)))
    (fiveam:is (null (instruction-descriptor-mode nop)))
    (fiveam:is (null (instruction-descriptor-operand-width nop)))))

;;; Clause errors

(fiveam:test missing-encoding-clause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (semantics (set! x operand))))))

(fiveam:test missing-semantics-clause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF) (operand :mode))))))

(fiveam:test two-modes-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate absolute)
             (encoding (opcode #xFF) (operand :mode))
             (semantics (set! x operand))))))

(fiveam:test unknown-clause-head-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (bogus-clause 1)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test mode-without-operand-subclause-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (modes immediate)
             (encoding (opcode #xFF))
             (semantics nil)))))

(fiveam:test operand-subclause-without-mode-signals-error
  (fiveam:signals error
    (eval '(definstruction instr-test-machine bogus
             (encoding (opcode #xFF) (operand :mode))
             (semantics nil)))))

(definstruction instr-test-machine bogus-ref
  (modes immediate)
  (encoding (opcode #xFE) (operand :mode))
  (semantics (mref machine 'nonexistent-ram operand)))

(fiveam:test unknown-storage-reference-in-semantics-signals-at-runtime
  (let ((m (make-machine 'instr-test-machine))
        (bogus-ref (find-instruction 'instr-test-machine 'bogus-ref)))
    (fiveam:signals unknown-storage (execute-instruction bogus-ref m 10))))

;;; match-operand-mode

(defun %single-operand (string)
  (first (statement-operands (first (parse (format nil "nop ~A" string))))))

(fiveam:test match-operand-mode-immediate
  (let ((ast (match-operand-mode (%single-operand "#10") :immediate)))
    (fiveam:is (expr-number-p ast))
    (fiveam:is (= 10 (expr-number-value ast)))))

(fiveam:test match-operand-mode-immediate-rejects-absolute-syntax
  (fiveam:signals parse-failure
    (match-operand-mode (%single-operand "$1000") :immediate)))

(fiveam:test match-operand-mode-absolute
  (let ((ast (match-operand-mode (%single-operand "$1000") :absolute)))
    (fiveam:is (expr-number-p ast))
    (fiveam:is (= #x1000 (expr-number-value ast)))))

(fiveam:test match-operand-mode-absolute-rejects-immediate-syntax
  (fiveam:signals parse-failure
    (match-operand-mode (%single-operand "#10") :absolute)))

(fiveam:test match-operand-mode-trailing-token-signals-parse-failure
  (fiveam:signals parse-failure
    (match-operand-mode (%single-operand "#10 20") :immediate)))

;;; eval-expr-constant

(fiveam:test eval-expr-constant-arithmetic
  (fiveam:is (= 7 (eval-expr-constant (match-operand-mode (%single-operand "#(3+4)") :immediate)))))

(fiveam:test eval-expr-constant-lo-hi
  (fiveam:is (= #x34 (eval-expr-constant (match-operand-mode (%single-operand "#<$1234") :immediate))))
  (fiveam:is (= #x12 (eval-expr-constant (match-operand-mode (%single-operand "#>$1234") :immediate)))))

(fiveam:test eval-expr-constant-unresolved-label-signals
  (fiveam:signals unresolved-label
    (eval-expr-constant (match-operand-mode (%single-operand "loop") :absolute))))

;;; encode-instruction

(fiveam:test encode-immediate-instruction
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (equal (list #xA2 10) (encode-instruction ldx 10)))))

(fiveam:test encode-absolute-instruction-little-endian
  (let ((adc (find-instruction 'instr-test-machine 'adc)))
    (fiveam:is (equal (list #x6D #x00 #x10) (encode-instruction adc #x1000)))))

(fiveam:test encode-instruction-masks-overwide-value
  (let ((ldx (find-instruction 'instr-test-machine 'ldx)))
    (fiveam:is (equal (list #xA2 #x2C) (encode-instruction ldx 300)))))

(fiveam:test encode-no-operand-instruction
  (let ((nop (find-instruction 'instr-test-machine 'nop)))
    (fiveam:is (equal (list #xEA) (encode-instruction nop nil)))))

(fiveam:test explicit-operand-width-overrides-mode-default
  (let ((jmpfar (find-instruction 'instr-test-machine 'jmpfar)))
    (fiveam:is (= 3 (instruction-descriptor-operand-width jmpfar)))
    ;; distinct bytes in every position pin little-endian ordering, not just width
    (fiveam:is (equal (list #x4C #x56 #x34 #x12) (encode-instruction jmpfar #x123456)))))

(fiveam:test ambiguous-memory-element-requires-explicit-width
  (fiveam:signals error
    (eval '(definstruction multi-memory-machine bogus
             (modes absolute)
             (encoding (opcode #xFF) (operand :mode))
             (semantics (set! a operand)))))
  (fiveam:is (= 2 (instruction-descriptor-operand-width
                   (find-instruction 'multi-memory-machine 'sta-ram)))))

;;; execute-instruction

(fiveam:test execute-immediate-sets-register
  (let ((m (make-machine 'instr-test-machine))
        (ldx (find-instruction 'instr-test-machine 'ldx)))
    (execute-instruction ldx m 10)
    (fiveam:is (= 10 (sref m 'x)))))

(fiveam:test execute-absolute-reads-memory-and-sets-flags
  (let ((m (make-machine 'instr-test-machine))
        (adc (find-instruction 'instr-test-machine 'adc)))
    (setf (sref m 'a) 200)
    (setf (mref m 'ram #x1000) 100)
    (execute-instruction adc m #x1000)
    (fiveam:is (= 44 (sref m 'a)))          ; 200 + 100 wraps mod 256
    (fiveam:is (= 1 (flag m 'c)))
    (fiveam:is (= 0 (flag m 'z)))))

(fiveam:test execute-branch-sets-pc-conditionally
  (let ((m (make-machine 'instr-test-machine))
        (bne (find-instruction 'instr-test-machine 'bne)))
    (setf (flag m 'z) t)
    (execute-instruction bne m #x2000)
    (fiveam:is (= 0 (sref m 'pc)))          ; branch not taken
    (setf (flag m 'z) nil)
    (execute-instruction bne m #x2000)
    (fiveam:is (= #x2000 (sref m 'pc)))))   ; branch taken
