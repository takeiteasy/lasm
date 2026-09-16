;;;; examples/sharedopcode.lisp
;;;;
;;;; Shared opcodes across mode-distinguished variants: an ANIMA-16-shaped
;;;; machine's opcode space is small (5 bits here, same as examples/
;;;; anima16.lisp) and often fully allocated, so a mnemonic that wants a
;;;; second addressing form cannot simply buy a second opcode for it -- the
;;;; distinction has to live entirely in the operand fields. LD below
;;;; declares two (MODES ...) clauses -- one reading a source register's own
;;;; value, one loading a small literal -- both at opcode 1, told apart at
;;;; decode time purely by which raw bits field A's value actually falls
;;;; into. ADDI and SUBI go further: two *different* mnemonics sharing one
;;;; opcode, each with its own field-A operand, told apart the same way --
;;;; every field discriminating two co-tenants here is a real operand a
;;;; program actually writes, not a synthetic marker; DEFINSTRUCTION has no
;;;; way to bake a field value that isn't backed by one of a mode's own
;;;; EXPR holes.
;;;;
;;;; This is the registration/decode half of the picture; examples/
;;;; anima16.lisp covers the complementary case, one mnemonic's operand
;;;; syntax steering a shared opcode's field code via `(choice mode)`.
;;;; DECODE-INSTRUCTION-AT (decoder.lisp) tries every descriptor registered
;;;; under an opcode in turn and returns the first whose fields the fetched
;;;; bits actually match; DEFINSTRUCTION itself only accepts co-tenants
;;;; whose fields are provably disjoint at some hole, so that trial is
;;;; always unambiguous, never a race between overlapping candidates.
;;;;
;;;; Run with:  sbcl --script examples/sharedopcode.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: PC (convention), a banked 16-bit REG register (8 elements), one
;;; word-addressed memory element -- same instruction-word shape as
;;; examples/anima16.lisp: a 6-bit `a` field, a 5-bit `b` field, a 5-bit
;;; `opcode` field, MSB-first.

(defmachine sharedopcodefoo
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode ld-reg-mode expr "," expr)
(defmode ld-imm-mode expr "," "#" expr)

;; LD dst, src -- reg[dst] := reg[src]; field A's value (0-7) is a plain
;; register index.
;; LD dst, #lit -- reg[dst] := lit; field A's value (33-63, biased) is the
;; literal itself.
;; One mnemonic, two (MODES ...) clauses, one opcode: before #105 this
;; registered with no error and then mis-decoded -- one form returning
;; :DECODE-FAILURE, the other silently reporting whichever descriptor
;; happened to occupy the opcode table (last-write-wins). Today it requires
;; (and gets, since the two field-A ranges are disjoint) a decodability
;; check at DEFINSTRUCTION time instead.
(definstruction sharedopcodefoo ld
  (modes
    (ld-reg-mode (opcode 1)
      (operand dst :field b)
      (operand src :field a (variant (range 0 7) inline))
      (semantics (set! (reg dst) (reg src))))
    (ld-imm-mode (opcode 1)
      (operand dst :field b)
      (operand lit :field a (variant (range 0 30) inline :bias 33))
      (semantics (set! (reg dst) lit)))))

(defmode addi-mode expr "," "#" expr)

;; ADDI dst, #n -- reg[dst] += n. SUBI dst, #n -- reg[dst] -= n. Two
;; distinct mnemonics sharing opcode 2, told apart the same way LD's two
;; modes are: ADDI's own field-A operand packs into 0-31, SUBI's into
;; 32-63 -- disjoint, so decode never has to guess which mnemonic a given
;; word came from.
(definstruction sharedopcodefoo addi
  (modes addi-mode)
  (encoding
    (opcode 2)
    (operand dst :field b)
    (operand n :field a (variant (range 0 31) inline :bias 0)))
  (semantics (set! (reg dst) (+ (reg dst) n))))

(definstruction sharedopcodefoo subi
  (modes addi-mode)
  (encoding
    (opcode 2)
    (operand dst :field b)
    (operand n :field a (variant (range 0 31) inline :bias 32)))
  (semantics (set! (reg dst) (- (reg dst) n))))

(definstruction sharedopcodefoo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(format t "~&LD's two mode-distinguished forms, sharing opcode 1:~%")
(let ((reg-form (assembly-cells (assemble "ld 1, 0" :machine 'sharedopcodefoo)))
      (imm-form (assembly-cells (assemble "ld 1, #5" :machine 'sharedopcodefoo))))
  (format t "  ld 1, 0   -> ~{~4,'0X~^ ~}~%" (coerce reg-form 'list))
  (format t "  ld 1, #5  -> ~{~4,'0X~^ ~}~%" (coerce imm-form 'list))
  (dolist (case (list (cons reg-form 'ld-reg-mode) (cons imm-form 'ld-imm-mode)))
    (multiple-value-bind (descriptor) (decode-instruction-at (vector-cell-reader (car case)) 0 'sharedopcodefoo)
      (assert (not (eq :decode-failure descriptor)))
      (assert (string= "LD" (instruction-descriptor-name descriptor)))
      (assert (eq (cdr case) (mode-descriptor-name (instruction-descriptor-mode descriptor))))))
  (format t "~%Both forms decode back to LD, each via its own mode -- neither ~
:DECODE-FAILURE nor the other's mode.~%"))

(format t "~%ADDI/SUBI, two mnemonics sharing opcode 2:~%")
(let ((addi-form (assembly-cells (assemble "addi 2, #3" :machine 'sharedopcodefoo)))
      (subi-form (assembly-cells (assemble "subi 2, #3" :machine 'sharedopcodefoo))))
  (format t "  addi 2, #3  -> ~{~4,'0X~^ ~}~%" (coerce addi-form 'list))
  (format t "  subi 2, #3  -> ~{~4,'0X~^ ~}~%" (coerce subi-form 'list))
  (assert (not (equalp addi-form subi-form)))
  (dolist (case (list (cons addi-form "ADDI") (cons subi-form "SUBI")))
    (multiple-value-bind (descriptor) (decode-instruction-at (vector-cell-reader (car case)) 0 'sharedopcodefoo)
      (assert (not (eq :decode-failure descriptor)))
      (assert (string= (cdr case) (instruction-descriptor-name descriptor)))))
  (format t "~%Both decode back to their own mnemonic.~%"))

(defparameter *source*
  "ld 0, #10   ; reg0 = 10                     LD, immediate mode
ld 1, 0     ; reg1 = reg0 = 10              LD, register mode
addi 1, #4  ; reg1 = 14
subi 0, #1  ; reg0 = 9
hlt")

(format t "~&~%Source:~%~A~2%" *source*)

(format t "Assembling and running:~%")
(let ((assembly (assemble *source* :machine 'sharedopcodefoo)))
  (let ((m (make-machine 'sharedopcodefoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  reg0=~D reg1=~D~%" (regref m 'reg 0) (regref m 'reg 1))
      (assert (eq :trap reason))
      (assert (= 5 steps))
      (assert (= 9 (regref m 'reg 0)))
      (assert (= 14 (regref m 'reg 1)))
      (format t "~%All assertions passed.~%")))

  (format t "~%Disassembling (each statement renders back to its own mnemonic):~%")
  (let ((lines (disassemble-assembly assembly :machine 'sharedopcodefoo :labels nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "ld $0,#$A" (disassembly-line-text (first lines))))
    (assert (string= "ld $1,$0" (disassembly-line-text (second lines))))
    (assert (string= "addi $1,#$4" (disassembly-line-text (third lines))))
    (assert (string= "subi $0,#$1" (disassembly-line-text (fourth lines))))
    (format t "~%All four round-trip to their own real mnemonic and mode.~%")))
