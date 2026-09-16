;;;; examples/subopcode.lisp
;;;;
;;;; Sub-opcode cells on a byte-encoded machine: examples/sharedopcode.lisp's
;;;; trick -- several mode- or mnemonic-distinguished descriptors sharing one
;;;; opcode, decode picking between them by inspecting some *operand* field's
;;;; raw bits -- only works on a word-encoded machine. A byte encoding's only
;;;; per-instruction bits are the opcode cell itself, so before #125 any
;;;; second descriptor at a byte-machine opcode was an unconditional
;;;; DEFINSTRUCTION-time error (#105), whether or not the two forms' operand
;;;; *syntax* was perfectly distinguishable.
;;;;
;;;; #125 gives a byte machine its own discriminator: an explicit
;;;; (opcode n :sub s) subclause reserves the cell right after the opcode as
;;;; a second, purely discriminating value. LDA below imagines SUBOPCODEFOO's
;;;; 8-bit opcode space as fully allocated elsewhere in a real ISA -- rather
;;;; than spend a second scarce opcode on its ABSOLUTE form, it shares
;;;; opcode #x10 with its IMMEDIATE form, distinguished only by :SUB. INCM/
;;;; DECM go further: two *different* mnemonics sharing opcode #x20, each
;;;; with its own :SUB.
;;;;
;;;; This is the byte-machine counterpart of examples/sharedopcode.lisp's
;;;; word-machine story; read that file first if this one is unfamiliar.
;;;;
;;;; Run with:  sbcl --script examples/subopcode.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: an ordinary 8-bit accumulator machine -- PC (convention), one
;;; 8-bit accumulator A, one byte-addressed memory element. No instruction-
;;; word clause, so this is byte-encoded: DEFINSTRUCTION's (opcode n) always
;;; meant "the whole first cell", with no operand field of its own to key
;;; decode off.

(defmachine subopcodefoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

;; LDA #n   -- a := n, the literal itself.
;; LDA $addr -- a := ram[addr].
;; One mnemonic, two (MODES ...) clauses, one opcode: without a :SUB on
;; each, this is #105's OPCODE-CONFLICT (:UNDECODABLE-BYTE-MACHINE) --
;; decode would have nothing but the opcode cell to go on, and that's
;; already spent telling LDA apart from every other mnemonic. With :SUB 0/1,
;; the cell right after the opcode does that job instead.
(definstruction subopcodefoo lda
  (modes
    (immediate (opcode #x10 :sub 0) (operand :mode)
      (semantics (set! a operand) (set-flags! (z (zero? a)))))
    (absolute (opcode #x10 :sub 1) (operand :mode)
      (semantics (set! a (mref machine 'ram operand)) (set-flags! (z (zero? a)))))))

;; INCM $addr -- ram[addr] += 1.  DECM $addr -- ram[addr] -= 1. Two distinct
;; mnemonics sharing opcode #x20, told apart by :SUB the same way LDA's two
;; modes are.
(definstruction subopcodefoo incm
  (modes absolute)
  (encoding (opcode #x20 :sub 0) (operand :mode))
  (semantics (setf (mref machine 'ram operand) (wrap-value (1+ (mref machine 'ram operand)) 8))))

(definstruction subopcodefoo decm
  (modes absolute)
  (encoding (opcode #x20 :sub 1) (operand :mode))
  (semantics (setf (mref machine 'ram operand) (wrap-value (1- (mref machine 'ram operand)) 8))))

(definstruction subopcodefoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(format t "~&LDA's two mode-distinguished forms, sharing opcode #x10:~%")
(let ((imm-form (assembly-cells (assemble "lda #5" :machine 'subopcodefoo)))
      (abs-form (assembly-cells (assemble "lda $2000" :machine 'subopcodefoo))))
  (format t "  lda #5     -> ~{~2,'0X~^ ~}~%" (coerce imm-form 'list))
  (format t "  lda $2000  -> ~{~2,'0X~^ ~}~%" (coerce abs-form 'list))
  (assert (= 3 (length imm-form)))    ; opcode + sub + 1-cell literal
  (assert (= 4 (length abs-form)))    ; opcode + sub + 2-cell address
  (dolist (case (list (cons imm-form 'immediate) (cons abs-form 'absolute)))
    (multiple-value-bind (descriptor) (decode-instruction-at (vector-cell-reader (car case)) 0 'subopcodefoo)
      (assert (not (eq :decode-failure descriptor)))
      (assert (string= "LDA" (instruction-descriptor-name descriptor)))
      (assert (eq (cdr case) (mode-descriptor-name (instruction-descriptor-mode descriptor))))))
  (format t "~%Both forms decode back to LDA, each via its own mode -- neither ~
:DECODE-FAILURE nor the other's mode.~%"))

(format t "~%INCM/DECM, two mnemonics sharing opcode #x20:~%")
(let ((incm-form (assembly-cells (assemble "incm $2000" :machine 'subopcodefoo)))
      (decm-form (assembly-cells (assemble "decm $2000" :machine 'subopcodefoo))))
  (format t "  incm $2000  -> ~{~2,'0X~^ ~}~%" (coerce incm-form 'list))
  (format t "  decm $2000  -> ~{~2,'0X~^ ~}~%" (coerce decm-form 'list))
  (assert (not (equalp incm-form decm-form)))
  (dolist (case (list (cons incm-form "INCM") (cons decm-form "DECM")))
    (multiple-value-bind (descriptor) (decode-instruction-at (vector-cell-reader (car case)) 0 'subopcodefoo)
      (assert (not (eq :decode-failure descriptor)))
      (assert (string= (cdr case) (instruction-descriptor-name descriptor)))))
  (format t "~%Both decode back to their own mnemonic.~%"))

;; A sub-opcode value matching neither co-tenant is a decode failure, exactly
;; like an unregistered opcode -- there is no third LDA mode to fall back to.
(format t "~%An unmatched sub-opcode cell is a decode failure, not a silent fallback:~%")
(let ((bogus (vector-cell-reader (vector #x10 99 0))))
  (assert (eq :decode-failure (decode-instruction-at bogus 0 'subopcodefoo)))
  (format t "  [#x10 99 0] -> :DECODE-FAILURE (no LDA sub-opcode is 99)~%"))

(defparameter *source*
  "lda #10      ; a = 10                       LDA, immediate mode
lda $2000    ; a = ram[$2000]                LDA, absolute mode
incm $2000   ; ram[$2000] += 1
decm $2000   ; ram[$2000] -= 1 (back to original)
hlt")

(format t "~&~%Source:~%~A~2%" *source*)

(format t "Assembling and running:~%")
(let ((assembly (assemble *source* :machine 'subopcodefoo)))
  (let ((m (make-machine 'subopcodefoo)))
    (setf (mref m 'ram #x2000) 41)
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  a=~D ram[$2000]=~D~%" (sref m 'a) (mref m 'ram #x2000))
      (assert (eq :trap reason))
      (assert (= 5 steps))
      (assert (= 41 (sref m 'a)))          ; second LDA re-reads ram[$2000]
      (assert (= 41 (mref m 'ram #x2000))) ; incm then decm nets to no change
      (format t "~%All assertions passed.~%")))

  (format t "~%Disassembling (each statement renders back to its own mnemonic/mode):~%")
  (let ((lines (disassemble-assembly assembly :machine 'subopcodefoo :labels nil :suffixes nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "lda #$A" (disassembly-line-text (first lines))))
    (assert (string= "lda $2000" (disassembly-line-text (second lines))))
    (assert (string= "incm $2000" (disassembly-line-text (third lines))))
    (assert (string= "decm $2000" (disassembly-line-text (fourth lines))))
    (format t "~%All four round-trip to their own real mnemonic and mode.~%")))
