;;;; examples/disasm.lisp
;;;;
;;;; #21 (M7): DISASSEMBLE-ASSEMBLY (disassembler.lisp), built on the pure
;;;; DECODE-INSTRUCTION-AT decoder (decoder.lisp) shared with the emulator's
;;;; STEP-MACHINE -- assemble a program, disassemble it back to source text,
;;;; and re-assemble that text to the same cells. First on an ordinary
;;;; byte-encoded machine (a small 6502-shaped one, like examples/modes.lisp),
;;;; then on a DCPU-16-shaped word-encoded machine (examples/dcpu16.lisp) to
;;;; show an instruction that spends an extra word, then (#143) the same
;;;; shape again with a #72 :names bank, to show a register-index operand
;;;; disassembling back to its own alias rather than a bare integer.
;;;;
;;;; Run with:  sbcl --script examples/disasm.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Part 1: a byte-encoded machine

(defmachine disasmfoo
  (register pc :width 16)
  (register x :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction disasmfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction disasmfoo lda
  (modes zero-page)
  (encoding (opcode #xA5) (operand :mode))
  (semantics (set! x (mref machine 'ram operand))))

(definstruction disasmfoo bra
  (modes relative)
  (encoding (opcode #x90) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction disasmfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *byte-source*
  "start:
ldx #10
lda $20
bra start
hlt")

(format t "~&Part 1: byte-encoded machine~2%Source:~%~A~2%" *byte-source*)

(let ((assembly (assemble *byte-source* :machine 'disasmfoo)))
  (format t "Assembled ~D cells.~2%" (length (assembly-cells assembly)))

  (format t "Disassembly listing:~%")
  (let ((lines (disassemble-assembly assembly :machine 'disasmfoo)))
    (print-disassembly lines)
    (assert (= 4 (length lines)))
    (assert (string= "start" (disassembly-line-label (first lines))))
    (assert (string= "ldx #$A" (disassembly-line-text (first lines))))
    (assert (string= "lda $20" (disassembly-line-text (second lines))))
    (assert (string= "bra start" (disassembly-line-text (third lines))))
    (assert (string= "hlt" (disassembly-line-text (fourth lines))))

    ;; Round trip: re-assemble the disassembled text (dropping labels, since
    ;; a fresh layout pass over final values can legitimately choose a
    ;; narrower encoding than one carried forward from a program with a
    ;; forward reference -- see disassembler.lisp's header comment) and
    ;; check it reproduces the same cells.
    (let* ((no-label-lines (disassemble-assembly assembly :machine 'disasmfoo :labels nil))
           (text (disassembly-text no-label-lines))
           (reassembled (assemble text :machine 'disasmfoo)))
      (format t "~%Re-assembled disassembly text:~%~A~%" text)
      (assert (equalp (assembly-cells assembly) (assembly-cells reassembled)))
      (format t "Round trip OK: re-assembled cells match the original.~%"))))

;;; Part 2: a DCPU-16-shaped word-encoded machine (examples/dcpu16.lisp)

(defmachine disasm-dcpu16
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode disasm-rr expr "," expr)

(definstruction disasm-dcpu16 set
  (modes disasm-rr)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction disasm-dcpu16 hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(defparameter *word-source*
  "set 0, 5        ; packs inline into field A
set 1, 1000      ; does not fit -- escapes to its own following word
hlt")

(format t "~2%Part 2: word-encoded (DCPU-16-shaped) machine~2%Source:~%~A~2%" *word-source*)

(let ((assembly (assemble *word-source* :machine 'disasm-dcpu16)))
  (format t "Assembled ~D cells.~2%" (length (assembly-cells assembly)))

  (format t "Disassembly listing:~%")
  (let ((lines (disassemble-assembly assembly :machine 'disasm-dcpu16 :labels nil)))
    (print-disassembly lines)
    (assert (= 3 (length lines)))
    (assert (= 1 (disassembly-line-size (first lines))))
    (assert (string= "set $0,$5" (disassembly-line-text (first lines))))
    (assert (= 2 (disassembly-line-size (second lines))))
    (assert (string= "set $1,$3E8" (disassembly-line-text (second lines))))
    (assert (string= "hlt" (disassembly-line-text (third lines))))

    (let* ((text (disassembly-text lines))
           (reassembled (assemble text :machine 'disasm-dcpu16)))
      (format t "~%Re-assembled disassembly text:~%~A~%" text)
      (assert (equalp (assembly-cells assembly) (assembly-cells reassembled)))
      (format t "Round trip OK: re-assembled cells match the original.~%"))))

;;; Part 3: #143 -- a register-index operand disassembles to its own alias

;; Same DCPU-16 shape as Part 2 and examples/dcpu16.lisp, but REG carries
;; #72's :names and both instructions' register holes carry #143's own
;; :register -- the ticket's own claim, disproved directly: disassembling
;; "addr a, b" must print "addr a,b", not "addr $0,$1".

(defmachine disasm-dcpu16-alias
  (register pc :width 16)
  (register reg :width 16 :names (a b c x y z i j))
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a-field 6)
    (field b-field 5)
    (field opcode 5)))

(definstruction disasm-dcpu16-alias set
  (modes disasm-rr)
  (encoding
    (opcode 1)
    (operand dst :field b-field :register reg)
    (operand src :field a-field
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction disasm-dcpu16-alias addr
  (modes disasm-rr)
  (encoding
    (opcode 2)
    (operand dst :field b-field :register reg)
    (operand srcreg :field a-field :register reg))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) (reg srcreg)) 16))))

(definstruction disasm-dcpu16-alias hlt
  (encoding (opcode 3))
  (semantics (trap :halt)))

(defparameter *alias-source*
  "set a, 5        ; reg[a] = 5, packs inline into field A-FIELD
addr a, b       ; reg[a] += reg[b], both plain register indices
hlt")

(format t "~2%Part 3: register-index operands render as #72 aliases (#143)~2%Source:~%~A~2%"
        *alias-source*)

(let ((assembly (assemble *alias-source* :machine 'disasm-dcpu16-alias)))
  (format t "Assembled ~D cells.~2%" (length (assembly-cells assembly)))

  (format t "Disassembly listing:~%")
  (let ((lines (disassemble-assembly assembly :machine 'disasm-dcpu16-alias :labels nil)))
    (print-disassembly lines)
    (assert (= 3 (length lines)))
    (assert (string= "set a,$5" (disassembly-line-text (first lines))))
    (assert (string= "addr a,b" (disassembly-line-text (second lines))))
    (assert (string= "hlt" (disassembly-line-text (third lines))))

    (let* ((text (disassembly-text lines))
           (reassembled (assemble text :machine 'disasm-dcpu16-alias)))
      (format t "~%Re-assembled disassembly text:~%~A~%" text)
      (assert (equalp (assembly-cells assembly) (assembly-cells reassembled)))
      (format t "Round trip OK: re-assembled cells match the original.~%"))))

(format t "~%All assertions passed.~%")
