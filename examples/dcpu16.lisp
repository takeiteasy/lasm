;;;; examples/dcpu16.lisp
;;;;
;;;; #55 (M4): a DCPU-16-shaped machine, the second M4 validation case --
;;;; combining word-addressed memory (#53's
;;;; :cell-width) with bitfield/variant instruction-word encoding (#20) for
;;;; the first time in one machine. DCPU16FOO's instruction word is
;;;; DCPU-16's real layout, MSB-first: a 6-bit `a` field, a 5-bit `b` field,
;;;; a 5-bit `opcode` field. `a`'s variant table is DCPU-16's real one too --
;;;; -1..30 biased by 33 packs into 0x20..0x3f inline, anything else escapes
;;;; to its own following word (escape value 0x1f) -- see examples/word.lisp
;;;; for the mechanism itself on a byte-addressed machine, and
;;;; examples/wordaddr.lisp for word-addressed memory on its own.
;;;;
;;;; Simplification (see the follow-up ticket this notes): real DCPU-16
;;;; overloads `a`'s low values (0x00-0x07) as register reads and its high
;;;; ones (0x20-0x3f) as small literals -- one field, two different decode
;;;; interpretations depending on value range. The `variant` mechanism here
;;;; selects an *encoding*, not a decode interpretation, so it can't express
;;;; that overload. DCPU16FOO's `a` field is a literal-or-escape only;
;;;; register-to-register motion goes through a separate ADDR instruction
;;;; that puts a plain register index in both fields instead.
;;;;
;;;; BRA below (#62, M4) is a PC-relative branch -- its operand syntax names
;;;; an absolute target, same as SET/ADD/STO above, but what packs into
;;;; field A is the signed offset from the address of the *next*
;;;; instruction, reusing that same field's inline/escape variant shape (a
;;;; short branch packs inline; a far one escapes to its own word, exactly
;;;; like a large literal does). Since DCPU16FOO is word-addressed
;;;; (:cell-width 16), that offset counts 16-bit words -- the same unit its
;;;; own addresses already do, so there is no separate byte/word distinction
;;;; to trip over here the way there would be on a byte-addressed
;;;; word-encoded machine (see examples/word.lisp).
;;;;
;;;; Run with:  sbcl --script examples/dcpu16.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: PC (convention), a banked 16-bit REG register (8 elements --
;;; DCPU-16's A B C X Y Z I J, addressed here by index 0-7 rather than by
;;; name -- see the follow-up ticket on symbolic banked-register names), and
;;; one word-addressed (:cell-width 16) memory element sized to DCPU-16's
;;; own 16-bit address space.

(defmachine dcpu16foo
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode rr expr "," expr)

;; SET dst, src -- dst is always a plain register index (field B, full
;; 5-bit range, no variant needed). src packs a small literal inline into
;; field A or escapes to its own word for a larger one.
(definstruction dcpu16foo set
  (modes rr)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

;; ADD dst, src -- reg[dst] += src (a literal, same field-A variant as SET).
(definstruction dcpu16foo add
  (modes rr)
  (encoding
    (opcode 2)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) src) 16))))

;; ADDR dst, srcreg -- register-to-register add: reg[dst] += reg[srcreg].
;; Both fields are plain register indices (no variant on either), the
;; workaround this example uses for what real DCPU-16 folds into A's own
;; value table (see the header comment).
(definstruction dcpu16foo addr
  (modes rr)
  (encoding
    (opcode 3)
    (operand dst :field b)
    (operand srcreg :field a))
  (semantics (set! (reg dst) (wrap-value (+ (reg dst) (reg srcreg)) 16))))

;; STO addr, dst -- RAM[addr] = reg[dst]. addr reuses SET/ADD's field-A
;; variant (a small address packs inline, same as any other field-A value;
;; selection is value-based, not by what the field happens to mean).
(definstruction dcpu16foo sto
  (modes rr)
  (encoding
    (opcode 4)
    (operand addr :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f)))
    (operand dst :field b))
  (semantics (setf (mref machine 'ram addr) (reg dst))))

(definstruction dcpu16foo hlt
  (encoding (opcode 5))
  (semantics (trap :halt)))

;; BRA target -- PC-relative branch (#62): target is an absolute address in
;; source syntax, same as any other DCPU16FOO operand, but the assembler
;; folds it to the signed offset from the *next* instruction's own address
;; before packing it into field A. Unlike SET/ADD/STO's own field-A table
;; above, BRA's own variant is bias 0, not 33 -- :BIAS is what lets an
;; *unsigned* field hold a small negative literal (DCPU-16's own -1..30);
;; a RELATIVE hole is already signed on its own account (its MODE's
;; :RELATIVE T implies :SIGNED T), so BRA's inline range is the field's own
;; signed bound directly, -30..30, with the one remaining raw value (31)
;; free to serve as the extra-word escape.
(definstruction dcpu16foo bra
  (modes relative)
  (encoding
    (opcode 6)
    (operand offset :field a
      (variant (range -30 30) inline :bias 0)
      (variant :else (extra-word :escape 31))))
  (semantics (set! pc (wrap-value (+ pc offset) 16))))

;; SET 1, 1000 exercises the extra-word path (1000 is far outside -1..30);
;; BRA below jumps clean over a dead ADD -- proving it actually branches,
;; not merely encodes -- and, packing its own small offset inline, shows
;; the same field-A escape mechanism serving a PC-relative use as it does
;; SET/ADD's literal one, just over its own signed range.
(defparameter *source*
  "set 0, 5        ; reg0 = 5, packs inline into A
set 1, 1000      ; reg1 = 1000, does not fit -- A escapes, value in its own word
addr 0, 1        ; reg0 = reg0 + reg1 = 1005
sto result, 0    ; RAM[result] = reg0
bra skip         ; PC-relative jump over the dead ADD below
add 0, 1         ; dead code -- skipped by BRA; reaching it would corrupt reg0
skip: hlt
result: .byte 0  ; one 16-bit cell -- .word would reserve two (#53)")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'dcpu16foo)))
  (format t "  cells:      ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  cell-width: ~D bits~%" (assembly-cell-width assembly))
  (format t "  length:     ~D cells (1 each for SET reg0/ADDR/STO/BRA/ADD(dead)/HLT/RESULT, ~
2 for SET reg1's extra word)~%"
          (length (assembly-cells assembly)))
  (assert (= 16 (assembly-cell-width assembly)))
  (assert (= 9 (length (assembly-cells assembly))))
  (assert (equal '(unsigned-byte 16) (array-element-type (assembly-cells assembly))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'dcpu16foo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  reg0 = ~D (expected 1005 -- BRA must have skipped the dead ADD)~%"
              (regref m 'reg 0))
      (format t "  RAM[result] = ~D (expected 1005)~%"
              (mref m 'ram (gethash "result" (assembly-symbols assembly))))
      (assert (eq :trap reason))
      ;; SET reg0, SET reg1, ADDR, STO, BRA, HLT -- BRA's own jump skips the
      ;; dead ADD in between, so this is 6 steps, not 7.
      (assert (= 6 steps))
      (assert (= 1005 (regref m 'reg 0)))
      (assert (= 1005 (mref m 'ram (gethash "result" (assembly-symbols assembly)))))
      (format t "~%All assertions passed.~%"))))
