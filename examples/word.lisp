;;;; examples/word.lisp
;;;;
;;;; #20 (M4): a machine whose whole instruction is one fixed-width word
;;;; split into bit fields, rather than an opcode byte plus fixed-width
;;;; operand bytes -- the DCPU-16-shaped case examples/dcpu16.lisp exercises.
;;;; WORDFOO's SET instruction packs a small operand value straight into its
;;;; 10-bit SRC field (biased so -1..30 fits an unsigned field); a value
;;;; outside that range instead writes a reserved escape value into SRC and
;;;; carries the real value in its own following word. Which form a given
;;;; `set` statement needs depends on its *operand's value*, not its syntax
;;;; -- something no fixed-width byte encoding (M1-M3) can express -- and the
;;;; assembler picks between them with the same relaxation loop it already
;;;; uses to pick an addressing-mode width (docs/assembler.md).
;;;;
;;;; This is intentionally small -- WORDFOO stays byte-addressed and keeps
;;;; its instruction word itself emitted as ordinary little-endian bytes;
;;;; see examples/dcpu16.lisp for a full DCPU-16-shaped machine combining
;;;; this mechanism with word-addressed memory.
;;;;
;;;; Run with:  sbcl --script examples/word.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: a 16-bit instruction word (4-bit OPCODE, 2-bit DST -- unused by
;;; any instruction below, kept only to show a word layout can carry more
;;; than the fields any one instruction happens to fill -- and a 10-bit SRC),
;;; two general registers, byte-addressed RAM, and the usual PC-is-a-plain-
;;; register convention.

(defmachine wordfoo
  (register pc :width 16)
  (register a :width 16)
  (register b :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 2)
    (field src 10)))

(defmode wimm "#" expr)
;; #63: a :SIGNED T immediate -- unlike WIMM above, SETC's negative values
;; below sign-extend on decode rather than relying on :BIAS to carry a
;; negative value through an otherwise-unsigned field.
(defmode wsimm "#" expr :signed t)

;; SETA/SETB load an immediate value into A/B. Both variants share one
;; opcode and mode -- DEFINSTRUCTION expands the (variant ...) clauses below
;; into two INSTRUCTION-DESCRIPTORs (one all-inline, one needing an extra
;; word), and %CHOOSE-VARIANT (assembler.lisp) picks between them per
;; statement exactly like it picks between two addressing-mode widths.
(definstruction wordfoo seta
  (modes wimm)
  (encoding
    (opcode 1)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff) :suffix "w")))
  (semantics (set! a operand)))

(definstruction wordfoo setb
  (modes wimm)
  (encoding
    (opcode 2)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff))))
  (semantics (set! b operand)))

(definstruction wordfoo add
  (encoding (opcode 3))
  (semantics (set! a (wrap-value (+ a b) 16))))

(definstruction wordfoo hlt
  (encoding (opcode 4))
  (semantics (trap :halt)))

;; SETC (#63): a :SIGNED T immediate over the same 10-bit SRC field, no
;; :BIAS at all -- its inline range (-16..15) packs as two's complement and
;; decodes back by sign-extension, and a magnitude that doesn't fit still
;; escapes to its own following word (also sign-extended on decode) exactly
;; like SETA/SETB's unsigned :ELSE fallback.
(definstruction wordfoo setc
  (modes wsimm)
  (encoding
    (opcode 5)
    (operand value :field src
      (variant (range -16 15) inline)
      ;; #x200 (512), not #x3ff -- a signed -16..15 range's raw footprint
      ;; wraps its negative half up to the field's own top (1008..1023,
      ;; #x3F0..#x3FF), so the escape marker SETA/SETB share (#x3ff) would
      ;; collide with it here; any value outside both raw chunks works.
      (variant :else (extra-word :escape #x200))))
  (semantics (set! a (wrap-value value 16))))

;; SETD (#135): the same unsigned inline/escape split as SETA/SETB, but its
;; escape's own trailing word is declared only 1 cell wide -- an 8-bit
;; immediate after the 16-bit instruction word, rather than another whole
;; instruction word. A value out of SETD's -1..30 inline range but within a
;; byte (0..255) now costs 3 bytes total, not 4.
(definstruction wordfoo setd
  (modes wimm)
  (encoding
    (opcode 6)
    (operand value :field src
      (variant (range -1 30) inline :bias 1)
      (variant :else (extra-word :escape #x3ff :cells 1))))
  (semantics (set! a operand)))

;; SETA's escape declares :SUFFIX "w", so source can force the extra word for
;; a value that would fit inline: "seta #w:7".
;; SETA's operand (5) fits inline; SETB's (1000) needs its own extra word --
;; one program exercising both forms of the same instruction. SETC then
;; exercises the same inline/extra-word split again, but signed: -5 fits its
;; field directly, -5000 does not. SETD's escape (200) fits its own 1-byte
;; extra word.
(defparameter *source*
  "seta #5          ; A = 5, packs inline into SRC
setb #1000       ; B = 1000, does not fit -- SRC escapes, value in its own word
add              ; A = A + B
setc #-5         ; A = -5, sign-extended from an inline two's-complement field
setc #-5000      ; A = -5000, sign-extended from SRC's own escaped extra word
setd #200        ; A = 200, escapes to SRC's own 1-byte extra word, not a full one
seta #w:7        ; A = 7, forced into SRC's extra word although it fits inline
hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'wordfoo)))
  (format t "  bytes:  ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  length: ~D bytes (2 each for SETA/ADD/HLT/SETC's one word, 4 for ~
SETB/SETC's extra-word forms, 3 for SETD's own 1-byte extra word, 4 for the forced SETA)~%"
          (length (assembly-cells assembly)))
  (assert (= 23 (length (assembly-cells assembly))))

  ;; A declared data region renders as .word lines: two 8-bit cells per word.
  (let* ((data (assemble (format nil "hlt~%.word $1234, $BEEF") :machine 'wordfoo))
         (texts (mapcar #'disassembly-line-text
                        (disassemble-assembly data :machine 'wordfoo :labels nil))))
    (format t "~%Data region disassembly: ~{~A~^ | ~}~%" texts)
    (assert (equal '("hlt" ".word $1234" ".word $BEEF") texts)))

  (format t "~%Running:~%")
  (let ((m (make-machine 'wordfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m :max-steps 3)
      (declare (ignore reason))
      (format t "  after SETA/SETB/ADD, A = ~D (expected 1005)~%" (sref m 'a))
      (assert (= 3 steps))
      (assert (= 1005 (sref m 'a))))
    (step-machine m)
    (format t "  after SETC #-5, A = ~D (expected 65531, i.e. -5 as 16-bit)~%" (sref m 'a))
    (assert (= 65531 (sref m 'a)))
    (assert (= -5 (signed-value (sref m 'a) 16)))
    (step-machine m)
    (format t "  after SETC #-5000, A = ~D, signed ~D (expected -5000)~%" (sref m 'a) (signed-value (sref m 'a) 16))
    (assert (= -5000 (signed-value (sref m 'a) 16)))
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D more step~:P~%" reason steps)
      (format t "  after SETA #w:7, A = ~D (expected 7)~%" (sref m 'a))
      (assert (eq :trap reason))
      (assert (= 3 steps))
      (assert (= 7 (sref m 'a)))
      (format t "~%All assertions passed.~%"))))
