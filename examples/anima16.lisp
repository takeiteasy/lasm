;;;; examples/anima16.lisp
;;;;
;;;; Mode-selected word-field codes (with unconditional extra words) and
;;;; CHOICE-CASE semantics dispatch, together: a `one-of` hole's matched
;;;; alternative steers a word-encoded field's own code, can spend a trailing
;;;; word unconditionally (not just when a value doesn't fit, the way an
;;;; ordinary `:else` escape does), and steers *semantics* the same way, so
;;;; `[reg]` really dereferences while a bare `reg` reads the register's value
;;;; directly. This is the shape ANIMA-16-style operand tables need
;;;; throughout, where the same field-value range means a different
;;;; addressing form depending on syntax alone:
;;;;
;;;;   0x00-0x07  register            reg
;;;;   0x08-0x0F  [register]          [reg]
;;;;   0x1E       [absolute address]  (addr)  -- always spends a trailing word
;;;;   0x20-0x3F  short literal       #lit    -- value-selected, packs inline
;;;;   0x1F       ""                  #lit    -- :else escape when #lit doesn't fit
;;;;
;;;; LD below demonstrates the CHOICE-selected rows (register/indirect/
;;;; absolute -- syntax picks the field code, the absolute form always spends
;;;; a word regardless of the address's own value, and CHOICE-CASE in LD's
;;;; own semantics picks a genuinely different runtime effect per alternative:
;;;; read a register, dereference it, or load from an absolute address); LDI
;;;; demonstrates the value-selected rows (short literal inline, or an :else
;;;; escape). They are two separate instructions, not two variant groups of
;;;; one field, since one operand field's variants may not mix CHOICE-selected
;;;; and value-selected forms -- real ANIMA-16 packs every row above into one
;;;; field, which this restriction does not yet support; see
;;;; docs/instructions.md and the tracker for more.
;;;;
;;;; `[register]` and `(address)` are spelled with different bracketing
;;;; ("[...]" vs "(...)") purely so this example's syntax stays unambiguous
;;;; without symbolic register names (a separate follow-up, noted already in
;;;; examples/dcpu16.lisp's own header) -- real ANIMA-16 spells both as
;;;; "[...]" and tells them apart by whether the bracketed expression names a
;;;; register or an arbitrary address, which lasm cannot do yet either.
;;;;
;;;; Run with:  sbcl --script examples/anima16.lisp

(require :asdf)
;; #75 gave LASM its first dependency (trivial-high-precision-timer, itself
;; depending on CFFI on SBCL) -- both are Quicklisp libraries, so a bare
;; `sbcl --script` run (no ~/.sbclrc) needs Quicklisp bootstrapped explicitly
;; before ASDF can resolve them, same as docs/getting-started.md's install
;; instructions assume.
(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-setup)
      (load quicklisp-setup)
      (error "Quicklisp not found at ~A -- see docs/getting-started.md" quicklisp-setup)))
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

;;; Machine: PC (convention), a banked 16-bit REG register (8 elements,
;;; addressed by index 0-7 -- symbolic banked-register names are their own
;;; follow-up, same as examples/dcpu16.lisp's REG), and one word-addressed
;;; (:cell-width 16) memory element -- DCPU-16/ANIMA-16-shaped, same
;;; instruction-word layout as examples/dcpu16.lisp: a 6-bit `a` field, a
;;; 5-bit `b` field, a 5-bit `opcode` field, MSB-first.

(defmachine anima16foo
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

;; The three CHOICE-selected alternatives LD's `src` operand independently
;; picks between -- "(" an absolute address ")", "[" a register index "]",
;; or a bare register index. All three parse to a plain integer (ANIMA16FOO
;; has no symbolic register names, same simplification as
;; examples/dcpu16.lisp) -- what differs is only which field code and
;; whether a trailing word gets spent, per LD's own (choice ...) variants
;; below.
;;
;; Declaration order matters here, unlike ORTHOGONAL-FOO's OO-TWO
;; (examples/orthogonal.lisp): the parser's own expression grammar treats a
;; leading "(" as grouping (PARSE-EXPRESSION, parser.lisp), so A-REG's bare
;; `expr` hole would happily parse a whole "(256)" as the grouped expression
;; 256 and never let A-MEM's own "(" ... ")" literal pattern get a turn --
;; ONE-OF tries alternatives in declaration order and only backtracks when
;; the *rest* of the pattern fails to match (mode.lisp's own docstring), and
;; here A-SRC's ONE-OF is the last element of LD-MODE's pattern, so a fully
;; matched A-REG would have nothing left to fail on. A bare `expr`
;; alternative sharing a ONE-OF with a literal-guarded one must always be
;; declared last for exactly this reason.
(defmode a-mem "(" expr ")")
(defmode a-ind "[" expr "]")
(defmode a-reg expr)

;; LDI's short-literal operand -- ordinary value-selected syntax, kept as a
;; separate mode/instruction from LD's CHOICE-selected one per this file's
;; header.
(defmode a-lit "#" expr)

(defmode ld-mode expr "," (one-of a-mem a-ind a-reg))
(defmode ldi-mode expr "," "#" expr)

;; LD dst, src -- reg[dst] := SRC, where SRC means something different
;; depending on which of its three syntaxes was actually written: a bare
;; register index reads that register's own value; "[reg]" dereferences it
;; as a RAM address; "(addr)" loads directly from an absolute RAM address.
;; Field A's three variants each pack a different field code -- (choice
;; a-reg)/(choice a-ind) into a disjoint half of A's 0-15 sub-range, and
;; (choice a-mem) spends its own trailing word *unconditionally* -- unlike
;; SET's :else in examples/dcpu16.lisp, this doesn't depend on whether the
;; address value would have fit inline; a-mem always means "value follows in
;; its own word", by syntax alone. CHOICE-CASE in the semantics below reads
;; back exactly which of the three was matched and picks the matching
;; runtime effect.
(definstruction anima16foo ld
  (modes ld-mode)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (choice a-reg) inline :range (0 7) :bias #x00)
      (variant (choice a-ind) inline :range (0 7) :bias #x08)
      (variant (choice a-mem) (extra-word :escape #x1e))))
  (semantics
    (set! (reg dst)
      (choice-case src
        (a-reg (reg src))
        (a-ind (mref machine 'ram (reg src)))
        (a-mem (mref machine 'ram src))))))

;; LDI dst, #lit -- reg[dst] := lit. Ordinary value-selected field A: a small
;; literal (-1..30, biased +33) packs inline into 0x20-0x3F; anything wider
;; escapes to its own word at 0x1F. No (choice ...) here at all.
(definstruction anima16foo ldi
  (modes ldi-mode)
  (encoding
    (opcode 2)
    (operand dst :field b)
    (operand lit :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) lit)))

(definstruction anima16foo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(format t "~&Encoding, by matched CHOICE alternative (LD):~%")
(let ((reg-form (assembly-cells (assemble "ld 1, 0" :machine 'anima16foo)))
      (ind-form (assembly-cells (assemble "ld 1, [0]" :machine 'anima16foo)))
      (mem-form (assembly-cells (assemble "ld 1, (0)" :machine 'anima16foo))))
  (format t "  ld 1, 0     -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce reg-form 'list) (length reg-form))
  (format t "  ld 1, [0]   -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce ind-form 'list) (length ind-form))
  (format t "  ld 1, (0)   -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce mem-form 'list) (length mem-form))
  ;; Unlike examples/orthogonal.lisp's byte-encoded MOV, these three
  ;; *don't* encode identically -- the whole point of #104: syntax alone
  ;; steers the field code (register vs. indirect, disjoint halves of A's
  ;; 0-15 sub-range) and, for the absolute form, whether a trailing word is
  ;; spent at all.
  (assert (not (equalp reg-form ind-form)))
  (assert (not (equalp reg-form mem-form)))
  (assert (= 1 (length reg-form)))    ; register: packs inline, one word
  (assert (= 1 (length ind-form)))    ; indirect: packs inline too, one word
  (assert (= 2 (length mem-form)))    ; absolute: unconditional trailing word
  (format t "~%All three assemble to distinct encodings, as expected.~%"))

(format t "~%Encoding, by value (LDI):~%")
(let ((inline-form (assembly-cells (assemble "ldi 1, #5" :machine 'anima16foo)))
      (escape-form (assembly-cells (assemble "ldi 1, #1000" :machine 'anima16foo))))
  (format t "  ldi 1, #5     -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce inline-form 'list) (length inline-form))
  (format t "  ldi 1, #1000  -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce escape-form 'list) (length escape-form))
  (assert (= 1 (length inline-form)))
  (assert (= 2 (length escape-form))))

(defparameter *source*
  "ldi 0, #100    ; reg0 = 100 -- base address the indirect/absolute forms below read through
ld 1, 0        ; reg1 = reg[0]      = 100        CHOICE a-reg -- reads the register directly
ld 2, [0]      ; reg2 = mem[reg[0]] = mem[100]   CHOICE a-ind -- dereferences it
ld 3, (256)    ; reg3 = mem[256]                 CHOICE a-mem -- absolute load
ldi 4, #5      ; reg4 = 5, value-selected inline
ldi 5, #1000   ; reg5 = 1000, value-selected :else escape
hlt")

(format t "~&~%Source:~%~A~2%" *source*)

(format t "Assembling and running:~%")
(let ((assembly (assemble *source* :machine 'anima16foo)))
  (format t "  cells: ~D~%" (length (assembly-cells assembly)))
  (let ((m (make-machine 'anima16foo)))
    (load-program m assembly)
    ;; Preload the two RAM cells LD's indirect and absolute forms read from,
    ;; each a different value from the other and from reg0's own 100 -- so
    ;; the three forms' three genuinely different runtime effects (register
    ;; read, dereference, absolute load) show up as three different results
    ;; below, not just three different encodings.
    (setf (mref m 'ram 100) 222)
    (setf (mref m 'ram 256) 333)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  reg1=~D reg2=~D reg3=~D reg4=~D reg5=~D~%"
              (regref m 'reg 1) (regref m 'reg 2) (regref m 'reg 3)
              (regref m 'reg 4) (regref m 'reg 5))
      (assert (eq :trap reason))
      (assert (= 7 steps))
      (assert (= 100 (regref m 'reg 1)))  ; a-reg: the register's own value
      (assert (= 222 (regref m 'reg 2)))  ; a-ind: mem[reg[0]] -- dereferenced
      (assert (= 333 (regref m 'reg 3)))  ; a-mem: mem[256] -- absolute load
      (assert (= 5 (regref m 'reg 4)))
      (assert (= 1000 (regref m 'reg 5)))
      (format t "~%All assertions passed -- the three LD forms produced three ~
different results for the same written value, not just three different ~
encodings.~%")))

  ;; Decode/disassemble round-trip (#117): LD's matched CHOICE alternative
  ;; comes back from DECODE-INSTRUCTION-AT (decoder.lisp) as each hole's own
  ;; WORD-FIELD-CHOICE, so the disassembler renders the real syntax that was
  ;; written -- "[$0]" / "($100)", not always the first alternative's own
  ;; pattern the way a byte-encoded machine's ONE-OF still must (see
  ;; examples/orthogonal.lisp and docs/disassembler.md).
  (format t "~%Disassembling (#117 -- the real alternative comes back):~%")
  (let ((lines (disassemble-assembly assembly :machine 'anima16foo :labels nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "ld $1,$0" (disassembly-line-text (second lines))))
    (assert (string= "ld $2,[$0]" (disassembly-line-text (third lines))))
    (assert (string= "ld $3,($100)" (disassembly-line-text (fourth lines))))
    (format t "~%All three LD forms round-trip to their own real syntax.~%")))
