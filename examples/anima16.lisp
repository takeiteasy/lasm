;;;; examples/anima16.lisp
;;;;
;;;; Mode-selected word-field codes (with unconditional extra words),
;;;; value-selected ones, and CHOICE-CASE semantics dispatch, all sharing one
;;;; operand field: a `one-of` hole's matched alternative steers a
;;;; word-encoded field's own code when it names a `(choice ...)` variant,
;;;; can spend a trailing word unconditionally (not just when a value
;;;; doesn't fit, the way an ordinary `:else` escape does), and steers
;;;; *semantics* the same way, so `[reg]` really dereferences while a bare
;;;; `reg` reads the register's value directly -- and the alternative with no
;;;; `(choice ...)` variant of its own (#lit) still packs and dispatches
;;;; correctly, selected by whichever alternative every CHOICE-selected
;;;; variant here leaves unclaimed. This is the exact shape real ANIMA-16's
;;;; operand table needs, where the same field packs every one of these
;;;; rows, depending on syntax alone which applies:
;;;;
;;;;   0x00-0x07  register            reg
;;;;   0x08-0x0F  [register]          [reg]
;;;;   0x1E       [absolute address]  (addr)  -- always spends a trailing word
;;;;   0x20-0x3F  short literal       #lit    -- value-selected, packs inline
;;;;   0x1F       ""                  #lit    -- :else escape when #lit doesn't fit
;;;;
;;;; LD below demonstrates all four rows on one field: register/indirect/
;;;; absolute are CHOICE-selected (syntax picks the field code, and the
;;;; absolute form always spends a word regardless of the address's own
;;;; value); short-literal is value-selected (packs inline, or escapes to its
;;;; own word, purely by whether the literal fits) -- yet still dispatches
;;;; through the same CHOICE-CASE as the other three, since #118 stamps it
;;;; with A-LIT, the one ONE-OF alternative none of the other three
;;;; (choice ...) variants claims.
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
;; below -- and A-LIT's value-selected rows share the same field, chosen not
;; by a (choice ...) of its own but by #118's stamping: A-LIT is the one
;; ONE-OF alternative none of A-REG/A-IND/A-MEM's own (choice ...) variants
;; claims.
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
;; declared last for exactly this reason -- A-MEM, A-IND, and A-LIT are all
;; guarded by their own leading literal ("(", "[", "#"), so only their order
;; relative to A-REG matters, not to each other.
(defmode a-mem "(" expr ")")
(defmode a-ind "[" expr "]")
(defmode a-lit "#" expr)
(defmode a-reg expr)

(defmode ld-mode expr "," (one-of a-mem a-ind a-lit a-reg))

;; LD dst, src -- reg[dst] := SRC, where SRC means something different
;; depending on which of its four syntaxes was actually written: a bare
;; register index reads that register's own value; "[reg]" dereferences it
;; as a RAM address; "(addr)" loads directly from an absolute RAM address;
;; "#lit" loads the literal itself. Field A's five variants pack into one
;; field, #118's whole point: (choice a-reg)/(choice a-ind) into a disjoint
;; half of A's 0-15 sub-range; (choice a-mem) spends its own trailing word
;; *unconditionally* -- unlike SET's :else in examples/dcpu16.lisp, this
;; doesn't depend on whether the address value would have fit inline; a-mem
;; always means "value follows in its own word", by syntax alone -- and
;; A-LIT's two value-selected variants (packs inline 0x20-0x3F, or escapes to
;; 0x1F when it doesn't fit) are picked by syntax the very same way, despite
;; declaring no (choice ...) of their own: #118 stamps them with A-LIT, since
;; it is the one alternative the other three variants leave unclaimed.
;; CHOICE-CASE in the semantics below reads back exactly which of the four
;; was matched and picks the matching runtime effect.
(definstruction anima16foo ld
  (modes ld-mode)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (choice a-reg) inline :range (0 7) :bias #x00)
      (variant (choice a-ind) inline :range (0 7) :bias #x08)
      (variant (choice a-mem) (extra-word :escape #x1e))
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics
    (set! (reg dst)
      (choice-case src
        (a-reg (reg src))
        (a-ind (mref machine 'ram (reg src)))
        (a-mem (mref machine 'ram src))
        (a-lit src)))))

(definstruction anima16foo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(format t "~&Encoding, by matched alternative -- CHOICE-selected and ~
value-selected sharing one field (LD):~%")
(let ((reg-form (assembly-cells (assemble "ld 1, 0" :machine 'anima16foo)))
      (ind-form (assembly-cells (assemble "ld 1, [0]" :machine 'anima16foo)))
      (mem-form (assembly-cells (assemble "ld 1, (0)" :machine 'anima16foo)))
      (lit-form (assembly-cells (assemble "ld 1, #5" :machine 'anima16foo)))
      (lit-escape-form (assembly-cells (assemble "ld 1, #1000" :machine 'anima16foo))))
  (format t "  ld 1, 0     -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce reg-form 'list) (length reg-form))
  (format t "  ld 1, [0]   -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce ind-form 'list) (length ind-form))
  (format t "  ld 1, (0)   -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce mem-form 'list) (length mem-form))
  (format t "  ld 1, #5    -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce lit-form 'list) (length lit-form))
  (format t "  ld 1, #1000 -> ~{~4,'0X~^ ~}  (~D cell~:P)~%"
          (coerce lit-escape-form 'list) (length lit-escape-form))
  ;; Unlike examples/orthogonal.lisp's byte-encoded MOV, these *don't* encode
  ;; identically -- the whole point of #104/#118: syntax alone steers the
  ;; field code (register vs. indirect, disjoint halves of A's 0-15
  ;; sub-range; a short literal its own 0x1F/0x20-0x3F sub-range) and, for
  ;; the absolute and over-wide-literal forms, whether a trailing word is
  ;; spent at all.
  (assert (not (equalp reg-form ind-form)))
  (assert (not (equalp reg-form mem-form)))
  (assert (not (equalp reg-form lit-form)))
  (assert (= 1 (length reg-form)))         ; register: packs inline, one word
  (assert (= 1 (length ind-form)))         ; indirect: packs inline too, one word
  (assert (= 2 (length mem-form)))         ; absolute: unconditional trailing word
  (assert (= 1 (length lit-form)))         ; short literal: packs inline
  (assert (= 2 (length lit-escape-form)))  ; wide literal: :else escape
  (format t "~%All four forms assemble to distinct encodings, as expected.~%"))

(defparameter *source*
  "ld 0, #100    ; reg0 = 100 -- base address the indirect/absolute forms below read through, A-LIT value-selected inline
ld 1, 0        ; reg1 = reg[0]      = 100        CHOICE a-reg -- reads the register directly
ld 2, [0]      ; reg2 = mem[reg[0]] = mem[100]   CHOICE a-ind -- dereferences it
ld 3, (256)    ; reg3 = mem[256]                 CHOICE a-mem -- absolute load
ld 4, #5       ; reg4 = 5, A-LIT value-selected inline
ld 5, #1000    ; reg5 = 1000, A-LIT value-selected :else escape
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
      (assert (= 5 (regref m 'reg 4)))    ; a-lit: value-selected inline
      (assert (= 1000 (regref m 'reg 5))) ; a-lit: value-selected :else escape
      (format t "~%All assertions passed -- the five LD forms produced five ~
different results for the same written value, not just five different ~
encodings.~%")))

  ;; Decode/disassemble round-trip (#117, extended by #118 to the
  ;; value-selected row): LD's matched alternative comes back from
  ;; DECODE-INSTRUCTION-AT (decoder.lisp) as each hole's own WORD-FIELD-
  ;; CHOICE -- CHOICE-selected or, since #118 stamps A-LIT's own
  ;; value-selected variants too, that one just the same -- so the
  ;; disassembler renders the real syntax that was written for every one of
  ;; the four forms, "[$0]" / "($100)" / "#$5" included, not always the
  ;; first alternative's own pattern the way a byte-encoded machine's ONE-OF
  ;; still must (see examples/orthogonal.lisp and docs/disassembler.md).
  (format t "~%Disassembling (#117/#118 -- the real alternative comes back, ~
CHOICE-selected or value-selected):~%")
  (let ((lines (disassemble-assembly assembly :machine 'anima16foo :labels nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "ld $1,$0" (disassembly-line-text (second lines))))
    (assert (string= "ld $2,[$0]" (disassembly-line-text (third lines))))
    (assert (string= "ld $3,($100)" (disassembly-line-text (fourth lines))))
    (assert (string= "ld $4,#$5" (disassembly-line-text (fifth lines))))
    (assert (string= "ld $5,#$3E8" (disassembly-line-text (sixth lines))))
    (format t "~%All five LD forms round-trip to their own real syntax, ~
CHOICE-selected and value-selected rows alike.~%")))
