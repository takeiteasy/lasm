;;;; examples/orthogonal.lisp
;;;;
;;;; Orthogonal per-operand addressing modes (see the tracker): each operand
;;;; hole of MOV below picks its own addressing-mode syntax independently of
;;;; the other, via a `(one-of mode...)` element inside a `defmode` pattern
;;;; (mode.lisp) -- the shape DCPU-16/ANIMA-16-style instruction sets need
;;;; throughout their operand tables (see examples/dcpu16.lisp's own header
;;;; for the workaround this replaces: a separate mnemonic per combination).
;;;;
;;;; Simplification (see the follow-up ticket this notes): declaring a
;;;; `one-of` gives each hole its own *syntax* choice, matched at
;;;; assemble time -- but nothing downstream of the match yet lets that
;;;; choice steer the *encoding*. `MOV`'s ONE-OF-TWO mode below lets each
;;;; operand be written as a bare register index, a bracketed `[reg]`
;;;; indirection, or a `#imm`-looking literal, but every one of those
;;;; alternatives still just parses to a plain integer and encodes into the
;;;; same one-byte field: `mov 1, 0`, `mov 1, [0]`, and `mov 1, #0` all
;;;; assemble identically (this file asserts that explicitly, below), and
;;;; MOV's semantics always reads its second operand as a register index
;;;; regardless of which syntax picked it. Giving each `one-of` alternative
;;;; its own field code (so `[reg]` really means "read through this
;;;; register" and `#imm` really means "this literal value") needs
;;;; mode-selected field codes and unconditional extra words -- a follow-up
;;;; ticket -- and, for `[reg+offset]`-shaped forms, symbolic register names
;;;; (another follow-up) so the assembler can tell a register apart from an
;;;; arbitrary expression inside the brackets.
;;;;
;;;; ORTHOGONAL-FOO has no symbolic register names yet (that follow-up
;;;; ticket again) -- operands below are plain register indices (0-3), the
;;;; same simplification examples/dcpu16.lisp's `set 0, 5` makes.
;;;;
;;;; Run with:  sbcl --script examples/orthogonal.lisp

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

(defmachine orthogonal-foo
  (register pc :width 8)
  (register reg :width 8 :count 4)
  (memory ram :width 8 :addr-width 8))

;; The three per-hole alternatives MOV's operands can independently pick
;; between -- a bare register index, "[" a register index "]", or "#" a
;; register index. (All three parse the same *kind* of value here -- a
;; register index -- only because ORTHOGONAL-FOO has nothing richer to
;; index with yet; nothing about ONE-OF itself requires that.)
(defmode oo-reg expr)
(defmode oo-ind "[" expr "]")
(defmode oo-imm "#" expr)
(defmode oo-two (one-of oo-reg oo-ind oo-imm) "," (one-of oo-reg oo-ind oo-imm))

;; SET dst, #imm -- seeds a register with a literal value so MOV below has
;; something to move. An ordinary (non-ONE-OF) two-hole mode, unrelated to
;; the feature this example demonstrates.
(defmode set-mode expr "," "#" expr)

(definstruction orthogonal-foo set
  (modes set-mode)
  (encoding (opcode 1) (operand dst :width 1) (operand val :width 1))
  (semantics (set! (reg dst) val)))

;; MOV dst, src -- reg[dst] = reg[src]. Both operands use OO-TWO, so each
;; independently accepts any of OO-REG/OO-IND/OO-IMM's syntax; see this
;; file's header for what that does and does not yet mean for encoding.
(definstruction orthogonal-foo mov
  (modes oo-two)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (reg dst) (reg src))))

(definstruction orthogonal-foo hlt
  (encoding (opcode 0))
  (semantics (trap :halt)))

(defparameter *source*
  "set 0, #5    ; reg0 = 5
mov 1, 0     ; reg1 = reg0 -- src via OO-REG (bare register index)
mov 2, [0]   ; reg2 = reg0 -- src via OO-IND ([reg]); see header: not yet
             ; distinguished from OO-REG in the encoding
mov 3, #0    ; reg3 = reg0 -- src via OO-IMM (#reg); same simplification
hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'orthogonal-foo)))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-cells assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'orthogonal-foo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  reg1 = ~D, reg2 = ~D, reg3 = ~D (all expected 5)~%"
              (regref m 'reg 1) (regref m 'reg 2) (regref m 'reg 3))
      (assert (eq :trap reason))
      (assert (= 5 steps))
      (assert (= 5 (regref m 'reg 1)))
      (assert (= 5 (regref m 'reg 2)))
      (assert (= 5 (regref m 'reg 3)))
      (format t "~%All assertions passed.~%"))))

;; The simplification, made explicit: three syntactically distinct MOV
;; operands -- a bare register, a bracketed indirection, and a "#" literal
;; -- currently assemble to the identical bytes. This is what a follow-up
;; ticket's mode-selected field codes will change.
(format t "~%Confirming today's simplification (see header):~%")
(let ((bare (assembly-cells (assemble "mov 1, 0" :machine 'orthogonal-foo)))
      (indirect (assembly-cells (assemble "mov 1, [0]" :machine 'orthogonal-foo)))
      (immediate (assembly-cells (assemble "mov 1, #0" :machine 'orthogonal-foo))))
  (format t "  mov 1, 0    -> ~{~2,'0X~^ ~}~%" (coerce bare 'list))
  (format t "  mov 1, [0]  -> ~{~2,'0X~^ ~}~%" (coerce indirect 'list))
  (format t "  mov 1, #0   -> ~{~2,'0X~^ ~}~%" (coerce immediate 'list))
  (assert (equalp bare indirect))
  (assert (equalp bare immediate))
  (format t "~%All three encode identically, as documented above.~%"))
