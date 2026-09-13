;;;; examples/word.lisp
;;;;
;;;; #20 (M4): a machine whose whole instruction is one fixed-width word
;;;; split into bit fields, rather than an opcode byte plus fixed-width
;;;; operand bytes -- the DCPU-16-shaped case LASM-plan.md sec. 3.8 sketches.
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
      (variant :else (extra-word :escape #x3ff))))
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

;; SETA's operand (5) fits inline; SETB's (1000) needs its own extra word --
;; one program exercising both forms of the same instruction.
(defparameter *source*
  "seta #5          ; A = 5, packs inline into SRC
setb #1000       ; B = 1000, does not fit -- SRC escapes, value in its own word
add              ; A = A + B
hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'wordfoo)))
  (format t "  bytes:  ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  length: ~D bytes (2 each for SETA/ADD/HLT's one word, 4 for SETB's ~
extra word)~%"
          (length (assembly-cells assembly)))
  (assert (= 10 (length (assembly-cells assembly))))

  (format t "~%Running:~%")
  (let ((m (make-machine 'wordfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = ~D (expected 1005)~%" (sref m 'a))
      (assert (eq :trap reason))
      (assert (= 4 steps))
      (assert (= 1005 (sref m 'a)))
      (format t "~%All assertions passed.~%"))))
