;;;; examples/subchoice.lisp
;;;;
;;;; Hole-selected sub-opcodes on a byte-encoded machine: examples/
;;;; subopcode.lisp's sub-opcode cell -- several descriptors sharing one
;;;; opcode, decode picking between them by reading the cell right after it
;;;; -- is chosen per (modes ...) clause there, not per operand hole. A mode
;;;; whose own syntax already varies (a ONE-OF pattern element, mode.lisp,
;;;; #103) has no way to make *that* choice drive the sub-opcode cell.
;;;;
;;;; This is what lets a single addressing mode's own ONE-OF alternatives --
;;;; here, a bare register operand vs. a "[expr]" indirect one -- share one
;;;; opcode, discriminated purely by which alternative the operand's own
;;;; syntax matched: (operand NAME :width n (variant (choice m) (sub s))*).
;;;; Every alternative of the carrying hole must be claimed by exactly one
;;;; such variant -- there is no value-selected fallback the way a word-
;;;; encoded field has (#118), so partial coverage is a DEFINSTRUCTION-time
;;;; error.
;;;;
;;;; The record this leaves behind -- DECODE-INSTRUCTION-AT's own CHOICES,
;;;; sourced here from the matched descriptor's SUB-CHOICES -- is exactly
;;;; what CHOICE-CASE (#73) needs to dispatch semantics on which alternative
;;;; was actually written, and what the disassembler needs to render it back
;;;; correctly, both for the first time reachable on a byte-encoded machine.
;;;;
;;;; The same selector also lets two alternatives disagree on :SIGNED (LDB,
;;;; below) or on :WIDTH (LDW, below) -- decode reads each matched sibling's
;;;; own OPERAND-SIGNEDNESS/OPERAND-WIDTHS, precomputed once per expanded
;;;; descriptor rather than assumed constant for the whole mode.
;;;;
;;;; Run with:  sbcl --script examples/subchoice.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: an 8-bit accumulator machine, same shape as subopcode.lisp's.

(defmachine subchoicefoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

;; A single addressing mode whose one hole may be written either as a bare
;; register-style literal or as "[expr]" indirection -- syntactically
;; distinguished at parse time (#103's ONE-OF), but, before #126, with no way
;; for that distinction to reach decode on a byte-encoded machine at all.
(defmode sc-direct expr)
(defmode sc-indirect "[" expr "]")
(defmode sc-any (one-of sc-direct sc-indirect))

;; LDA n    -- a := n, the literal itself (direct).
;; LDA [n]  -- a := ram[n] (indirect).
;; One mnemonic, one opcode, one mode -- the sub-opcode cell is chosen by
;; which ONE-OF alternative the operand actually matched, not by a separate
;; (modes ...) clause per form the way subopcode.lisp's LDA needed.
(definstruction subchoicefoo lda
  (modes sc-any)
  (encoding (opcode #x10)
            (operand src :width 1
              (variant (choice sc-direct) (sub 0))
              (variant (choice sc-indirect) (sub 1))))
  (semantics (choice-case src
               (sc-direct (set! a src))
               (sc-indirect (set! a (mref machine 'ram src))))))

(definstruction subchoicefoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; #124/#127: per-hole :SIGNED on a ONE-OF alternative. SC-UVAL and SC-SVAL
;; disagree on signedness (unsigned bare literal vs. a "#"-prefixed signed
;; one) -- legal now that mode.lisp's %CHECK-ONE-OF-ELEMENTS! no longer
;; rejects :SIGNED inside ONE-OF, *and* actually decodable, since this hole
;; carries the same (variant (choice m) (sub s)) selector #126 gave the
;; direct/indirect hole above -- the decode-time record instruction.lisp's
;; %CHECK-BYTE-ONE-OF-SIGNED requires whenever a ONE-OF hole's alternatives
;; disagree on :SIGNED.
(defmode sc-uval expr)
(defmode sc-sval "#" expr :signed t)
(defmode sc-mixed (one-of sc-uval sc-sval))

(definstruction subchoicefoo ldb
  (modes sc-mixed)
  (encoding (opcode #x20)
            (operand val :width 1
              (variant (choice sc-uval) (sub 0))
              (variant (choice sc-sval) (sub 1))))
  (semantics (set! a val)))

(format t "~&LDA's two ONE-OF-matched forms, sharing opcode #x10 and one mode:~%")
(let ((direct-form (assembly-cells (assemble "lda 5" :machine 'subchoicefoo)))
      (indirect-form (assembly-cells (assemble "lda [5]" :machine 'subchoicefoo))))
  (format t "  lda 5    -> ~{~2,'0X~^ ~}~%" (coerce direct-form 'list))
  (format t "  lda [5]  -> ~{~2,'0X~^ ~}~%" (coerce indirect-form 'list))
  (assert (equalp #(#x10 #x00 #x05) direct-form))
  (assert (equalp #(#x10 #x01 #x05) indirect-form))
  (format t "~%Sub-opcode 0 for the direct form, 1 for the indirect one -- ~
the same opcode, told apart purely by which alternative the operand matched.~%")
  (dolist (case (list (cons direct-form 'sc-direct) (cons indirect-form 'sc-indirect)))
    (multiple-value-bind (descriptor values size choices)
        (decode-instruction-at (vector-cell-reader (car case)) 0 'subchoicefoo)
      (assert (not (eq :decode-failure descriptor)))
      (assert (string= "LDA" (instruction-descriptor-name descriptor)))
      (assert (= 3 size))
      (assert (equal (list 5) values))
      ;; #126: CHOICES is no longer unconditionally NIL on a byte-encoded
      ;; machine -- it carries the descriptor's own SUB-CHOICES, naming the
      ;; alternative this decode actually matched.
      (assert (eq (cdr case) (%matched-choice-name choices 0))))))

(format t "~%An unmatched sub-opcode cell is a decode failure:~%")
(let ((bogus (vector-cell-reader (vector #x10 99 5))))
  (assert (eq :decode-failure (decode-instruction-at bogus 0 'subchoicefoo)))
  (format t "  [#x10 99 5] -> :DECODE-FAILURE (no LDA sub-opcode is 99)~%"))

(defparameter *source*
  "lda 5
lda [7]
hlt")

(format t "~%CHOICE-CASE dispatch, now reachable on a byte-encoded machine (#122):~%")
(let ((assembly (assemble *source* :machine 'subchoicefoo)))
  (let ((m (make-machine 'subchoicefoo)))
    (setf (mref m 'ram 7) 99)
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (declare (ignore steps))
      (assert (eq :trap reason))
      ;; The second LDA overwrote A with RAM[7], proving CHOICE-CASE really
      ;; dispatched to the SC-INDIRECT clause, not SC-DIRECT's literal-5.
      (assert (= 99 (sref m 'a)))
      (format t "  lda 5 then lda [7]: a ends at ~D (ram[7]), not 7 -- ~
CHOICE-CASE took the indirect branch.~%" (sref m 'a)))))

(format t "~%Disassembling (renders each statement's own matched alternative, not always the first):~%")
(let ((assembly (assemble *source* :machine 'subchoicefoo)))
  (let ((lines (disassemble-assembly assembly :machine 'subchoicefoo :labels nil :suffixes nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "lda $5" (disassembly-line-text (first lines))))
    (assert (string= "lda [$7]" (disassembly-line-text (second lines))))
    (format t "~%Both round-trip to their own real syntax.~%")))

(format t "~%LDB's two ONE-OF-matched forms disagree on signedness (#124/#127):~%")
(let ((unsigned-form (assembly-cells (assemble "ldb 200" :machine 'subchoicefoo)))
      (signed-form (assembly-cells (assemble "ldb #-100" :machine 'subchoicefoo))))
  (format t "  ldb 200   -> ~{~2,'0X~^ ~}~%" (coerce unsigned-form 'list))
  (format t "  ldb #-100 -> ~{~2,'0X~^ ~}~%" (coerce signed-form 'list))
  (assert (equalp #(#x20 #x00 #xC8) unsigned-form))
  (assert (equalp #(#x20 #x01 #x9C) signed-form))
  (format t "~%Decoding reinterprets only the SC-SVAL-matched hole as signed:~%")
  (multiple-value-bind (descriptor values size choices) (decode-instruction-at (vector-cell-reader unsigned-form) 0 'subchoicefoo)
    (declare (ignore size))
    (assert (string= "LDB" (instruction-descriptor-name descriptor)))
    (assert (equal (list 200) values))
    (assert (eq 'sc-uval (%matched-choice-name choices 0))))
  (multiple-value-bind (descriptor values size choices) (decode-instruction-at (vector-cell-reader signed-form) 0 'subchoicefoo)
    (declare (ignore size))
    (assert (string= "LDB" (instruction-descriptor-name descriptor)))
    (assert (equal (list -100) values))
    (assert (eq 'sc-sval (%matched-choice-name choices 0))))
  (format t "  ldb 200   decodes back to 200 (SC-UVAL, unsigned)~%")
  (format t "  ldb #-100 decodes back to -100 (SC-SVAL, signed)~%"))

;; #129: per-hole :WIDTH on a ONE-OF alternative. SC-NARROW and SC-WIDE
;; disagree on width (a 1-byte bare literal vs. a "#"-prefixed 2-byte one) --
;; legal now that mode.lisp's %CHECK-ONE-OF-ELEMENTS! no longer rejects
;; :WIDTH inside ONE-OF either, and decodable via the same
;; (variant (choice m) (sub s)) selector #126 gave the direct/indirect hole
;; above. Unlike :SIGNED, a disagreeing :WIDTH changes each sibling
;; descriptor's own encoded size -- %LAYOUT's monotone-widening FLOOR
;; fixpoint (assembler.lisp) never has to widen across this, though, since
;; which sibling matches is decided purely by syntax (which alternative's
;; own tokens were written), not by a value that could still be provisional
;; mid-relaxation the way ZERO-PAGE/ABSOLUTE's shared-syntax widening is.
(defmode sc-narrow expr :width 1)
(defmode sc-wide "#" expr :width 2)
(defmode sc-widths (one-of sc-narrow sc-wide))

(definstruction subchoicefoo ldw
  (modes sc-widths)
  (encoding (opcode #x30)
            (operand val :mode
              (variant (choice sc-narrow) (sub 0))
              (variant (choice sc-wide) (sub 1))))
  (semantics (set! a val)))

(format t "~%LDW's two ONE-OF-matched forms disagree on width (#129):~%")
(let ((narrow-form (assembly-cells (assemble "ldw 200" :machine 'subchoicefoo)))
      (wide-form (assembly-cells (assemble "ldw #300" :machine 'subchoicefoo))))
  (format t "  ldw 200  -> ~{~2,'0X~^ ~}~%" (coerce narrow-form 'list))
  (format t "  ldw #300 -> ~{~2,'0X~^ ~}~%" (coerce wide-form 'list))
  (assert (equalp #(#x30 #x00 #xC8) narrow-form))
  (assert (equalp #(#x30 #x01 #x2C #x01) wide-form))
  (format t "~%Decoding reads each alternative's own operand width -- 1 byte for ~
SC-NARROW, 2 for SC-WIDE:~%")
  (multiple-value-bind (descriptor values size choices) (decode-instruction-at (vector-cell-reader narrow-form) 0 'subchoicefoo)
    (assert (string= "LDW" (instruction-descriptor-name descriptor)))
    (assert (equal (list 200) values))
    (assert (= 3 size))
    (assert (eq 'sc-narrow (%matched-choice-name choices 0))))
  (multiple-value-bind (descriptor values size choices) (decode-instruction-at (vector-cell-reader wide-form) 0 'subchoicefoo)
    (assert (string= "LDW" (instruction-descriptor-name descriptor)))
    (assert (equal (list 300) values))
    (assert (= 4 size))
    (assert (eq 'sc-wide (%matched-choice-name choices 0))))
  (format t "  ldw 200  decodes back to 200 (SC-NARROW, 3 bytes total)~%")
  (format t "  ldw #300 decodes back to 300 (SC-WIDE, 4 bytes total)~%"))

(format t "~%A following label's address accounts for each LDW's own chosen width, ~
even across a forward reference relaxation has to lay out first:~%")
(let* ((source "ldw 5
ldw #300
next: hlt")
       (assembly (assemble source :machine 'subchoicefoo)))
  (format t "  next: ~D (3 + 4 = 7 bytes before it)~%" (gethash "next" (assembly-symbols assembly)))
  (assert (= 7 (gethash "next" (assembly-symbols assembly))))
  (assert (equalp #(#x30 #x00 #x05 #x30 #x01 #x2C #x01 #x00) (assembly-cells assembly))))

(format t "~%All assertions passed.~%")
