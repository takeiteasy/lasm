;;;; examples/listing.lisp
;;;;
;;;; #25 (M7): the address<->statement mapping ASSEMBLE retains on ASSEMBLY
;;;; (assembler.lisp) -- ASSEMBLY-LISTING/ASSEMBLY-SOURCE -- rendered as a
;;;; conventional listing (LISTING-TEXT/PRINT-LISTING, listing.lisp) and
;;;; queried in both directions (LISTING-LINE-AT, LISTING-LINES-FOR-SOURCE-LINE).
;;;; First on an ordinary byte-encoded machine, including a macro
;;;; invocation (docs/listing.md covers what happens when a macro's body is
;;;; invoked more than once), then on a DCPU-16-shaped word-encoded machine
;;;; to show the cell hex field widening to match a 16-bit cell.
;;;;
;;;; Run with:  sbcl --script examples/listing.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Part 1: a byte-encoded machine, with a macro invocation

(defmachine listingfoo
  (register pc :width 16)
  (register x :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction listingfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction listingfoo lda
  (modes zero-page)
  (encoding (opcode #xA5) (operand :mode))
  (semantics (set! x (mref machine 'ram operand))))

(definstruction listingfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *byte-source*
  ".macro loadhalt val
ldx #val
hlt
.endm
start: lda $20
loadhalt 10")

(format t "~&Part 1: byte-encoded machine~2%Source:~%~A~2%" *byte-source*)

(let ((assembly (assemble *byte-source* :machine 'listingfoo)))
  (format t "Listing:~%")
  (print-listing assembly)

  ;; Expanded statements map to the invocation's line (6), so both LDX and
  ;; HLT come back for it; the listing shows "loadhalt 10", and
  ;; LISTING-LINE-DEFINITION-LINE gives the macro body line that emitted each
  ;; entry (2 for LDX). Only the encoded bytes (A2 0A) reflect the argument.
  (let* ((entries (listing-lines-for-source-line assembly 6))
         (ldx (find 2 entries :key #'listing-line-definition-line)))
    (assert (= 2 (length entries)))
    (assert ldx)
    (format t "~%LDX (macro body line 2) assembled at address ~D.~%"
            (listing-line-address ldx)))

  ;; Look a mid-instruction address back up to its owning entry.
  (let ((entry (listing-line-at assembly 1)))
    (assert entry)
    (format t "Address 1 belongs to the entry starting at ~D (source line ~D).~%"
            (listing-line-address entry) (listing-line-line entry))))

;;; Part 2: a DCPU-16-shaped word-encoded machine (examples/dcpu16.lisp) --
;;; the listing's cell column renders full 16-bit cells, not truncated bytes.

(defmachine listing-dcpu16
  (register pc :width 16)
  (register reg :width 16 :count 8)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field a 6)
    (field b 5)
    (field opcode 5)))

(defmode listing-rr expr "," expr)

(definstruction listing-dcpu16 set
  (modes listing-rr)
  (encoding
    (opcode 1)
    (operand dst :field b)
    (operand src :field a
      (variant (range -1 30) inline :bias 33)
      (variant :else (extra-word :escape #x1f))))
  (semantics (set! (reg dst) src)))

(definstruction listing-dcpu16 hlt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(defparameter *word-source*
  "set 0, 5        ; packs inline into field A
set 1, 1000      ; does not fit -- escapes to its own following word
hlt")

(format t "~2%Part 2: word-encoded (DCPU-16-shaped) machine~2%Source:~%~A~2%" *word-source*)

(let ((assembly (assemble *word-source* :machine 'listing-dcpu16)))
  (format t "Listing:~%")
  (print-listing assembly)
  (format t "~%With the cycles column (no (cycles n) declared, so every cost is 1):~%")
  (print-listing assembly :cycles t)
  (assert (search "1    " (listing-text assembly :cycles t)))
  (let ((entries (assembly-listing assembly)))
    (assert (= 3 (length entries)))
    (assert (= 1 (listing-line-size (first entries))))   ; SET 0,5 -- inline
    (assert (= 2 (listing-line-size (second entries))))  ; SET 1,1000 -- extra word
    (assert (= 1 (listing-line-size (third entries))))))

(format t "~%All assertions passed.~%")
