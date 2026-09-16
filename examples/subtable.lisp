;;;; examples/subtable.lisp
;;;;
;;;; Multi-hole sub-opcode selection on a byte-encoded machine (#128, the
;;;; follow-up examples/subchoice.lisp's own hole-selected sub-opcode filed
;;;; for itself): subchoice.lisp's (variant (choice m) (sub s)) selector
;;;; picks the sub-opcode cell by which alternative *one* ONE-OF hole
;;;; matched -- the sub-opcode cell is one cell, so a mode with two holes
;;;; each wanting to pick it has no coherent meaning without an explicit
;;;; cartesian product of values.
;;;;
;;;; A (sub-opcode ...) subclause is that product, spelled out: each
;;;; (variant (choice m1 m2 ...) (sub s)) names one alternative per
;;;; participating ONE-OF hole, in hole order, and every combination of the
;;;; mode's ONE-OF holes must be claimed by exactly one such entry -- there
;;;; is no value-selected fallback for an unclaimed combination, the same
;;;; permanent-error rule subchoice.lisp's single-hole selector already
;;;; holds every alternative to.
;;;;
;;;; This is what lets a two-operand instruction -- here, MOV's destination
;;;; and source, each independently direct or indirect -- share one opcode,
;;;; discriminated purely by which combination of alternatives the two
;;;; operands' own syntax matched.
;;;;
;;;; Run with:  sbcl --script examples/subtable.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; Machine: an 8-bit machine with room for two operand bytes, same shape as
;;; subchoice.lisp's.

(defmachine subtablefoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

;; A mode with two holes, each independently a bare register-style literal
;; or "[expr]" indirection -- reusing subchoice.lisp's SC-DIRECT/SC-INDIRECT
;; shape at both positions.
(defmode sc-direct expr)
(defmode sc-indirect "[" expr "]")
(defmode sc-two (one-of sc-direct sc-indirect) "," (one-of sc-direct sc-indirect))

;; MOV dst, src -- writes SRC's value (direct: the literal itself; indirect:
;; RAM[n]) to DST's own location (direct: RAM[n]; indirect: RAM[RAM[n]]).
;; One mnemonic, one opcode, one mode -- the sub-opcode cell is chosen by
;; which combination of the two holes' own ONE-OF alternatives matched, a
;; 2x2 cross product needing four distinct sub values.
(definstruction subtablefoo mov
  (modes sc-two)
  (encoding (opcode #x10)
            (operand dst :width 1)
            (operand src :width 1)
            (sub-opcode
              (variant (choice sc-direct sc-direct)     (sub 0))
              (variant (choice sc-direct sc-indirect)   (sub 1))
              (variant (choice sc-indirect sc-direct)   (sub 2))
              (variant (choice sc-indirect sc-indirect) (sub 3))))
  (semantics
    (let ((value (choice-case src
                   (sc-direct src)
                   (sc-indirect (mref machine 'ram src)))))
      (choice-case dst
        (sc-direct (setf (mref machine 'ram dst) value))
        (sc-indirect (setf (mref machine 'ram (mref machine 'ram dst)) value))))))

(definstruction subtablefoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(format t "~&MOV's four ONE-OF-matched combinations, sharing opcode #x10 and one mode:~%")
(let ((dd (assembly-cells (assemble "mov 20, 5" :machine 'subtablefoo)))
      (di (assembly-cells (assemble "mov 20, [5]" :machine 'subtablefoo)))
      (id (assembly-cells (assemble "mov [20], 5" :machine 'subtablefoo)))
      (ii (assembly-cells (assemble "mov [20], [5]" :machine 'subtablefoo))))
  (format t "  mov 20, 5      -> ~{~2,'0X~^ ~}~%" (coerce dd 'list))
  (format t "  mov 20, [5]    -> ~{~2,'0X~^ ~}~%" (coerce di 'list))
  (format t "  mov [20], 5    -> ~{~2,'0X~^ ~}~%" (coerce id 'list))
  (format t "  mov [20], [5]  -> ~{~2,'0X~^ ~}~%" (coerce ii 'list))
  (assert (equalp #(#x10 #x00 #x14 #x05) dd))
  (assert (equalp #(#x10 #x01 #x14 #x05) di))
  (assert (equalp #(#x10 #x02 #x14 #x05) id))
  (assert (equalp #(#x10 #x03 #x14 #x05) ii))
  (format t "~%Sub-opcode 0-3 for the four combinations -- the same opcode, told apart ~
purely by which alternative each hole matched.~%")
  (dolist (case (list (list dd 'sc-direct 'sc-direct) (list di 'sc-direct 'sc-indirect)
                       (list id 'sc-indirect 'sc-direct) (list ii 'sc-indirect 'sc-indirect)))
    (destructuring-bind (bytes dst-choice src-choice) case
      (multiple-value-bind (descriptor values size choices)
          (decode-instruction-at (vector-cell-reader bytes) 0 'subtablefoo)
        (assert (not (eq :decode-failure descriptor)))
        (assert (string= "MOV" (instruction-descriptor-name descriptor)))
        (assert (= 4 size))
        (assert (equal (list 20 5) values))
        ;; #128: CHOICES now carries a hole-aligned record for *both*
        ;; operand holes, not just one -- the multi-hole generalization of
        ;; #126's single-hole SUB-CHOICES.
        (assert (eq dst-choice (%matched-choice-name choices 0)))
        (assert (eq src-choice (%matched-choice-name choices 1)))))))

(format t "~%An unmatched sub-opcode cell is a decode failure:~%")
(let ((bogus (vector-cell-reader (vector #x10 99 20 5))))
  (assert (eq :decode-failure (decode-instruction-at bogus 0 'subtablefoo)))
  (format t "  [#x10 99 20 5] -> :DECODE-FAILURE (no MOV sub-opcode is 99)~%"))

(defparameter *source*
  "mov 20, 5
mov 21, [20]
hlt")

(format t "~%CHOICE-CASE dispatch on both holes at once:~%")
(let ((assembly (assemble *source* :machine 'subtablefoo)))
  (let ((m (make-machine 'subtablefoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (declare (ignore steps))
      (assert (eq :trap reason))
      ;; "mov 20, 5" writes 5 to ram[20] (direct/direct); "mov 21, [20]"
      ;; reads ram[20] (=5, indirect src) and writes it to ram[21]
      ;; (direct dst) -- both combinations' own CHOICE-CASE branch ran.
      (assert (= 5 (mref m 'ram 20)))
      (assert (= 5 (mref m 'ram 21)))
      (format t "  mov 20, 5 then mov 21, [20]: ram[20]=~D, ram[21]=~D -- ~
each statement's own combination dispatched correctly.~%" (mref m 'ram 20) (mref m 'ram 21)))))

(format t "~%Disassembling (renders each statement's own matched alternative at both holes):~%")
(let ((assembly (assemble *source* :machine 'subtablefoo)))
  (let ((lines (disassemble-assembly assembly :machine 'subtablefoo :labels nil :suffixes nil)))
    (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
    (assert (string= "mov $14,$5" (disassembly-line-text (first lines))))
    (assert (string= "mov $15,[$14]" (disassembly-line-text (second lines))))
    (format t "~%Both round-trip to their own real syntax.~%")))

;; #129: per-hole :WIDTH, two holes at once. Each hole independently picks a
;; 1-byte narrow literal or a "#"-prefixed 2-byte wide one, jointly selecting
;; the sub-opcode cell via the same (sub-opcode ...) table MOV uses above --
;; #128 generalized both %CHECK-BYTE-ONE-OF-SIGNED and (with #129)
;; %CHECK-BYTE-ONE-OF-WIDTH from one carrying hole to a carrying-hole *set*,
;; so two holes disagreeing on width is exactly as legal as two disagreeing
;; on signedness.
(defmode sw-narrow expr :width 1)
(defmode sw-wide "#" expr :width 2)
(defmode sw-two (one-of sw-narrow sw-wide) "," (one-of sw-narrow sw-wide))

(definstruction subtablefoo movw
  (modes sw-two)
  (encoding (opcode #x20)
            (operand v1 :mode)
            (operand v2 :mode)
            (sub-opcode
              (variant (choice sw-narrow sw-narrow) (sub 0))
              (variant (choice sw-narrow sw-wide)   (sub 1))
              (variant (choice sw-wide sw-narrow)   (sub 2))
              (variant (choice sw-wide sw-wide)     (sub 3))))
  (semantics (set! a (wrap-value (+ v1 v2) 8))))

(format t "~%MOVW's four ONE-OF-matched width combinations, sharing opcode #x20:~%")
(let ((nn (assembly-cells (assemble "movw 5, 10" :machine 'subtablefoo)))
      (ww (assembly-cells (assemble "movw #300, #400" :machine 'subtablefoo))))
  (format t "  movw 5, 10        -> ~{~2,'0X~^ ~} (2 narrow operands, 4 bytes total)~%" (coerce nn 'list))
  (format t "  movw #300, #400   -> ~{~2,'0X~^ ~} (2 wide operands, 6 bytes total)~%" (coerce ww 'list))
  (assert (equalp #(#x20 #x00 #x05 #x0A) nn))
  (assert (equalp #(#x20 #x03 #x2C #x01 #x90 #x01) ww))
  (multiple-value-bind (descriptor values size choices) (decode-instruction-at (vector-cell-reader ww) 0 'subtablefoo)
    (assert (string= "MOVW" (instruction-descriptor-name descriptor)))
    (assert (equal (list 300 400) values))
    (assert (= 6 size))
    (assert (eq 'sw-wide (%matched-choice-name choices 0)))
    (assert (eq 'sw-wide (%matched-choice-name choices 1))))
  (format t "~%Each hole's own width is read independently at decode, not fixed once per mode.~%"))

(format t "~%All assertions passed.~%")
