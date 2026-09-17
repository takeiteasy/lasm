;;;; examples/bigendian.lisp
;;;;
;;;; #66: a big-endian machine -- BIGFOO's sole memory element declares
;;;; :ENDIAN :BIG, so ABSOLUTE's two-byte address operand and .WORD's data
;;;; both lay their high byte down first, the opposite of every earlier
;;;; example (#53's WORDADDR included, which stays little-endian). Otherwise
;;;; deliberately as close to examples/counter.lisp's shape as possible, so
;;;; the only difference worth noticing is the byte order.
;;;;
;;;; Run with:  sbcl --script examples/bigendian.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine bigfoo
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16 :endian :big))

(definstruction bigfoo lda
  (modes immediate)
  (encoding (opcode #xA9) (operand :mode))
  (semantics (set! a operand)))

(definstruction bigfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction bigfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "lda #$42
sta $1234
hlt
result: .word $ABCD")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'bigfoo)))
  (format t "  cells: ~S~%" (coerce (assembly-cells assembly) 'list))
  ;; STA's two-byte absolute operand $1234 -- big-endian means the high
  ;; byte #x12 comes first, at cell index 3 (opcode #xA9, operand #x42,
  ;; opcode #x8D, then the address), low byte #x34 right after.
  (assert (equal '(#x12 #x34) (coerce (subseq (assembly-cells assembly) 3 5) 'list)))
  ;; RESULT's .WORD $ABCD -- same rule applies to directive data, not just
  ;; instruction operands, since both go through %ENCODE-VALUE-CELLS (#66).
  (assert (equal '(#xAB #xCD) (coerce (subseq (assembly-cells assembly) 6 8) 'list)))

  (format t "~%Running:~%")
  (let ((m (make-machine 'bigfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  RAM[$1234] = ~D (expected 66, i.e. $42)~%" (mref m 'ram #x1234))
      (assert (eq :trap reason))
      (assert (= 3 steps))
      (assert (= #x42 (mref m 'ram #x1234)))
      (format t "~%All assertions passed.~%"))))
