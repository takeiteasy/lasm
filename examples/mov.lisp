;;;; examples/mov.lisp
;;;;
;;;; Multi-operand instructions: a MOV wiring two operand encoding
;;;; fields off one two-hole addressing mode -- a 2-byte destination address
;;;; and a 1-byte immediate value, named DST/VAL so its (semantics ...) body
;;;; reads directly rather than indexing into a list. Assembled and run end
;;;; to end, in the style of examples/counter.lisp; see docs/instructions.md
;;;; ("Repeated (operand ...) subclauses") and docs/modes.md.
;;;;
;;;; Run with:  sbcl --script examples/mov.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(deflexer twoop-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (string-delim "\""))

;; MOV-IMM's pattern: an address expression, a comma, then a value
;; expression -- two EXPR holes on one mode, one more than any M1/M2 mode
;; needed.
(defmode mov-imm expr "," expr)

(defmachine twoop
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction twoop mov
  (modes mov-imm)
  (encoding (opcode #x01) (operand dst :width 2) (operand val :width 1))
  (semantics (setf (mref machine 'ram dst) val)))

(definstruction twoop lda
  (modes absolute)
  (encoding (opcode #xA1) (operand :mode))
  (semantics (set! a (mref machine 'ram operand)) (set-flags! (z (zero? a)))))

(definstruction twoop hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "        mov $2000, $42   ; ram[$2000] = 0x42
        lda $2000         ; a = ram[$2000]
        hlt               ; stop the emulator loop (see docs/emulator.md)")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :lexer 'twoop-syntax :machine 'twoop)))
  (format t "  bytes: ~S~%" (coerce (assembly-cells assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'twoop)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = $~2,'0X, RAM[$2000] = $~2,'0X~%" (sref m 'a) (mref m 'ram #x2000)))))
