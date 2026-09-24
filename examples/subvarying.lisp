;;;; examples/subvarying.lisp
;;;;
;;;; Varying hole counts on a byte-encoded machine: one ONE-OF whose
;;;; alternatives contribute different numbers of operand holes -- a bare
;;;; register (one hole) vs. "[base, offset]" (two) -- sharing one opcode.
;;;;
;;;; The sub-opcode cell (examples/subchoice.lisp) is what tells decode
;;;; which shape follows: the carrying hole's (variant (choice m) (sub s))
;;;; selects an alternative, and a (for-choice ALT (operand ...)) subclause
;;;; names the extra hole(s) the longer alternative adds. definstruction
;;;; expands one descriptor per alternative, each with its own operand
;;;; count and instruction size.
;;;;
;;;; Run with:  sbcl --script examples/subvarying.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine subvaryingfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(defmode sv-imm expr :width 1)
(defmode sv-idx "[" expr "," expr "]" :width 1)
(defmode sv-any (one-of sv-imm sv-idx))

(definstruction subvaryingfoo lda
  (modes sv-any)
  (encoding (opcode #x10)
            (operand src :width 1
              (variant (choice sv-imm) (sub 0))
              (variant (choice sv-idx) (sub 1)))
            (for-choice sv-idx (operand off :width 1)))
  (semantics (choice-case src
               (sv-imm (set! a src))
               (sv-idx (set! a (mref machine 'ram (+ src off)))))))

(definstruction subvaryingfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "lda 5
lda [100, 7]
hlt")

(format t "~&LDA's two shapes share opcode #x10; the sub-opcode cell picks the size:~%")
(let ((short (assembly-cells (assemble "lda 5" :machine 'subvaryingfoo)))
      (long (assembly-cells (assemble "lda [100, 7]" :machine 'subvaryingfoo))))
  (format t "  lda 5         -> ~{~2,'0X~^ ~}~%" (coerce short 'list))
  (format t "  lda [100, 7]  -> ~{~2,'0X~^ ~}~%" (coerce long 'list))
  (assert (equalp #(#x10 #x00 #x05) short))
  (assert (equalp #(#x10 #x01 #x64 #x07) long))
  (multiple-value-bind (descriptor values size choices)
      (decode-instruction-at (vector-cell-reader long) 0 'subvaryingfoo)
    (assert (string= "LDA" (instruction-descriptor-name descriptor)))
    (assert (equal '(100 7) values))
    (assert (= 4 size))
    (assert (equal '(sv-idx sv-idx) choices))))

(format t "~%Running it -- the indexed form reads RAM[100 + 7]:~%")
(let ((m (make-machine 'subvaryingfoo)))
  (setf (mref m 'ram 107) 99)
  (load-program m (assemble *source* :machine 'subvaryingfoo))
  (run m)
  (assert (= 99 (sref m 'a)))
  (format t "  a ends at ~D (ram[107])~%" (sref m 'a)))

(format t "~%Disassembling renders each statement's own shape:~%")
(let ((lines (disassemble-assembly (assemble *source* :machine 'subvaryingfoo)
                                   :machine 'subvaryingfoo :labels nil :suffixes nil)))
  (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
  (assert (string= "lda $5" (disassembly-line-text (first lines))))
  (assert (string= "lda [$64,$7]" (disassembly-line-text (second lines)))))

(format t "~%All assertions passed.~%")
