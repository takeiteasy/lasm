;;;; examples/nestvarying.lisp
;;;;
;;;; A varying ONE-OF nested inside another ONE-OF. NV-IND is itself a
;;;; ONE-OF whose alternatives differ in hole count -- a bare address (one
;;;; hole) vs. "[base, offset]" (two) -- and it sits beside "#imm" in the
;;;; outer NV-ANY.
;;;;
;;;; A path names the alternative picked inside the nested element:
;;;; (choice (nv-ind nv-idx)) selects the two-hole shape, and
;;;; (for-choice (src nv-ind nv-idx) ...) names its extra hole. A
;;;; choice-case with a qualified operand, (choice-case (src nv-ind) ...),
;;;; dispatches on the inner pick.
;;;;
;;;; Run with:  sbcl --script examples/nestvarying.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine nestvaryingfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(defmode nv-abs expr)
(defmode nv-idx "[" expr "," expr "]")
(defmode nv-ind (one-of nv-abs nv-idx))
(defmode nv-imm "#" expr)
(defmode nv-any (one-of nv-ind nv-imm))

(definstruction nestvaryingfoo lda
  (modes nv-any)
  (encoding (opcode #x10)
            (operand src :width 1
              (variant (choice (nv-ind nv-abs)) (sub 0))
              (variant (choice (nv-ind nv-idx)) (sub 1))
              (variant (choice nv-imm) (sub 2)))
            (for-choice (src nv-ind nv-idx) (operand off :width 1)))
  (semantics (choice-case src
               (nv-imm (set! a src))
               (nv-ind (choice-case (src nv-ind)
                         (nv-abs (set! a (mref machine 'ram src)))
                         (nv-idx (set! a (mref machine 'ram (+ src off)))))))))

(definstruction nestvaryingfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "lda #7
lda 100
lda [100, 7]
hlt")

(format t "~&Three shapes share opcode #x10; the sub-opcode cell picks one:~%")
(dolist (case '(("lda 100" #(#x10 #x00 #x64))
                ("lda [100, 7]" #(#x10 #x01 #x64 #x07))
                ("lda #7" #(#x10 #x02 #x07))))
  (let ((cells (assembly-cells (assemble (first case) :machine 'nestvaryingfoo))))
    (format t "  ~13A -> ~{~2,'0X~^ ~}~%" (first case) (coerce cells 'list))
    (assert (equalp (second case) cells))))

(multiple-value-bind (descriptor values size choices)
    (decode-instruction-at (vector-cell-reader #(#x10 #x01 #x64 #x07)) 0 'nestvaryingfoo)
  (assert (string= "LDA" (instruction-descriptor-name descriptor)))
  (assert (equal '(100 7) values))
  (assert (= 4 size))
  (assert (equal '((nv-ind nv-idx) (nv-ind nv-idx)) choices)))

(format t "~%Running it -- each shape reads a different value:~%")
(let ((m (make-machine 'nestvaryingfoo)))
  (setf (mref m 'ram 100) 55
        (mref m 'ram 107) 99)
  (dolist (case '(("lda #7" 7) ("lda 100" 55) ("lda [100, 7]" 99)))
    (setf (sref m 'a) 0
          (sref m 'pc) 0)
    (load-program m (assemble (format nil "~A~%hlt" (first case)) :machine 'nestvaryingfoo))
    (run m)
    (format t "  ~13A -> a = ~D~%" (first case) (sref m 'a))
    (assert (= (second case) (sref m 'a)))))

(format t "~%Disassembling renders each statement's own shape:~%")
(let ((lines (disassemble-assembly (assemble *source* :machine 'nestvaryingfoo)
                                   :machine 'nestvaryingfoo :labels nil :suffixes nil)))
  (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
  (assert (string= "lda #$7" (disassembly-line-text (first lines))))
  (assert (string= "lda $64" (disassembly-line-text (second lines))))
  (assert (string= "lda [$64,$7]" (disassembly-line-text (third lines)))))

(format t "~%All assertions passed.~%")
