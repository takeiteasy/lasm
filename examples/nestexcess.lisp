;;;; examples/nesttree.lisp
;;;;
;;;; An alternative whose minimum shape has more holes than its sibling's.
;;;; NT-PAIR has two ("lhs" and "rhs") where "#imm" has one, so the operand has one
;;;; base hole and NT-PAIR's second minimum hole is its own extra:
;;;; (for-choice (src nt-pair) (operand rv ...)) declares it. The indexed
;;;; extras of each ONE-OF are declared as in nesttree.lisp. Extras bind in
;;;; pattern order: src, loff, rv, roff.
;;;;
;;;; Run with:  sbcl --script examples/nestexcess.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine nestexcessfoo
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode nt-abs expr)
(defmode nt-idx "[" expr "," expr "]")
(defmode nt-pair (one-of (lhs nt-abs nt-idx)) "," (one-of (rhs nt-abs nt-idx)))
(defmode nt-imm "#" expr)
(defmode nt-any (one-of nt-pair nt-imm))

(definstruction nestexcessfoo add
  (modes nt-any)
  (encoding (opcode #x10)
            (operand src :width 1
              (variant (choice (nt-pair nt-abs nt-abs)) (sub 0))
              (variant (choice (nt-pair nt-abs nt-idx)) (sub 1))
              (variant (choice (nt-pair nt-idx nt-abs)) (sub 2))
              (variant (choice (nt-pair nt-idx nt-idx)) (sub 3))
              (variant (choice nt-imm) (sub 4)))
            (for-choice (src nt-pair) (operand rv :width 1))
            (for-choice (src nt-pair lhs nt-idx) (operand loff :width 1))
            (for-choice (src nt-pair rhs nt-idx) (operand roff :width 1)))
  (semantics
    (choice-case src
      (nt-imm (set! a src))
      (nt-pair (choice-case (src nt-pair lhs)
                 (nt-abs (choice-case (src nt-pair rhs)
                           (nt-abs (set! a (+ src rv)))
                           (nt-idx (set! a (+ src rv roff)))))
                 (nt-idx (choice-case (src nt-pair rhs)
                           (nt-abs (set! a (+ src loff rv)))
                           (nt-idx (set! a (+ src loff rv roff))))))))))

(format t "~&Each combination of picks has its own sub-opcode and cell layout:~%")
(dolist (case '(("add 1, 3" #(#x10 #x00 #x01 #x03))
                ("add [1, 2], 3" #(#x10 #x02 #x01 #x02 #x03))
                ("add 1, [3, 4]" #(#x10 #x01 #x01 #x03 #x04))
                ("add [1, 2], [3, 4]" #(#x10 #x03 #x01 #x02 #x03 #x04))
                ("add #1" #(#x10 #x04 #x01))))
  (let ((cells (assembly-cells (assemble (first case) :machine 'nestexcessfoo))))
    (format t "  ~19A -> ~{~2,'0X~^ ~}~%" (first case) (coerce cells 'list))
    (assert (equalp (second case) cells))))

(multiple-value-bind (descriptor values size choices)
    (decode-instruction-at (vector-cell-reader #(#x10 #x02 #x01 #x02 #x03)) 0 'nestexcessfoo)
  (assert (string= "ADD" (instruction-descriptor-name descriptor)))
  (assert (equal '(src loff rv) (instruction-descriptor-operand-names descriptor)))
  (assert (equal '(1 2 3) values))
  (assert (= 5 size))
  (assert (every (lambda (choice) (equal '(nt-pair nt-idx nt-abs) choice)) choices)))

(format t "~%Running it -- own extras and ONE-OF extras keep their own bindings:~%")
(let ((m (make-machine 'nestexcessfoo)))
  (dolist (case '(("add 1, 3" 4) ("add [1, 2], 3" 6) ("add 1, [3, 4]" 8)
                  ("add [1, 2], [3, 4]" 10) ("add #1" 1)))
    (setf (sref m 'pc) 0)
    (load-program m (assemble (first case) :machine 'nestexcessfoo))
    (step-machine m)
    (format t "  ~19A -> a = ~D~%" (first case) (sref m 'a))
    (assert (= (second case) (sref m 'a)))))

(format t "~%Disassembling renders each statement's own shape:~%")
(dolist (text '("add 1, 3" "add [1, 2], 3" "add 1, [3, 4]" "add [1, 2], [3, 4]"))
  (let ((line (first (disassemble-assembly (assemble text :machine 'nestexcessfoo)
                                           :machine 'nestexcessfoo :labels nil :suffixes nil))))
    (format t "  ~A~%" (disassembly-line-text line))))

(format t "~%All assertions passed.~%")
