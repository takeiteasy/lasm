;;;; examples/slotvarying.lisp
;;;;
;;;; A named ONE-OF with a hole-less alternative on a byte-encoded machine.
;;;; SV-SRC picks between a register-like value (one hole), the fixed words
;;;; SP and PC (no hole), and a nested SV-STK whose own options are POP (no
;;;; hole) or "[base, offset]" (two holes).
;;;;
;;;; A hole-less alternative has no operand hole to carry its sub-opcode
;;;; selector, so the sub-opcode table names the ONE-OF by its slot:
;;;; (sub-opcode (holes src) ...). A nested alternative is one path,
;;;; (choice (sv-stk sv-pop)), and (choice-case (src sv-stk) ...) dispatches
;;;; on the inner pick.
;;;;
;;;; Run with:  sbcl --script examples/slotvarying.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine slotvaryingfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(defmode sv-val expr)
(defmode sv-sp "SP")
(defmode sv-pc "PC")
(defmode sv-pop "POP")
(defmode sv-idx "[" expr "," expr "]")
(defmode sv-stk (one-of sv-pop sv-idx))
(defmode sv-src (one-of (src sv-val sv-sp sv-pc sv-stk)))

(definstruction slotvaryingfoo lda
  (modes sv-src)
  (encoding (opcode #x10)
            (sub-opcode (holes src)
              (variant (choice sv-val) (sub 0))
              (variant (choice sv-sp) (sub 1))
              (variant (choice sv-pc) (sub 2))
              (variant (choice (sv-stk sv-pop)) (sub 3))
              (variant (choice (sv-stk sv-idx)) (sub 4)))
            (for-choice (src sv-val) (operand val :width 1))
            (for-choice (src sv-stk sv-idx) (operand base :width 1) (operand off :width 1)))
  (semantics (choice-case src
               (sv-val (set! a val))
               (sv-sp (set! a #xf0))
               (sv-pc (set! a #xf1))
               (sv-stk (choice-case (src sv-stk)
                         (sv-pop (set! a #xf2))
                         (sv-idx (set! a (+ base off))))))))

(definstruction slotvaryingfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(format t "~&Each shape gets its own sub-opcode; SP and PC have no operand cells:~%")
(dolist (case '(("lda 7" #(#x10 #x00 #x07))
                ("lda SP" #(#x10 #x01))
                ("lda PC" #(#x10 #x02))
                ("lda POP" #(#x10 #x03))
                ("lda [100, 7]" #(#x10 #x04 #x64 #x07))))
  (let ((cells (assembly-cells (assemble (first case) :machine 'slotvaryingfoo))))
    (format t "  ~13A -> ~{~2,'0X~^ ~}~%" (first case) (coerce cells 'list))
    (assert (equalp (second case) cells))))

(multiple-value-bind (descriptor values size choices selections)
    (decode-instruction-at (vector-cell-reader #(#x10 #x03)) 0 'slotvaryingfoo)
  (assert (string= "LDA" (instruction-descriptor-name descriptor)))
  (assert (null values))
  (assert (= 2 size))
  (assert (null choices))
  (assert (equal '((src sv-stk sv-pop)) selections)))

(format t "~%Running it -- each shape sets a different value:~%")
(dolist (case '(("lda 7" 7) ("lda SP" #xf0) ("lda PC" #xf1) ("lda POP" #xf2) ("lda [100, 7]" 107)))
  (let ((m (make-machine 'slotvaryingfoo)))
    (load-program m (assemble (format nil "~A~%hlt" (first case)) :machine 'slotvaryingfoo))
    (run m)
    (format t "  ~13A -> a = ~D~%" (first case) (sref m 'a))
    (assert (= (second case) (sref m 'a)))))

(format t "~%Disassembling renders each statement's own shape:~%")
(let ((lines (disassemble-assembly (assemble "lda SP
lda PC
lda POP
lda [100, 7]" :machine 'slotvaryingfoo)
                                   :machine 'slotvaryingfoo :labels nil :suffixes nil)))
  (dolist (l lines) (format t "  ~A~%" (disassembly-line-text l)))
  (assert (equal '("lda SP" "lda PC" "lda POP" "lda [$64,$7]")
                 (mapcar #'disassembly-line-text lines))))

(format t "~%All assertions passed.~%")
