;;;; examples/nestkeyed.lisp
;;;;
;;;; A nested ONE-OF whose alternatives differ in a per-hole attribute. IND
;;;; holds "(near)" (one signed byte) and "(abs addr)" (two unsigned bytes) and
;;;; sits beside "#imm" in the outer ANY. IND's holes have the same count, so
;;;; it does not vary, but its pick still decides width and signedness, so
;;;; options are trees: (choice (ind near)) and (choice (ind abs)). The sub-opcode
;;;; selector records the pick for decode, and CHOICE-CASE can read it back.
;;;;
;;;; Run with:  sbcl --script examples/nestkeyed.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine nestkeyedfoo
  (register pc :width 16)
  (register a :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode near expr :width 1 :signed t)
(defmode abs-addr "abs" expr :width 2)
(defmode ind "(" (one-of near abs-addr) ")")
(defmode imm "#" expr :width 1)
(defmode any (one-of ind imm))

(definstruction nestkeyedfoo ld
  (modes any)
  (encoding (opcode #x20)
            (operand src :mode
              (variant (choice (ind near)) (sub 0))
              (variant (choice (ind abs-addr)) (sub 1))
              (variant (choice imm) (sub 2))))
  (semantics
    (choice-case src
      (imm (set! a src))
      (ind (choice-case (src ind)
             (near (set! a (wrap-value src 16)))
             (abs-addr (set! a (+ src 1))))))))

(format t "~&Each pick has its own width:~%")
(dolist (case '(("ld (5)" #(#x20 #x00 #x05))
                ("ld (abs 1000)" #(#x20 #x01 #xE8 #x03))
                ("ld #7" #(#x20 #x02 #x07))))
  (let ((cells (assembly-cells (assemble (first case) :machine 'nestkeyedfoo))))
    (format t "  ~15A -> ~{~2,'0X~^ ~}~%" (first case) (coerce cells 'list))
    (assert (equalp (second case) cells))))

(format t "~%Decode sign-extends only the near pick:~%")
(dolist (case '((#(#x20 #x00 #xFF) (-1) (t)) (#(#x20 #x01 #xE8 #x03) (1000) (nil))))
  (multiple-value-bind (descriptor values)
      (decode-instruction-at (vector-cell-reader (first case)) 0 'nestkeyedfoo)
    (format t "  ~S -> ~S~%" (coerce (first case) 'list) values)
    (assert (equal (second case) values))
    (assert (equal (third case) (instruction-descriptor-operand-signedness descriptor)))))

(format t "~%Running it:~%")
(dolist (case '(("ld (5)" 5) ("ld (abs 1000)" 1001) ("ld #7" 7)))
  (let ((m (make-machine 'nestkeyedfoo)))
    (load-program m (assemble (first case) :machine 'nestkeyedfoo))
    (step-machine m)
    (format t "  ~15A -> a = ~D~%" (first case) (sref m 'a))
    (assert (= (second case) (sref m 'a)))))

(format t "~%All assertions passed.~%")
