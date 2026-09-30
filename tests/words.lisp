;;;; tests/pairs.lisp
;;;; #416: register-pair words. pairfoo-lang-abi (tests/fixtures/cli/pairfoo.lisp) keeps
;;;; every 16-bit language word in two 8-bit registers.

(in-package #:lasm)

(fiveam:def-suite words :in lasm)
(fiveam:in-suite words)

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "tests/fixtures/cli/pairfoo.lisp")))

(eval '(defbackend pf-static-abi (:extends pairfoo-lang-abi) (frame :static t)))

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "tests/fixtures/cli/zpfoo.lisp")))

(eval '(defbackend zf-static-abi (:extends zpfoo-lang-abi) (frame :static t)))

;;; Definition

(fiveam:test a-backend-declares-register-pairs
  (fiveam:is (equal '(("AB" ("B" "A") 8) ("CD" ("D" "C") 8) ("EF" ("F" "E") 8) ("GH" ("H" "G") 8))
                    (backend-words 'pairfoo-lang-abi)))
  (fiveam:is (null (backend-words 'callfoo-lang-abi)))
  (fiveam:is (equal '("AB") (backend-register 'pairfoo-lang-abi :return))))

(fiveam:test the-pairs-make-the-word-width
  (fiveam:is (= 2 (backend-word-cells 'pairfoo-lang-abi)))
  (fiveam:is (= 2 (backend-word-cells 'pf-static-abi)))
  (fiveam:is (= 1 (backend-word-cells 'callfoo-lang-abi))))

(eval '(defmachine pf-mixed-machine
         (register pc :width 16) (register sp :width 16)
         (register r :width 8 :names (a b)) (register w :width 16)
         (memory ram :width 8 :addr-width 16)
         (stack-pointer sp :memory ram :grows :down)))

(defparameter +pf-bad-backends+
  '(("is already a register" (defbackend pf-bad-1 (:extends pairfoo-lang-abi) (registers :words ((a c d)))))
    ("is already a register" (defbackend pf-bad-2 (:extends pairfoo-lang-abi) (registers :words ((ab a b) (ab c d)))))
    ("is a part of both" (defbackend pf-bad-3 (:extends pairfoo-lang-abi) (registers :words ((ab a b) (xy b c)))))
    ("for two parts" (defbackend pf-bad-4 (:extends pairfoo-lang-abi) (registers :words ((ab a a)))))
    ("not a register" (defbackend pf-bad-5 (:extends pairfoo-lang-abi) (registers :words ((ab a nosuch)))))
    ("expected (NAME PART PART...)" (defbackend pf-bad-6 (:extends pairfoo-lang-abi) (registers :words ((ab a)))))
    ("not a register word" (defbackend pf-bad-7 (:extends pairfoo-lang-abi) (registers :scratch (ab c))))
    ("not a register word" (defbackend pf-bad-8 (:extends pairfoo-lang-abi) (registers :callee-saved (e))))
    ("not a register word" (defbackend pf-bad-9 (:extends pairfoo-lang-abi) (call :args (c))))
    ("frame :slot" (defbackend pf-bad-10 (:extends pairfoo-lang-abi) (frame :offsets :slots)))
    ("needs (registers :words" (defbackend pf-bad-11 (:extends callfoo-abi) (ops (:tag (x) (movv (:lo x) (:hi x)))))))
  "Backends that fail to define, each with the words its error contains.")

(fiveam:test a-bad-register-pair-is-a-definition-error
  (loop for (text form) in +pf-bad-backends+
        do (let ((c (%backend-error-of form)))
             (fiveam:is (typep c 'backend-definition-error) "~S" form)
             (fiveam:is (and c (search text (princ-to-string c))) "~S: ~A" form c))))

(fiveam:test pairs-need-halves-of-one-width
  (let ((c (%backend-error-of '(defbackend pf-bad-width (:isa pf-mixed-machine)
                                (registers :words ((ab a w)) :return (ab) :operand reg)))))
    (fiveam:is (typep c 'backend-definition-error))
    (fiveam:is (search "same width" (princ-to-string c)))))

;;; Expansion

(defun %pf-plain (tree)
  "TREE with each symbol and string as a lowercase string, for comparing expansions."
  (typecase tree
    (cons (cons (%pf-plain (car tree)) (%pf-plain (cdr tree))))
    (null nil)
    (keyword tree)
    ((or symbol string) (string-downcase (string tree)))
    (t tree)))

(defun %pf-expand (op &rest args)
  (%pf-plain (backend-expand-op 'pairfoo-lang-abi op args)))

(fiveam:test a-half-of-a-register-operand-is-that-register
  (fiveam:is (equal '(("movv" ("reg" "d") ("reg" "b")) ("movv" ("reg" "c") ("reg" "a")))
                    (%pf-expand :move '(reg cd) '(reg ab)))))

(fiveam:test a-half-of-an-integer-is-masked-to-the-half
  (fiveam:is (equal '(("ldi" ("reg" "b") ("imm" 52)) ("ldi" ("reg" "a") ("imm" 18)))
                    (%pf-expand :const '(reg ab) 4660)))
  (fiveam:is (equal '(("ldi" ("reg" "d") ("imm" 255)) ("ldi" ("reg" "c") ("imm" 255)))
                    (%pf-expand :const '(reg cd) -1))
             "a negative integer splits as two's complement"))

(fiveam:test a-half-of-a-label-is-an-expression
  (fiveam:is (equal '(("ldi" ("reg" "b") ("imm" ("&" "there" 255)))
                      ("ldi" ("reg" "a") ("imm" ("&" (">>" "there" 8) 255))))
                    (%pf-expand :const '(reg ab) 'there))))

(fiveam:test a-half-of-a-frame-slot-waits-for-its-offset
  (let ((forms (backend-expand-op 'pairfoo-lang-abi :get '((reg ab) (:local 0)))))
    (fiveam:is (equal '(:part 0 (:local 0)) (third (first forms))))
    (fiveam:is (equal '(:part 1 (:local 0)) (third (second forms))))))

(fiveam:test a-resolved-slot-splits-in-endian-order
  (fiveam:is (equal '(("lds" ("reg" "d") ("sp-idx" 6)) ("lds" ("reg" "c") ("sp-idx" 7)))
                    (%pf-expand :move '(reg cd) '(sp-idx 6)))
             "little-endian: the low half is the lower cell"))

(fiveam:test a-pair-used-whole-in-a-template-is-an-items-error
  (eval '(defbackend pf-whole-abi (:extends pairfoo-lang-abi)
          (ops (:branch-zero (r target) (jzp r target)))))
  (let ((c (handler-case (progn (backend-expand-op 'pf-whole-abi :branch-zero '((reg ab) done)) nil)
             (items-malformed (c) c))))
    (fiveam:is (typep c 'items-malformed))
    (fiveam:is (and c (search "whole" (princ-to-string c))))))

;;; A big-endian machine puts the high half in the lower cell.

(eval '(defmachine pf-be-machine
         (register pc :width 16) (register sp :width 16)
         (register r :width 8 :names (a b))
         (memory ram :width 8 :addr-width 16 :endian :big)
         (stack-pointer sp :memory ram :grows :down)))
(eval '(defmode pf-be-reg (expr :register r)))
(eval '(defmode pf-be-slot "[" "sp" "+" expr "]"))
(eval '(defmode pf-be-rs (expr :register r) "," "[" "sp" "+" expr "]"))
(eval '(definstruction pf-be-machine lds (modes pf-be-rs)
        (encoding (opcode 1) (operand dst :width 1) (operand offset :width 1))
        (semantics (set! (r dst) (mref machine 'ram (wrap-value (+ sp offset) 16))))))
(eval '(defbackend pf-be-abi (:isa pf-be-machine)
        (registers :words ((ab a b)) :return (ab) :stack-pointer sp :operand reg)
        (frame :grows :down :slot slot :offsets :cells :counts :cells)
        (operands (reg pf-be-reg) (slot pf-be-slot))
        (ops (:get (r s) (lds (:lo r) (:lo s)) (lds (:hi r) (:hi s))))))

(fiveam:test a-big-endian-word-has-its-high-half-in-the-lower-cell
  (fiveam:is (= 2 (backend-word-cells 'pf-be-abi)))
  (let ((*items-backend* (find-backend 'pf-be-abi)))
    (fiveam:is (equal '("slot" 4) (%pf-plain (%frame-part 1 '(slot 4) nil))))
    (fiveam:is (equal '("slot" 5) (%pf-plain (%frame-part 0 '(slot 4) nil)))))
  (let ((*items-backend* (find-backend 'pairfoo-lang-abi)))
    (fiveam:is (equal '("sp-idx" 5) (%pf-plain (%frame-part 1 '(sp-idx 4) nil)))
               "little-endian: the high half is the higher cell")
    (fiveam:is (equal '("sp-idx" 4) (%pf-plain (%frame-part 0 '(sp-idx 4) nil))))))

;;; Programs

(defparameter +pf-programs+
  '(("(defun main () (+ 200 100))" . 300)
    ("(defun main () (- 256 1))" . 255)
    ("(defun main () (- 5))" . 65531)
    ("(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 5))" . 120)
    ("(defun main () (+ (< 3 5) (>= 3 5) (= 4 4) (/= 4 4) (<= 4 4) (> 2 1)))" . 4)
    ("(defun main () (< (- 1) 2))" . 1)
    ("(defun main () (< 300 200))" . 0)
    ("(defun main () (+ (< 32767 -32768) (> 32767 -32768) (< 255 256) (> 256 255) (= 256 512)))" . 3)
    ("(defvar x 1000) (defvar y 2000) (defun main () (+ x y))" . 3000)
    ("(defarray arr (10 20 300)) (defun main () (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))" . 330)
    ("(defarray arr (10 20 30)) (defun main () (let ((i 2)) (aset arr i 999) (aref arr 2)))" . 999)
    ("(defstring s \"hi\") (defun main () (+ (aref s 0) (aref s 1)))" . 209)
    ("(defun main () (poke 8192 7000) (poke 8194 (+ (peek 8192) 1)) (peek 8194))" . 7001)
    ("(defun f (x) x) (defarray fns ((function f))) (defun main () (funcall (aref fns 0) 4242))" . 4242)
    ("(defun add (a b) (+ a b)) (defvar g 0) (defun main () (set g (function add)) (funcall g 300 400))" . 700)
    ("(defun f (a b c d) (- (* a d) (- b c))) (defun main () (f 600 1 2 700))" . 26785)
    ("(defun main () (/ 1000 7))" . 142)
    ("(defun main () (+ (mod 1000 7) (shl 3 9)))" . 1542)
    ("(defun main () (+ (logand 4660 255) (logior 4096 1) (logxor 65535 255) (shr 32768 4)))" . 5941)
    ("(defun main () (let ((x 9)) (asm (:clobbers a) (:op :const (reg ab) 3) (:op :set (:var x) (reg ab))) x))" . 3)
    ("(defun f (n) (+ n 1)) (defun main () (+ (f 1) (f 2) (f 3) (f 4)))" . 14)
    ("(defun f (x) (+ x 1)) (defun main () (let ((i 0) (s 0)) (while (< i 50) (set s (+ s (f i))) (set i (+ i 1))) s))" . 1275)
    ("(defun main () (if (and (< 1 2) (or 0 (> 300 299))) 1000 2000))" . 1000))
  "Programs that run the same on any backend with 16-bit words, and the value main leaves in the accumulator.")

(defparameter +pf-sp+ #xF000)

(defun %pf-run (source backend &optional (optimize :size))
  "Compile, assemble and run SOURCE on pairfoo; returns the machine."
  (let ((m (make-machine 'pairfoo)))
    (load-program m (assemble-items (%cl-compile source backend optimize) :backend backend))
    (setf (sref m 'sp) +pf-sp+)
    (values m (run m :max-steps 400000))))

(defun %pf-word (m)
  (+ (* 256 (regref m 'r 0)) (regref m 'r 1)))

(fiveam:test sixteen-bit-programs-run-on-eight-bit-registers
  (dolist (backend '(pairfoo-lang-abi pairfoo-lang-reg-abi))
    (loop for (source . expected) in +pf-programs+
          do (multiple-value-bind (m reason) (%pf-run source backend)
               (fiveam:is (eq :trap reason) "~A on ~A stopped with ~S" source backend reason)
               (fiveam:is (= expected (%pf-word m)) "~A on ~A" source backend)
               (fiveam:is (= +pf-sp+ (sref m 'sp)) "the stack is balanced: ~A on ~A" source backend)))))

(fiveam:test optimize-speed-holds-values-in-callee-saved-pairs
  (loop for (source . expected) in +pf-programs+
        do (let ((m (%pf-run source 'pairfoo-lang-abi :speed)))
             (fiveam:is (= expected (%pf-word m)) "~A" source)
             (fiveam:is (= +pf-sp+ (sref m 'sp)) "~A" source))))

(fiveam:test static-frames-run-on-eight-bit-registers
  (loop for (source . expected) in '(("(defun square (n) (* n n)) (defun sos (a b) (+ (square a) (square b))) (defun main () (sos 30 40))" . 2500)
                                     ("(defun f (a b c) (let ((x (+ a b))) (* x c))) (defun main () (f 100 200 3))" . 900)
                                     ("(defvar g 5) (defun f (a b) (+ a (* b g))) (defun main () (f (f 1 2) (f 3 4)))" . 126))
        do (let ((m (%pf-run source 'pf-static-abi)))
             (fiveam:is (= expected (%pf-word m)) "~A" source))))

(fiveam:test a-global-and-an-array-lay-out-two-cells-a-word
  (let* ((items (%cl-compile "(defvar x 5) (defarray arr (1 2 3)) (defun main () 1)" 'pairfoo-lang-abi))
         (res (find-if (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "res"))) items)))
    (fiveam:is (= 2 (third res)) "a global reserves a whole word (.res 2)")
    (fiveam:is (every (lambda (i) (eql 2 (third i)))
                      (remove-if-not (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "emit"))) items))
               ".emit's width is the word size")))

;;; Calls

(fiveam:test a-stack-argument-reaches-the-stack-through-a-scratch-pair
  (let ((text (render-items (%cl-compile "(defun f (a b) (+ a b)) (defun main () (f 1 2))" 'pairfoo-lang-abi)
                            :backend 'pairfoo-lang-abi)))
    (fiveam:is (not (search "pushr [" text)) "no push of a slot")
    (fiveam:is (search (format nil "lds a, [ sp + 3 ]~%pushr a~%pushr b~%lds b, [ sp + 2 ]") text)
               "each half goes through a scratch pair, high byte first")))

(fiveam:test a-computed-call-with-a-stack-argument-needs-a-scratch-pair-besides-its-target
  (eval '(defbackend pf-noscratch-abi (:extends pairfoo-lang-reg-abi) (registers :scratch (ab))))
  (let ((c (handler-case (progn (%pf-run "(defun add (a b) (+ a b)) (defvar g 0)
                                          (defun main () (set g (function add)) (funcall g 300 400))"
                                         'pf-noscratch-abi)
                                nil)
             (items-error (c) c))))
    (fiveam:is (typep c 'items-malformed))
    (fiveam:is (and c (search ":scratch word" (princ-to-string c))))))

;;; Inline items

(fiveam:test asm-clobbers-a-half-marks-its-pair
  (let ((*cc-backend* (find-backend 'pairfoo-lang-abi)))
    (fiveam:is (equal '("AB") (%cc-asm-clobbers (list (%cc-symbol "asm") (list :clobbers (%cc-symbol "a"))))))
    (fiveam:is (equal '("AB") (%cc-asm-clobbers (list (%cc-symbol "asm") (list :clobbers (%cc-symbol "ab"))))))
    (fiveam:is (equal '("CD" "AB")
                      (%cc-asm-clobbers (list (%cc-symbol "asm") (list :clobbers (%cc-symbol "d") (%cc-symbol "b"))))))))

;;; The pair backend is a target like any other

(fiveam:test cli-runs-a-program-on-eight-bit-registers
  (multiple-value-bind (status out)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/fact.lsp") "-m" (%cli-path "tests/fixtures/cli/pairfoo.lisp")
                      "--backend" "pairfoo-lang-abi"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

;;; Memory halves: zpfoo-lang-abi (tests/fixtures/cli/zpfoo.lisp) keeps every word in two
;;; zero-page cells.

(fiveam:test a-pair-can-name-memory-cells
  (fiveam:is (equal '(("W0" (16 17) 8) ("W1" (18 19) 8) ("W2" (20 21) 8) ("W3" (48 32) 8))
                    (backend-words 'zpfoo-lang-abi)))
  (fiveam:is (= 2 (backend-word-cells 'zpfoo-lang-abi)))
  (fiveam:is (= 2 (backend-word-cells 'zf-static-abi))))

(defparameter +zf-bad-backends+
  '(("mixes a register and a memory address" (defbackend zf-bad-1 (:isa zpfoo) (registers :words ((w0 a 16)))))
    ("mixes a register and a memory address" (defbackend zf-bad-2 (:isa zpfoo) (registers :words ((w0 16 a)))))
    ("differ in kind" (defbackend zf-bad-3 (:extends zpfoo-lang-abi) (registers :words ((w0 #x11 #x10) (w1 a a)))))
    ("for two parts" (defbackend zf-bad-4 (:extends zpfoo-lang-abi) (registers :words ((w0 16 16)))))
    ("is a part of both" (defbackend zf-bad-5 (:extends zpfoo-lang-abi) (registers :words ((w0 #x11 #x10) (w1 #x10 #x12)))))
    ("is not an address" (defbackend zf-bad-6 (:extends zpfoo-lang-abi) (registers :words ((w0 65536 16)))))
    ("is not an address" (defbackend zf-bad-7 (:extends zpfoo-lang-abi) (registers :words ((w0 -1 16)))))
    ("is already a register" (defbackend zf-bad-8 (:extends zpfoo-lang-abi) (registers :words ((a 17 16))))))
  "Memory-pair backends that fail to define, each with the words its error contains.")

(fiveam:test a-bad-memory-pair-is-a-definition-error
  (loop for (text form) in +zf-bad-backends+
        do (let ((c (%backend-error-of form)))
             (fiveam:is (typep c 'backend-definition-error) "~S" form)
             (fiveam:is (and c (search text (princ-to-string c))) "~S: ~A" form c))))

(defun %zf-expand (op &rest args)
  (%pf-plain (backend-expand-op 'zpfoo-lang-abi op args)))

(fiveam:test a-half-of-a-memory-pair-is-its-address
  (fiveam:is (equal '(("lda" ("zp" 16)) ("sta" ("zp" 18)) ("lda" ("zp" 17)) ("sta" ("zp" 19)))
                    (%zf-expand :move '(zp w1) '(zp w0))))
  (fiveam:is (equal '(("ldw" ("zp" 19) ("zp" 18) ("zp" 17) ("zp" 16)))
                    (%zf-expand :peek 'w1 'w0))
             "a pair name splits into its addresses")
  (fiveam:is (equal '(("lda" ("zp" 48)) ("sta" ("zp" 16)) ("lda" ("zp" 32)) ("sta" ("zp" 17)))
                    (%zf-expand :move '(zp w0) '(zp w3)))
             "the halves need not be adjacent"))

(fiveam:test a-half-of-an-integer-or-label-is-unchanged-on-a-memory-pair
  (fiveam:is (equal '(("ldi" ("imm" 52)) ("sta" ("zp" 16)) ("ldi" ("imm" 18)) ("sta" ("zp" 17)))
                    (%zf-expand :const '(zp w0) 4660)))
  (fiveam:is (equal '(("ldi" ("imm" ("&" "there" 255))) ("sta" ("zp" 16))
                      ("ldi" ("imm" ("&" (">>" "there" 8) 255))) ("sta" ("zp" 17)))
                    (%zf-expand :const '(zp w0) 'there))))

(fiveam:test a-memory-pair-used-whole-is-an-items-error
  (eval '(defbackend zf-whole-abi (:extends zpfoo-lang-abi)
          (ops (:branch-zero (r target) (jzw r target)))))
  (let ((c (handler-case (progn (backend-expand-op 'zf-whole-abi :branch-zero '((zp w0) done)) nil)
             (items-malformed (c) c))))
    (fiveam:is (typep c 'items-malformed))
    (fiveam:is (and c (search "whole" (princ-to-string c))))))

(fiveam:test a-raw-instruction-takes-a-half-of-a-memory-pair
  (let ((by-half (assembly-cells (assemble-items '((lda (:lo (zp w1))) (sta (:hi (zp w3)))) :backend 'zpfoo-lang-abi)))
        (by-address (assembly-cells (assemble-items '((lda (zp 18)) (sta (zp 32))) :backend 'zpfoo-lang-abi))))
    (fiveam:is (equalp by-address by-half)))
  (let ((c (handler-case (progn (assemble-items '((lda (zp w1))) :backend 'zpfoo-lang-abi) nil)
             (items-malformed (c) c))))
    (fiveam:is (typep c 'items-malformed))
    (fiveam:is (and c (search "whole" (princ-to-string c))))))

(fiveam:test asm-clobbers-a-memory-pair-by-name
  (let ((*cc-backend* (find-backend 'zpfoo-lang-abi)))
    (fiveam:is (equal '("W0") (%cc-asm-clobbers (list (%cc-symbol "asm") (list :clobbers (%cc-symbol "w0"))))))
    (fiveam:is (equal '("A") (%cc-asm-clobbers (list (%cc-symbol "asm") (list :clobbers (%cc-symbol "a"))))))))

(defparameter +zf-origin+ #x200)

(defun %zf-run (source backend &optional (optimize :size))
  "Compile, assemble and run SOURCE on zpfoo; returns the machine."
  (let ((m (make-machine 'zpfoo)))
    (load-program m (assemble-items (%cl-compile source backend optimize) :backend backend :origin +zf-origin+))
    (setf (sref m 'sp) +pf-sp+)
    (values m (run m :max-steps 400000))))

(defun %zf-word (m)
  (+ (* 256 (mref m 'ram #x11)) (mref m 'ram #x10)))

(fiveam:test sixteen-bit-programs-run-in-zero-page-pairs
  (dolist (backend '(zpfoo-lang-abi zpfoo-lang-reg-abi))
    (loop for (source . expected) in (remove-if (lambda (entry) (search "asm" (car entry))) +pf-programs+)
          do (multiple-value-bind (m reason) (%zf-run source backend)
               (fiveam:is (eq :trap reason) "~A on ~A stopped with ~S" source backend reason)
               (fiveam:is (= expected (%zf-word m)) "~A on ~A" source backend)
               (fiveam:is (= +pf-sp+ (sref m 'sp)) "the stack is balanced: ~A on ~A" source backend)))))

(fiveam:test an-asm-block-clobbers-a-memory-pair
  (let ((m (%zf-run "(defun main () (let ((x 9)) (asm (:clobbers w0) (:op :const (zp w0) 3) (:op :set (:var x) (zp w0))) x))"
                    'zpfoo-lang-abi)))
    (fiveam:is (= 3 (%zf-word m)))))

(fiveam:test optimize-speed-holds-values-in-callee-saved-memory-pairs
  (loop for (source . expected) in (remove-if (lambda (entry) (search "asm" (car entry))) +pf-programs+)
        do (let ((m (%zf-run source 'zpfoo-lang-abi :speed)))
             (fiveam:is (= expected (%zf-word m)) "~A" source)
             (fiveam:is (= +pf-sp+ (sref m 'sp)) "~A" source))))

(fiveam:test static-frames-run-in-zero-page-pairs
  (loop for optimize in '(:size :speed)
        do (loop for (source . expected) in '(("(defun square (n) (* n n)) (defun sos (a b) (+ (square a) (square b))) (defun main () (sos 30 40))" . 2500)
                                              ("(defun f (a b c) (let ((x (+ a b))) (* x c))) (defun main () (f 100 200 3))" . 900)
                                              ("(defvar g 5) (defun f (a b) (+ a (* b g))) (defun main () (f (f 1 2) (f 3 4)))" . 126))
                 do (let ((m (%zf-run source 'zf-static-abi optimize)))
                      (fiveam:is (= expected (%zf-word m)) "~A ~A" optimize source)))))

;;; Arguments that read each other's pairs are ordered by the pair they name.

(fiveam:test call-arguments-that-swap-memory-pairs-are-ordered
  (eval '(defbackend zf-swap-abi (:extends zpfoo-lang-abi)
          (registers :scratch (w0 w3) :caller-saved (w1 w2) :callee-saved ())
          (call :args (w1 w2) :order :left-to-right :cleanup :caller :return-address-slots 1)))
  (let* ((items '((:op :const (zp w1) 10) (:op :const (zp w2) 3)
                  (:call sub2 (zp w2) (zp w1))
                  (:op :halt)
                  (:function sub2 (:args 2)
                    (:op :move (zp w0) (:arg 0))
                    (:op :sub (zp w0) (:arg 1))
                    (:return))))
         (m (make-machine 'zpfoo)))
    (load-program m (assemble-items items :backend 'zf-swap-abi :origin +zf-origin+))
    (setf (sref m 'sp) +pf-sp+)
    (run m :max-steps 1000)
    (fiveam:is (= 65529 (%zf-word m)) "sub2 gets 3 and 10, so it returns 3 - 10")))

;;; A call argument that is an integer or a label is a value, loaded with :const (#452).

(defparameter +value-call-specs+
  '((pairfoo pairfoo-lang-abi reg ab cd %pf-word)
    (pairfoo pairfoo-lang-reg-abi reg ab cd %pf-word)
    (zpfoo zpfoo-lang-abi zp w0 w1 %zf-word)
    (zpfoo zpfoo-lang-reg-abi zp w0 w1 %zf-word))
  "(MACHINE BACKEND KIND RESULT OTHER READER): the reg-abi backends pass the first argument in OTHER,
the others on the stack.")

(defun %value-call (spec prefix call &optional (backend (second spec)))
  "Run PREFIX, then CALL, a call of sub2 returning its first argument less its second, on SPEC's machine.
Returns the result word and the assembly."
  (destructuring-bind (name ignored kind result other reader) spec
    (declare (ignore ignored))
    (let* ((items `(,@prefix ,call (:op :halt)
                    (:function sub2 (:args 2)
                      (:op :move (,kind ,result) (:arg 0))
                      (:op :move (,kind ,other) (:arg 1))
                      (:op :sub (,kind ,result) (,kind ,other))
                      (:return))))
           (assembly (assemble-items items :backend backend :origin #x300))
           (m (make-machine name)))
      (load-program m assembly)
      (setf (sref m 'sp) +pf-sp+)
      (run m :max-steps 1000)
      (values (funcall reader m) assembly (sref m 'sp)))))

(fiveam:test a-call-argument-that-is-an-integer-is-a-value
  (loop for spec in +value-call-specs+
        do (loop for (a b expected) in '((5 7 65534) (-2 1 65533) (300 44 256) (1000 1 999))
                 do (multiple-value-bind (word assembly sp) (%value-call spec '() `(:call sub2 ,a ,b))
                      (declare (ignore assembly))
                      (fiveam:is (= expected word) "~A (:call sub2 ~A ~A) gave ~A" (second spec) a b word)
                      (fiveam:is (= +pf-sp+ sp) "~A leaves the stack balanced" (second spec))))))

(fiveam:test a-call-argument-that-is-a-label-or-expression-is-a-value
  (loop for spec in +value-call-specs+
        do (let ((address (symbol-info-value
                           (assembly-symbol (nth-value 1 (%value-call spec '() '(:call sub2 0 0))) "sub2"))))
             (fiveam:is (> address 255) "the label lies above the low half")
             (fiveam:is (= (- address 3) (%value-call spec '() '(:call sub2 sub2 3)))
                        "~A passes a label" (second spec))
             (fiveam:is (= (+ address 4) (%value-call spec '() '(:call sub2 (+ sub2 10) 6)))
                        "~A passes an expression" (second spec)))))

(fiveam:test a-value-argument-is-loaded-after-the-moves-that-read-its-register
  (loop for spec in +value-call-specs+
        do (multiple-value-bind (kind other) (values (third spec) (fifth spec))
             (fiveam:is (= 65531 (%value-call spec `((:op :const (,kind ,other) 10)) `(:call sub2 5 (,kind ,other))))
                        "~A pushes the pair before the value overwrites it" (second spec))))
  (eval '(defbackend zf-swap-abi (:extends zpfoo-lang-abi)
          (registers :scratch (w0 w3) :caller-saved (w1 w2) :callee-saved ())
          (call :args (w1 w2) :order :left-to-right :cleanup :caller :return-address-slots 1)))
  (fiveam:is (= 65531 (%value-call (fourth +value-call-specs+) '((:op :const (zp w1) 10)) '(:call sub2 5 (zp w1)) 'zf-swap-abi))
             "w2 takes w1 before w1 takes 5"))

(fiveam:test a-value-argument-does-not-disturb-a-computed-call-target
  (loop for spec in (list (second +value-call-specs+) (fourth +value-call-specs+))
        do (destructuring-bind (name backend kind result other reader) spec
             (declare (ignore name backend result reader))
             (fiveam:is (= 2 (%value-call spec `((:op :const (,kind ,other) sub2)) `(:call (,kind ,other) 5 3)))
                        "~A calls through the pair the first argument is loaded into" (second spec)))))

(fiveam:test a-stack-value-is-staged-through-a-scratch-pair-no-argument-reads
  (loop for spec in (list (second +value-call-specs+) (fourth +value-call-specs+))
        do (destructuring-bind (name backend kind result other reader) spec
             (declare (ignore name backend other reader))
             (fiveam:is (= 993 (%value-call spec `((:op :const (,kind ,result) 1000)) `(:call sub2 (,kind ,result) 7)))
                        "~A keeps the register argument out of the staging pair" (second spec)))))

(fiveam:test a-stack-value-without-a-free-scratch-pair-is-an-items-error
  (eval '(defbackend pf-tight-abi (:extends pairfoo-lang-reg-abi) (registers :scratch (ab))))
  (let ((c (handler-case (progn (%value-call (second +value-call-specs+) '((:op :const (reg ab) 1))
                                             '(:call sub2 (reg ab) 7) 'pf-tight-abi)
                                nil)
             (items-malformed (c) c))))
    (fiveam:is (typep c 'items-malformed))
    (fiveam:is (and c (search "value" (princ-to-string c))))))

;;; A frame operand of an :op dispatches as what it addresses (#458).

(fiveam:test a-frame-operand-of-an-op-selects-the-clause-for-its-kind
  (loop for spec in +value-call-specs+
        do (fiveam:is (= 65534 (%value-call spec '() '(:call sub2 5 7)))
                      "~A: :move from an argument on the stack or in a register" (second spec)))
  (dolist (spec (list (first +value-call-specs+) (third +value-call-specs+)))
    (destructuring-bind (name backend kind result other reader) spec
      (declare (ignore other))
      (let ((m (make-machine name)))
        (load-program m (assemble-items `((:function getloc (:locals 1)
                                            (:op :const (,kind ,result) 42)
                                            (:op :set (:local 0) (,kind ,result))
                                            (:op :const (,kind ,result) 0)
                                            (:op :move (,kind ,result) (:local 0))
                                            (:return))
                                          (:call getloc) (:op :halt))
                                        :backend backend :origin #x300))
        (setf (sref m 'sp) +pf-sp+)
        (run m :max-steps 1000)
        (fiveam:is (= 42 (funcall reader m)) "~A: :move from a local" backend)))))

;;; Words of four registers: quadfoo-lang-abi (tests/fixtures/cli/quadfoo.lisp) keeps every
;;; 32-bit language word in four 8-bit registers.

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "tests/fixtures/cli/quadfoo.lisp")))

(eval '(defbackend qf-static-abi (:extends quadfoo-lang-abi) (frame :static t)))

(fiveam:test a-backend-declares-words-of-four-registers
  (fiveam:is (equal '(("WA" ("D" "C" "B" "A") 8) ("WB" ("H" "G" "F" "E") 8)
                      ("WC" ("L" "K" "J" "I") 8) ("WD" ("P" "O" "N" "M") 8))
                    (backend-words 'quadfoo-lang-abi)))
  (fiveam:is (= 4 (backend-word-cells 'quadfoo-lang-abi)))
  (fiveam:is (= 4 (backend-word-cells 'qf-static-abi))))

(defparameter +qf-bad-backends+
  '(("has 3 parts, not 4" (defbackend qf-bad-1 (:extends quadfoo-lang-abi) (registers :words ((wa a b c d) (wb e f g)))))
    ("is a part of both" (defbackend qf-bad-2 (:extends quadfoo-lang-abi) (registers :words ((wa a b c d) (wb e f g a)))))
    ("for two parts" (defbackend qf-bad-3 (:extends quadfoo-lang-abi) (registers :words ((wa a b c c)))))
    ("expected (NAME PART PART...)" (defbackend qf-bad-4 (:extends quadfoo-lang-abi) (registers :words ((wa a)))))
    ("the part must be an integer from 0 to 3" (defbackend qf-bad-5 (:extends quadfoo-lang-abi) (ops (:tag (x) (movv (:part 4 x) (:lo x))))))
    ("the part must be an integer from 0 to 3" (defbackend qf-bad-6 (:extends quadfoo-lang-abi) (ops (:tag (x) (movv (:part -1 x) (:lo x))))))
    ("the part must be an integer from 0 to 3" (defbackend qf-bad-7 (:extends quadfoo-lang-abi) (ops (:tag (x) (movv (:part k x) (:lo x))))))
    ("needs (registers :words" (defbackend qf-bad-8 (:extends callfoo-abi) (ops (:tag (x) (movv (:part 1 x) (:lo x))))))
    ("not both" (defbackend qf-bad-9 (:extends quadfoo-lang-abi) (call :return-address-slots 1 :return-address-cells 2)))
    ("must be a non-negative integer" (defbackend qf-bad-10 (:extends quadfoo-lang-abi) (call :return-address-cells -1)))
    ("needs (frame :offsets :cells)" (defbackend qf-bad-11 (:extends quadfoo-lang-abi) (frame :offsets :slots :counts :slots))))
  "Backends that fail to define, each with the words its error contains.")

(fiveam:test a-bad-word-of-four-registers-is-a-definition-error
  (loop for (text form) in +qf-bad-backends+
        do (let ((c (%backend-error-of form)))
             (fiveam:is (typep c 'backend-definition-error) "~S" form)
             (fiveam:is (and c (search text (princ-to-string c))) "~S: ~A" form c))))

(defun %qf-expand (op &rest args)
  (%pf-plain (backend-expand-op 'quadfoo-lang-abi op args)))

(fiveam:test a-part-of-an-integer-is-masked-to-the-part
  (fiveam:is (equal '(("ldi" ("reg" "d") ("imm" 112)) ("ldi" ("reg" "c") ("imm" 17))
                      ("ldi" ("reg" "b") ("imm" 1)) ("ldi" ("reg" "a") ("imm" 0)))
                    (%qf-expand :const '(reg wa) 70000)))
  (fiveam:is (equal '(("ldi" ("reg" "d") ("imm" 255)) ("ldi" ("reg" "c") ("imm" 255))
                      ("ldi" ("reg" "b") ("imm" 255)) ("ldi" ("reg" "a") ("imm" 255)))
                    (%qf-expand :const '(reg wa) -1))))

(fiveam:test a-part-of-a-label-is-an-expression
  (fiveam:is (equal '(("ldi" ("reg" "d") ("imm" ("&" "there" 255)))
                      ("ldi" ("reg" "c") ("imm" ("&" (">>" "there" 8) 255)))
                      ("ldi" ("reg" "b") ("imm" ("&" (">>" "there" 16) 255)))
                      ("ldi" ("reg" "a") ("imm" ("&" (">>" "there" 24) 255))))
                    (%qf-expand :const '(reg wa) 'there))))

(fiveam:test a-part-of-a-register-word-is-that-register
  (fiveam:is (equal '(("movv" ("reg" "h") ("reg" "d")) ("movv" ("reg" "g") ("reg" "c"))
                      ("movv" ("reg" "f") ("reg" "b")) ("movv" ("reg" "e") ("reg" "a")))
                    (%qf-expand :move '(reg wb) '(reg wa)))))

(fiveam:test a-part-of-a-frame-slot-waits-for-its-offset
  (let ((forms (backend-expand-op 'quadfoo-lang-abi :get '((reg wa) (:local 0)))))
    (fiveam:is (equal '((:part 0 (:local 0)) (:part 1 (:local 0)) (:part 2 (:local 0)) (:part 3 (:local 0)))
                      (mapcar #'third forms)))))

(fiveam:test a-resolved-slot-splits-in-endian-order-across-four-cells
  (fiveam:is (equal '(("lds" ("reg" "d") ("sp-idx" 6)) ("lds" ("reg" "c") ("sp-idx" 7))
                      ("lds" ("reg" "b") ("sp-idx" 8)) ("lds" ("reg" "a") ("sp-idx" 9)))
                    (%qf-expand :move '(reg wa) '(sp-idx 6)))))

(eval '(defmachine qf-be-machine
         (register pc :width 16) (register sp :width 16)
         (register r :width 8 :names (a b c d))
         (memory ram :width 8 :addr-width 16 :endian :big)
         (stack-pointer sp :memory ram :grows :down)))
(eval '(defmode qf-be-reg (expr :register r)))
(eval '(defmode qf-be-slot "[" "sp" "+" expr "]"))
(eval '(defbackend qf-be-abi (:isa qf-be-machine)
        (registers :words ((wa a b c d)) :return (wa) :stack-pointer sp :operand reg)
        (frame :grows :down :slot slot :offsets :cells :counts :cells)
        (operands (reg qf-be-reg) (slot qf-be-slot))))

(fiveam:test a-big-endian-word-has-its-most-significant-part-in-the-lowest-cell
  (let ((*items-backend* (find-backend 'qf-be-abi)))
    (fiveam:is (equal '(("slot" 7) ("slot" 6) ("slot" 5) ("slot" 4))
                      (loop for k below 4 collect (%pf-plain (%frame-part k '(slot 4) nil))))))
  (let ((*items-backend* (find-backend 'quadfoo-lang-abi)))
    (fiveam:is (equal '(("sp-idx" 4) ("sp-idx" 5) ("sp-idx" 6) ("sp-idx" 7))
                      (loop for k below 4 collect (%pf-plain (%frame-part k '(sp-idx 4) nil)))))))

(defparameter +qf-programs+
  '(("(defun main () (+ 200 100))" . 300)
    ("(defun main () (+ 65535 1))" . 65536)
    ("(defun main () (* 70000 3))" . 210000)
    ("(defun main () (- 5))" . 4294967291)
    ("(defun main () (- 65536 1))" . 65535)
    ("(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (defun main () (fact 10))" . 3628800)
    ("(defun main () (+ (< 3 5) (>= 3 5) (= 4 4) (/= 4 4) (<= 4 4) (> 2 1)))" . 4)
    ("(defun main () (+ (< (- 1) 2) (< 70000 60000) (> 70000 60000) (= 65536 65536) (= 65536 256)))" . 3)
    ("(defvar x 100000) (defvar y 200000) (defun main () (+ x y))" . 300000)
    ("(defarray arr (10 200000 3000000)) (defun main () (+ (aref arr 0) (+ (aref arr 1) (aref arr 2))))" . 3200010)
    ("(defarray arr (10 20 30)) (defun main () (let ((i 2)) (aset arr i 999999) (aref arr 2)))" . 999999)
    ("(defstring s \"hi\") (defun main () (+ (aref s 0) (aref s 1)))" . 209)
    ("(defun main () (poke 8192 7000000) (poke 8196 (+ (peek 8192) 1)) (peek 8196))" . 7000001)
    ("(defun f (a b c d) (- (* a d) (- b c))) (defun main () (f 60000 1 2 70000))" . 4200000001)
    ("(defun add (a b) (+ a b)) (defvar g 0) (defun main () (set g (function add)) (funcall g 300000 400000))" . 700000)
    ("(defun main () (/ 1000000 7))" . 142857)
    ("(defun main () (+ (mod 1000000 7) (shl 3 20)))" . 3145729)
    ("(defun main () (+ (logand #x12345678 #xff00ff) (logior #x10000 1) (logxor #xffffffff #xff) (shr #x80000000 4)))" . 137691001))
  "Programs that need 32-bit words, and the value main leaves in the accumulator.")

(defun %qf-run (source backend &optional (optimize :size))
  "Compile, assemble and run SOURCE on quadfoo; returns the machine."
  (let ((m (make-machine 'quadfoo)))
    (load-program m (assemble-items (%cl-compile source backend optimize) :backend backend))
    (setf (sref m 'sp) +pf-sp+)
    (values m (run m :max-steps 800000))))

(defun %qf-word (m)
  (loop for k below 4 sum (ash (regref m 'r k) (* 8 (- 3 k)))))

(fiveam:test thirty-two-bit-programs-run-on-eight-bit-registers
  (dolist (backend '(quadfoo-lang-abi quadfoo-lang-reg-abi qf-static-abi))
    (loop for (source . expected) in +qf-programs+
          do (multiple-value-bind (m reason) (%qf-run source backend)
               (fiveam:is (eq :trap reason) "~A on ~A stopped with ~S" source backend reason)
               (fiveam:is (= expected (%qf-word m)) "~A on ~A" source backend)
               (fiveam:is (= +pf-sp+ (sref m 'sp)) "the stack is balanced: ~A on ~A" source backend)))))

(fiveam:test a-global-and-an-array-lay-out-four-cells-a-word
  (let* ((items (%cl-compile "(defvar x 5) (defarray arr (1 2 3)) (defun main () 1)" 'quadfoo-lang-abi))
         (res (find-if (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "res"))) items)))
    (fiveam:is (= 4 (third res)) "a global reserves a whole word (.res 4)")
    (fiveam:is (every (lambda (i) (eql 4 (third i)))
                      (remove-if-not (lambda (i) (and (consp i) (eq (first i) :directive) (%same-name-p (second i) "emit"))) items))
               ".emit's width is the word size")))

(fiveam:test a-stack-argument-clears-the-two-cell-return-address
  (let ((text (render-items (%cl-compile "(defun f (a b) (+ a b)) (defun main () (f 1 2))" 'quadfoo-lang-abi)
                            :backend 'quadfoo-lang-abi)))
    (fiveam:is (search (format nil "lds d, [ sp + 2 ]~%lds c, [ sp + 3 ]") text)
               "the first argument is 2 cells up, past the return address, not 4 past a word")
    (fiveam:is (search "lds h, [ sp + 6 ]" text) "the second argument follows the first word")))

(fiveam:test cli-runs-a-thirty-two-bit-program-on-eight-bit-registers
  (multiple-value-bind (status out)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/fact32.lsp") "-m" (%cli-path "tests/fixtures/cli/quadfoo.lisp")))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped" out))))

;;; Four zero-page cells a word

(eval '(defbackend zf-quad-abi (:extends zpfoo-lang-abi)
        (registers :words ((w0 #x13 #x12 #x11 #x10) (w1 #x17 #x16 #x15 #x14)) :return (w0) :scratch (w0 w1) :callee-saved ())))

(fiveam:test a-raw-instruction-takes-any-part-of-a-four-cell-memory-word
  (fiveam:is (= 4 (backend-word-cells 'zf-quad-abi)))
  (let ((by-part (assembly-cells (assemble-items '((lda (:part 3 (zp w0))) (sta (:part 1 (zp w1))) (lda (:lo (zp w1))) (sta (:hi (zp w1))))
                                                 :backend 'zf-quad-abi)))
        (by-address (assembly-cells (assemble-items '((lda (zp 19)) (sta (zp 21)) (lda (zp 20)) (sta (zp 23))) :backend 'zf-quad-abi))))
    (fiveam:is (equalp by-address by-part))))
