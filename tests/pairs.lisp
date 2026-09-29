;;;; tests/pairs.lisp
;;;; #416: register-pair words. pairfoo-lang-abi (tests/fixtures/cli/pairfoo.lisp) keeps
;;;; every 16-bit language word in two 8-bit registers.

(in-package #:lasm)

(fiveam:def-suite pairs :in lasm)
(fiveam:in-suite pairs)

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "tests/fixtures/cli/pairfoo.lisp")))

(eval '(defbackend pf-static-abi (:extends pairfoo-lang-abi) (frame :static t)))

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "tests/fixtures/cli/zpfoo.lisp")))

(eval '(defbackend zf-static-abi (:extends zpfoo-lang-abi) (frame :static t)))

;;; Definition

(fiveam:test a-backend-declares-register-pairs
  (fiveam:is (equal '(("AB" "A" "B" 8) ("CD" "C" "D" 8) ("EF" "E" "F" 8) ("GH" "G" "H" 8))
                    (backend-pairs 'pairfoo-lang-abi)))
  (fiveam:is (null (backend-pairs 'callfoo-lang-abi)))
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
  '(("is already a register" (defbackend pf-bad-1 (:extends pairfoo-lang-abi) (registers :pairs ((a c d)))))
    ("is already a register" (defbackend pf-bad-2 (:extends pairfoo-lang-abi) (registers :pairs ((ab a b) (ab c d)))))
    ("is a half of both" (defbackend pf-bad-3 (:extends pairfoo-lang-abi) (registers :pairs ((ab a b) (xy b c)))))
    ("both halves" (defbackend pf-bad-4 (:extends pairfoo-lang-abi) (registers :pairs ((ab a a)))))
    ("not a register" (defbackend pf-bad-5 (:extends pairfoo-lang-abi) (registers :pairs ((ab a nosuch)))))
    ("expected (NAME HIGH LOW)" (defbackend pf-bad-6 (:extends pairfoo-lang-abi) (registers :pairs ((ab a)))))
    ("not a register pair" (defbackend pf-bad-7 (:extends pairfoo-lang-abi) (registers :scratch (ab c))))
    ("not a register pair" (defbackend pf-bad-8 (:extends pairfoo-lang-abi) (registers :callee-saved (e))))
    ("not a register pair" (defbackend pf-bad-9 (:extends pairfoo-lang-abi) (call :args (c))))
    ("frame :slot" (defbackend pf-bad-10 (:extends pairfoo-lang-abi) (frame :offsets :slots)))
    ("needs (registers :pairs" (defbackend pf-bad-11 (:extends callfoo-abi) (ops (:tag (x) (movv (:lo x) (:hi x)))))))
  "Backends that fail to define, each with the words its error contains.")

(fiveam:test a-bad-register-pair-is-a-definition-error
  (loop for (text form) in +pf-bad-backends+
        do (let ((c (%backend-error-of form)))
             (fiveam:is (typep c 'backend-definition-error) "~S" form)
             (fiveam:is (and c (search text (princ-to-string c))) "~S: ~A" form c))))

(fiveam:test pairs-need-halves-of-one-width
  (let ((c (%backend-error-of '(defbackend pf-bad-width (:isa pf-mixed-machine)
                                (registers :pairs ((ab a w)) :return (ab) :operand reg)))))
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
    (fiveam:is (equal '(:lo (:local 0)) (third (first forms))))
    (fiveam:is (equal '(:hi (:local 0)) (third (second forms))))))

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
        (registers :pairs ((ab a b)) :return (ab) :stack-pointer sp :operand reg)
        (frame :grows :down :slot slot :offsets :cells :counts :cells)
        (operands (reg pf-be-reg) (slot pf-be-slot))
        (ops (:get (r s) (lds (:lo r) (:lo s)) (lds (:hi r) (:hi s))))))

(fiveam:test a-big-endian-word-has-its-high-half-in-the-lower-cell
  (fiveam:is (= 2 (backend-word-cells 'pf-be-abi)))
  (let ((*items-backend* (find-backend 'pf-be-abi)))
    (fiveam:is (equal '("slot" 4) (%pf-plain (%frame-half :hi '(slot 4) nil))))
    (fiveam:is (equal '("slot" 5) (%pf-plain (%frame-half :lo '(slot 4) nil)))))
  (let ((*items-backend* (find-backend 'pairfoo-lang-abi)))
    (fiveam:is (equal '("sp-idx" 5) (%pf-plain (%frame-half :hi '(sp-idx 4) nil)))
               "little-endian: the high half is the higher cell")
    (fiveam:is (equal '("sp-idx" 4) (%pf-plain (%frame-half :lo '(sp-idx 4) nil))))))

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
    (fiveam:is (and c (search ":scratch pair" (princ-to-string c))))))

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
  (fiveam:is (equal '(("W0" 17 16 8) ("W1" 19 18 8) ("W2" 21 20 8) ("W3" 32 48 8))
                    (backend-pairs 'zpfoo-lang-abi)))
  (fiveam:is (= 2 (backend-word-cells 'zpfoo-lang-abi)))
  (fiveam:is (= 2 (backend-word-cells 'zf-static-abi))))

(defparameter +zf-bad-backends+
  '(("mixes a register and a memory address" (defbackend zf-bad-1 (:isa zpfoo) (registers :pairs ((w0 a 16)))))
    ("mixes a register and a memory address" (defbackend zf-bad-2 (:isa zpfoo) (registers :pairs ((w0 16 a)))))
    ("differ in kind" (defbackend zf-bad-3 (:extends zpfoo-lang-abi) (registers :pairs ((w0 #x11 #x10) (w1 a a)))))
    ("both halves" (defbackend zf-bad-4 (:extends zpfoo-lang-abi) (registers :pairs ((w0 16 16)))))
    ("is a half of both" (defbackend zf-bad-5 (:extends zpfoo-lang-abi) (registers :pairs ((w0 #x11 #x10) (w1 #x10 #x12)))))
    ("is not an address" (defbackend zf-bad-6 (:extends zpfoo-lang-abi) (registers :pairs ((w0 65536 16)))))
    ("is not an address" (defbackend zf-bad-7 (:extends zpfoo-lang-abi) (registers :pairs ((w0 -1 16)))))
    ("is already a register" (defbackend zf-bad-8 (:extends zpfoo-lang-abi) (registers :pairs ((a 17 16))))))
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
