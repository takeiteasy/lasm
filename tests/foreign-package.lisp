;;;; tests/foreign-package.lisp
;;;; A machine defined outside #:lasm, in a package that only USEs it, with
;;;; every clause a DCPU-16-style machine needs.

(in-package #:lasm)

(fiveam:def-suite foreign-package :in lasm)
(fiveam:in-suite foreign-package)

(defpackage #:lasm-foreign-test
  (:use #:cl #:lasm)
  (:shadowing-import-from #:lasm #:push #:pop))

(in-package #:lasm-foreign-test)

(defmachine fpm
  (register reg :width 16 :names (a b c x y z i j))
  (register pc :width 16)
  (register sp :width 16)
  (register ia :width 16)
  (flags queueing)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (stack-pointer sp :memory ram :grows :down)
  (interrupts :vector ia :message (reg 0) :save (pc (reg 0)) :stack sp
              :queue 16 :on-overflow :trap :mask-flag queueing :mask-on-deliver t)
  (instruction-word :width 16
    (field av 6)
    (field bv 5)
    (field opcode 5)
    (extra-word-order av bv))
  (clock-speed 1000)
  (undefined-opcode :fault)
  (device ticker :id 7 :version 1 :manufacturer 9 :receive fpm-ticker-receive))

(defun fpm-ticker-receive (machine device)
  (declare (ignore device))
  (setf (regref machine 'reg 2) 42))

(defmode fpm-pop "pop")
(defmode fpm-push "push")
(defmode fpm-reg (expr :register reg))
(defmode fpm-idx "[" (expr :register reg) "+" expr "]")
(defmode fpm-lit expr)
(defmode fpm-common (one-of fpm-reg fpm-idx fpm-lit))
(defmode fpm-a (one-of (a-slot fpm-pop fpm-common)))
(defmode fpm-ba
  (one-of (b-slot fpm-push fpm-common)) ","
  (one-of (a-slot fpm-pop fpm-common)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fpm-operands (slot field reg idx off lit)
    `((for-choice (,slot fpm-common fpm-reg)
        (operand ,reg :field ,field
          (variant (choice (fpm-common fpm-reg)) inline :range (0 7) :bias 0)))
      (for-choice (,slot fpm-common fpm-idx)
        (operand ,idx :field ,field
          (variant (choice (fpm-common fpm-idx)) inline :range (0 7) :bias #x10))
        (operand ,off :trailing-word))
      (for-choice (,slot fpm-common fpm-lit)
        (operand ,lit :field ,field
          ,@(if (eq slot 'b-slot)
                `((variant (choice (fpm-common fpm-lit)) (extra-word :escape #x1f)))
                `((variant (choice (fpm-common fpm-lit)) inline :range (-1 30) :bias 33)
                  (variant (choice (fpm-common fpm-lit)) (extra-word :escape #x1f) :suffix "w"))))))))

(defmacro fpm-read (slot reg idx off lit)
  `(choice-case ,slot
     ,@(if (eq slot 'a-slot) '((fpm-pop (pop sp))) '())
     (fpm-common
      (choice-case (,slot fpm-common)
        (fpm-reg (regref machine 'reg ,reg))
        (fpm-idx (mref machine 'ram (wrap-value (+ (regref machine 'reg ,idx) ,off) 16)))
        (fpm-lit ,lit)))))

(defmacro fpm-write (slot reg idx off value)
  `(choice-case ,slot
     ,@(if (eq slot 'b-slot) `((fpm-push (push ,value sp))) '())
     (fpm-common
      (choice-case (,slot fpm-common)
        (fpm-reg (setf (regref machine 'reg ,reg) ,value))
        (fpm-idx (setf (mref machine 'ram (wrap-value (+ (regref machine 'reg ,idx) ,off) 16)) ,value))
        (fpm-lit nil)))))

(defmacro def-fpm-basic (name opcode &body semantics)
  `(definstruction fpm ,name
     (modes fpm-ba)
     (encoding
       (opcode ,opcode)
       ,@(fpm-operands 'b-slot 'bv 'breg 'bidx 'boff 'blit)
       ,@(fpm-operands 'a-slot 'av 'areg 'aidx 'aoff 'alit)
       (for-choice (b-slot fpm-push) (field-value bv #x18))
       (for-choice (a-slot fpm-pop) (field-value av #x18)))
     (semantics ,@semantics)))

(defmacro def-fpm-special (name code &body semantics)
  `(definstruction fpm ,name
     (modes fpm-a)
     (encoding
       (opcode 0)
       (field-value bv ,code)
       ,@(fpm-operands 'a-slot 'av 'areg 'aidx 'aoff 'alit)
       (for-choice (a-slot fpm-pop) (field-value av #x18)))
     (semantics ,@semantics)))

(def-fpm-basic set 1
  (let ((value (fpm-read a-slot areg aidx aoff alit)))
    (fpm-write b-slot breg bidx boff value)))

(def-fpm-special hwi 1
  (device-send machine (fpm-read a-slot areg aidx aoff alit)))

(definstruction fpm halt
  (encoding (opcode 2))
  (semantics (trap :halt)))

(deflexer fpm-colon
  (comment-styles (";" :line))
  (number-formats (:hex "0x") (:dec :default))
  (label-suffix ":")
  (string-delim "\"")
  (ident-chars :alnum "_"))

(defmachine fpm8
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16))

;; Mode names that are also DSL words stay the user's own.
(defmode register "r" expr)
(defmode memory "[" expr "]")
(defmode stack "#" expr)

(definstruction fpm8 ld
  (modes (register (opcode 1) (operand :mode))
         (memory (opcode 2) (operand :mode))
         (stack (opcode 3) (operand :mode)))
  (semantics (set! a operand)))

(in-package #:lasm)

(defun %foreign-run (source)
  (let ((machine (make-machine 'lasm-foreign-test::fpm)))
    (load-program machine (assemble source :cpu 'lasm-foreign-test::fpm))
    (values machine (run machine))))

(fiveam:test foreign-package-machine-assembles-and-runs
  (multiple-value-bind (m reason)
      (%foreign-run "set a, 5
set b, 1000
set x, 0x2000
set [x+3], b
set push, a
set y, pop
halt")
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 5 (regref m 'lasm-foreign-test::reg 0)))
    (fiveam:is (= 1000 (mref m 'lasm-foreign-test::ram #x2003)))
    (fiveam:is (= 5 (regref m 'lasm-foreign-test::reg 4)))))

(fiveam:test foreign-package-machine-encodes-like-dcpu
  (fiveam:is (equalp #(#x9801 #x7c21 1000)
                     (assembly-cells (assemble "set a, 5
set b, 1000" :cpu 'lasm-foreign-test::fpm)))))

(fiveam:test foreign-package-device-and-interrupt
  (multiple-value-bind (m reason) (%foreign-run "hwi 0
halt")
    (fiveam:is (eq :trap reason))
    (fiveam:is (= 42 (regref m 'lasm-foreign-test::reg 2))))
  (let ((m (make-machine 'lasm-foreign-test::fpm)))
    (load-program m (assemble "halt" :cpu 'lasm-foreign-test::fpm))
    (setf (sref m 'lasm-foreign-test::ia) #x0100
          (sref m 'lasm-foreign-test::sp) #x8000)
    (signal-interrupt m 9)
    (step-machine m)
    (fiveam:is (= #x0100 (sref m 'pc)))
    (fiveam:is (= 9 (regref m 'lasm-foreign-test::reg 0)))))

(fiveam:test foreign-package-lexer
  (fiveam:is (typep (find-lexer-descriptor 'lasm-foreign-test::fpm-colon) 'lexer-descriptor)))

(fiveam:test foreign-package-mode-names-may-be-dsl-words
  (fiveam:is (equalp #(1 5 0 2 6 0 3 7 0)
                     (assembly-cells (assemble "ld r 5
ld [6]
ld #7" :cpu 'lasm-foreign-test::fpm8)))))

(fiveam:test foreign-package-debugger-names-its-storage
  (let* ((assembly (assemble "set push, 5
halt" :cpu 'lasm-foreign-test::fpm))
         (machine (make-machine 'lasm-foreign-test::fpm)))
    (load-program machine assembly)
    (let ((session (make-debug-session machine :assembly assembly)))
      (debug-command session "set sp = 100")
      (fiveam:is (= 100 (sref machine 'lasm-foreign-test::sp)))
      (fiveam:is (search "pc = 0" (debug-command session "print pc")))
      (debug-watch session "sp" :access :write)
      (fiveam:is (eq :watchpoint (debug-continue session))))))

(fiveam:test foreign-package-machine-is-found-by-name
  (fiveam:is (eq 'lasm-foreign-test::fpm (%table-key-named *machines* "fpm")))
  (fiveam:is (eq 'lasm-foreign-test::fpm (%table-key-named *machines* "FPM")))
  (fiveam:is (null (%table-key-named *machines* "no-such-machine"))))
