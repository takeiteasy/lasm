;;;; test.lisp

(defpackage #:dcpu16/test
  (:use #:cl #:lasm #:dcpu16)
  (:shadowing-import-from #:lasm #:push #:pop)
  (:export #:run-tests))

(in-package #:dcpu16/test)

(fiveam:def-suite dcpu16-tests)
(fiveam:in-suite dcpu16-tests)

(defun run-tests ()
  (fiveam:run! 'dcpu16-tests))

(defun cells (source)
  (coerce (assembly-cells (assemble source :machine 'dcpu16)) 'list))

(defun run-source (source &key (max-steps 10000))
  "A machine that has run SOURCE until it halts on a jump to itself."
  (let ((machine (load-dcpu16 (format nil "~A~%halt: set pc, halt" source))))
    (run-dcpu16 machine :max-steps max-steps)
    machine))

(defun regs (machine &rest names)
  (mapcar (lambda (name) (reg-value machine name)) names))

(defun ex (machine) (sref machine 'ex))
(defun sp (machine) (sref machine 'sp))
(defun ram (machine address) (mref machine 'ram address))

(defun cycles-of (source)
  "The cycles of the first instruction of SOURCE."
  (let ((machine (load-dcpu16 source)))
    (nth-value 1 (step-machine machine))))

(fiveam:test machine-loads
  (fiveam:is (typep (make-machine 'dcpu16) 'machine)))

;;; Encoding: each row is one line of the spec's value table.

(fiveam:test operand-encodings
  (loop for (source . expected) in
        '(("set a, b" #x0401)
          ("set a, [b]" #x2401)
          ("set a, [b + 5]" #x4401 5)
          ("set a, pop" #x6001)
          ("set push, a" #x0301)
          ("set a, peek" #x6401)
          ("set a, pick 3" #x6801 3)
          ("set a, sp" #x6c01)
          ("set a, pc" #x7001)
          ("set a, ex" #x7401)
          ("set a, [0x1000]" #x7801 #x1000)
          ("set a, 0x1000" #x7c01 #x1000)
          ("set a, -1" #x8001)
          ("set a, 30" #xfc01)
          ("set a, 31" #x7c01 31)
          ("set [a + 3], 5" #x9a01 3)
          ("set 5, a" #x03e1 5)
          ("set [x + 1], [y + 2]" #x5261 2 1))
        do (fiveam:is (equal expected (cells source)) "~A" source)))

(fiveam:test basic-opcodes
  (loop for (name . code) in
        '(("set" . #x01) ("add" . #x02) ("sub" . #x03) ("mul" . #x04) ("mli" . #x05)
          ("div" . #x06) ("dvi" . #x07) ("mod" . #x08) ("mdi" . #x09) ("and" . #x0a)
          ("bor" . #x0b) ("xor" . #x0c) ("shr" . #x0d) ("asr" . #x0e) ("shl" . #x0f)
          ("ifb" . #x10) ("ifc" . #x11) ("ife" . #x12) ("ifn" . #x13) ("ifg" . #x14)
          ("ifa" . #x15) ("ifl" . #x16) ("ifu" . #x17) ("adx" . #x1a) ("sbx" . #x1b)
          ("sti" . #x1e) ("std" . #x1f))
        do (fiveam:is (equal (list (logior #x0400 code)) (cells (format nil "~A a, b" name)))
                      "~A" name)))

(fiveam:test special-opcodes
  (loop for (source . expected) in
        '(("jsr 5" #x9820)
          ("int 0" #x8500)
          ("iag a" #x0120)
          ("ias a" #x0140)
          ("rfi 0" #x8560)
          ("iaq 1" #x8980)
          ("hwn a" #x0200)
          ("hwq a" #x0220)
          ("hwi 1" #x8a40))
        do (fiveam:is (equal expected (cells source)) "~A" source)))

(fiveam:test disassembly-round-trips
  (let* ((source "set a, 5
set b, [c + 2]
add [0x2000], pop
ife x, 0x1234
set push, pick 3
jsr 9
hwi 1")
         (assembly (assemble source :machine 'dcpu16))
         (lines (mapcar #'disassembly-line-text
                        (disassemble-assembly assembly :machine 'dcpu16 :labels nil))))
    (fiveam:is (= 7 (length lines)))
    (fiveam:is (equalp (assembly-cells assembly)
                       (assembly-cells (assemble (format nil "~{~A~%~}" lines) :machine 'dcpu16))))))

;;; Arithmetic and EX.

(fiveam:test add-sets-ex-on-overflow
  (let ((m (run-source "set a, 0xffff
add a, 2")))
    (fiveam:is (equal '(1) (regs m :a)))
    (fiveam:is (= 1 (ex m)))))

(fiveam:test sub-sets-ex-on-underflow
  (let ((m (run-source "set a, 1
sub a, 2")))
    (fiveam:is (equal '(#xffff) (regs m :a)))
    (fiveam:is (= #xffff (ex m)))))

(fiveam:test multiplication
  (let ((m (run-source "set a, 0x8000
mul a, 4
set b, -2
mli b, 3")))
    (fiveam:is (equal '(0 #xfffa) (regs m :a :b)))
    (fiveam:is (= #xffff (ex m))))
  (fiveam:is (= 2 (ex (run-source "set a, 0x8000
mul a, 4")))))

(fiveam:test division
  (let ((m (run-source "set a, 7
div a, 2")))
    (fiveam:is (equal '(3) (regs m :a)))
    (fiveam:is (= #x8000 (ex m))))
  (let ((m (run-source "set a, -7
dvi a, 2")))
    (fiveam:is (equal '(#xfffd) (regs m :a))))
  (let ((m (run-source "set ex, 9
set a, 7
div a, 0")))
    (fiveam:is (equal '(0) (regs m :a)))
    (fiveam:is (= 0 (ex m)))))

(fiveam:test modulo
  (fiveam:is (equal '(1 0 #xfff9)
                    (regs (run-source "set a, 7
mod a, 3
set b, 7
mod b, 0
set c, -7
mdi c, 16")
                          :a :b :c))))

(fiveam:test bitwise
  (fiveam:is (equal '(#x0f0 #xfff #xf0f)
                    (regs (run-source "set a, 0xff0
and a, 0x0f0
set b, 0xf00
bor b, 0x0ff
set c, 0xfff
xor c, 0x0f0")
                          :a :b :c))))

(fiveam:test shifts
  (let ((m (run-source "set a, 0x8001
shr a, 1")))
    (fiveam:is (equal '(#x4000) (regs m :a)))
    (fiveam:is (= #x8000 (ex m))))
  (let ((m (run-source "set a, 0x8000
asr a, 1")))
    (fiveam:is (equal '(#xc000) (regs m :a)))
    (fiveam:is (= 0 (ex m))))
  (let ((m (run-source "set a, 0x8001
shl a, 1")))
    (fiveam:is (equal '(2) (regs m :a)))
    (fiveam:is (= 1 (ex m)))))

(fiveam:test add-and-subtract-with-extra
  (let ((m (run-source "set ex, 1
set a, 5
adx a, 2")))
    (fiveam:is (equal '(8) (regs m :a)))
    (fiveam:is (= 0 (ex m))))
  (let ((m (run-source "set ex, 1
set a, 0
sbx a, 2")))
    (fiveam:is (equal '(#xffff) (regs m :a)))
    (fiveam:is (= #xffff (ex m)))))

(fiveam:test sti-and-std-step-i-and-j
  (fiveam:is (equal '(7 1 1) (regs (run-source "sti a, 7") :a :i :j)))
  (fiveam:is (equal '(7 #xffff #xffff) (regs (run-source "std a, 7") :a :i :j))))

;;; Operands.

(fiveam:test memory-operands
  (let ((m (run-source "set x, 0x2000
set [x], 5
set [x + 1], 6
set a, [0x2000]
set b, [x + 1]
set [0x2002], [x]")))
    (fiveam:is (equal '(5 6) (regs m :a :b)))
    (fiveam:is (equal '(5 6 5) (list (ram m #x2000) (ram m #x2001) (ram m #x2002))))))

(fiveam:test stack-operands
  (let ((m (run-source "set push, 1
set push, 2
set a, peek
set b, pick 1
set c, pop
set x, pop")))
    (fiveam:is (equal '(2 1 2 1) (regs m :a :b :c :x)))
    (fiveam:is (= 0 (sp m)))))

(fiveam:test push-lands-below-the-stack-top
  (let ((m (run-source "set push, 7")))
    (fiveam:is (= #xffff (sp m)))
    (fiveam:is (= 7 (ram m #xffff)))))

(fiveam:test a-is-read-before-b-is-written
  (let ((m (run-source "set push, 5
set push, pop")))
    (fiveam:is (= #xffff (sp m)))
    (fiveam:is (= 5 (ram m #xffff)))))

(fiveam:test writing-a-literal-is-ignored
  (fiveam:is (equal '(3) (regs (run-source "set a, 3
set 5, a") :a))))

(fiveam:test sp-pc-and-ex-are-operands
  (let ((m (run-source "set sp, 0x8000
set a, sp
set b, pc
set ex, 4
set c, ex")))
    (fiveam:is (equal '(#x8000 4 4) (regs m :a :b :c)))
    (fiveam:is (= #x8000 (sp m)))))

;;; Conditionals.

(fiveam:test conditions
  (loop for (test a b taken) in
        '(("ifb" #b1010 #b0100 nil) ("ifb" #b1010 #b0010 t)
          ("ifc" #b1010 #b0100 t) ("ifc" #b1010 #b0010 nil)
          ("ife" 5 5 t) ("ife" 5 6 nil)
          ("ifn" 5 6 t) ("ifn" 5 5 nil)
          ("ifg" 6 5 t) ("ifg" 5 5 nil) ("ifg" #xffff 1 t)
          ("ifa" 1 #xffff t) ("ifa" #xffff 1 nil)
          ("ifl" 5 6 t) ("ifl" 6 5 nil) ("ifl" 1 #xffff t)
          ("ifu" #xffff 1 t) ("ifu" 1 #xffff nil))
        do (fiveam:is (eq taken
                          (= 1 (first (regs (run-source
                                             (format nil "set a, ~D
set b, ~D
~A a, b
set c, 1
set x, 1" a b test))
                                            :c))))
                      "~A ~D, ~D" test a b)))

(fiveam:test failed-condition-skips-one-instruction
  (fiveam:is (equal '(0 1) (regs (run-source "ife 1, 2
set a, 9
set b, 1") :a :b))))

(fiveam:test failed-condition-chains-through-conditions
  (fiveam:is (equal '(0 8 1) (regs (run-source "ife 1, 2
ife 1, 1
set a, 9
set b, 8
set c, 1") :a :b :c)))
  (fiveam:is (equal '(9 1) (regs (run-source "ife 1, 1
set a, 9
set b, 1") :a :b))))

(fiveam:test skipping-steps-over-next-words
  (let ((m (run-source "ife 1, 2
set [0x2000], 0x1234
set b, 1")))
    (fiveam:is (= 0 (ram m #x2000)))
    (fiveam:is (equal '(1) (regs m :b)))))

(fiveam:test skipped-pop-leaves-sp-alone
  (let ((m (run-source "set push, 3
ife 1, 2
set a, pop")))
    (fiveam:is (= #xffff (sp m)))
    (fiveam:is (equal '(0) (regs m :a)))))

;;; Subroutines and cycle counts.

(fiveam:test jsr-pushes-the-return-address
  (let ((m (run-source "jsr routine
set b, 1
set pc, done
routine: set a, 5
set pc, pop
done:")))
    (fiveam:is (equal '(5 1) (regs m :a :b)))
    (fiveam:is (= 0 (sp m)))))

(fiveam:test instruction-cycles
  (loop for (source cycles) in
        '(("set a, 1" 1)
          ("set a, 0x1000" 2)
          ("set [0x2000], 0x1000" 3)
          ("set a, [0x2000]" 2)
          ("add a, b" 2)
          ("mul a, b" 2)
          ("div a, b" 3)
          ("adx a, b" 3)
          ("sti a, b" 2)
          ("ife a, b" 2)
          ("ifn a, b
set a, 0x1000" 3)
          ("ifn a, b
ife a, b
set a, 0x1000" 4)
          ("jsr 5" 3)
          ("iag a" 1)
          ("hwn a" 2)
          ("hwq 0" 4)
          ("hwi 5" 4))
        do (fiveam:is (= cycles (cycles-of source)) "~A" source)))

(fiveam:test undefined-opcode-does-not-decode
  (let ((m (make-machine 'dcpu16)))
    (setf (mref m 'ram 0) #x03e0)
    (fiveam:is (eq :decode-failure (step-machine m)))))

;;; Interrupts.

(fiveam:test software-interrupt-runs-the-handler-and-returns
  (let ((m (run-source "ias handler
set c, 9
int 0x42
set b, 1
set pc, halt
handler: set x, a
set y, 1
rfi 0")))
    (fiveam:is (equal '(0 1 9 #x42 1) (regs m :a :b :c :x :y)))
    (fiveam:is (= 0 (sp m)))))

(fiveam:test interrupts-are-ignored-without-a-handler
  (fiveam:is (equal '(1) (regs (run-source "int 5
set a, 1") :a))))

(fiveam:test queued-interrupt-waits-for-iaq-off
  (let ((m (run-source "ias handler
iaq 1
int 5
set b, 1
iaq 0
set c, 9
set pc, halt
handler: set x, a
rfi 0")))
    (fiveam:is (equal '(0 1 9 5) (regs m :a :b :c :x)))))

(fiveam:test interrupt-handler-runs-with-queueing-on
  (let ((m (run-source "ias handler
int 1
set pc, halt
handler: set b, 5
iag a
rfi 0")))
    (fiveam:is (equal '(0 5) (regs m :a :b)))))

;;; Hardware.

(fiveam:test hardware-enumeration
  (let ((m (run-source "hwn a
hwq 0
set push, a
set push, b
set push, c
set push, x
set push, y
hwq 1
hwq 9")))
    (fiveam:is (equal '(0 0 0 0 0) (regs m :a :b :c :x :y)))
    (fiveam:is (equal '(#xb402 #x12d0 1 #x8b36 #x1c6c) (mapcar (lambda (n) (ram m (- #x10000 n))) '(1 2 3 4 5)))))
  (let ((m (run-source "hwn z
hwq 1")))
    (fiveam:is (equal '(2) (regs m :z)))
    (fiveam:is (equal '(#x7406 #x30cf 1 #x8b36 #x1c6c) (regs m :a :b :c :x :y)))))

(fiveam:test clock-counts-ticks
  (let ((m (load-dcpu16 "set a, 0
set b, 1
hwi 0
busy: set pc, busy")))
    (run-for-cycles m 20000 :max-steps 100000)
    (setf (regref m 'reg 0) 1)
    (device-send m 0)
    (fiveam:is (<= 10 (reg-value m :c) 12))))

(fiveam:test clock-interrupts-the-program
  (let ((m (load-dcpu16 "ias handler
set a, 2
set b, 0x77
hwi 0
set a, 0
set b, 1
hwi 0
busy: set pc, busy
handler: add x, 1
rfi 0")))
    (run-for-cycles m 20000 :max-steps 100000)
    (fiveam:is (<= 10 (reg-value m :x) 12))))

(fiveam:test keyboard-reports-typed-and-pressed-keys
  (let ((m (load-dcpu16 "set a, 1
hwi 1
set x, c
set a, 1
hwi 1
set y, c
set a, 2
set b, 0x61
hwi 1
set z, c
halt: set pc, halt")))
    (key-down m #x61)
    (run-dcpu16 m)
    (fiveam:is (equal '(#x61 0 1) (regs m :x :y :z)))
    (key-up m #x61)
    (setf (regref m 'reg 0) 2 (regref m 'reg 1) #x61)
    (device-send m 1)
    (fiveam:is (= 0 (reg-value m :c)))))

(fiveam:test keyboard-interrupts-on-key-events
  (let ((m (load-dcpu16 "ias handler
set a, 3
set b, 0x99
hwi 1
busy: set pc, busy
handler: set x, a
rfi 0")))
    (run-for-cycles m 100)
    (key-down m #x20)
    (run-for-cycles m 100)
    (fiveam:is (= #x99 (reg-value m :x)))))

;;; A program that uses everything at once.

(fiveam:test sum-of-the-first-ten-numbers
  (let ((m (run-source "set a, 0
set i, 10
loop: add a, i
sub i, 1
ifn i, 0
set pc, loop
set [0x2000], a")))
    (fiveam:is (equal '(55 0) (regs m :a :i)))
    (fiveam:is (= 55 (ram m #x2000)))))
