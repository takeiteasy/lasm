;;;; examples/privilege.lisp
;;;;
;;;; #111: privilege levels -- a (privilege ...) clause names the flag that
;;;; holds the current level, a region's :PRIVILEGE gates CPU access to it,
;;;; and a (privilege LEVEL) clause on DEFINSTRUCTION gates an instruction.
;;;; A user-level program's stores into the kernel region and its SUPER
;;;; instruction both fault; the same program succeeds at supervisor level.
;;;;
;;;; Run with:  sbcl --script examples/privilege.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine privfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16
    (region kernel #x0000 #x00ff :privilege supervisor)
    (region user   #x0100 #xffff))
  (flags s)
  (privilege :level s :levels (user supervisor)))

(definstruction privfoo lda
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! a operand)))

(definstruction privfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction privfoo super
  (privilege supervisor)
  (encoding (opcode #x40))
  (semantics (set! a #xff)))

(definstruction privfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defun run-at-level (level source)
  (let ((m (make-machine 'privfoo)))
    (setf (sref m 's) level)
    (load-program m (assemble source :machine 'privfoo :origin #x100))
    (multiple-value-bind (reason steps condition) (run m)
      (format t "~&level ~A: ~(~A~) after ~D step~:P~@[ -- ~A~]~%"
              (privilege-level m) reason steps condition)
      m)))

(defparameter *store*
  "lda #7
sta $0010   ; kernel region
hlt")

(defparameter *privileged* "super
hlt")

(run-at-level 0 *store*)
(run-at-level 1 *store*)
(run-at-level 0 *privileged*)
(run-at-level 1 *privileged*)
