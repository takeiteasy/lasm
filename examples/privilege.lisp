;;;; examples/privilege.lisp
;;;;
;;;; #111: privilege levels -- a (privilege ...) clause names the flag that
;;;; holds the current level, a region's :PRIVILEGE gates CPU access to it,
;;;; and a (privilege LEVEL) clause on DEFINSTRUCTION gates an instruction.
;;;; A user-level program's stores into the kernel region and its SUPER
;;;; instruction both fault; the same program succeeds at supervisor level.
;;;;
;;;; #300-#302: a register, flag or stack may be gated too, interrupt delivery
;;;; can switch level (:deliver-level), and a violation can raise an interrupt
;;;; (:on-violation (:interrupt DATA)) instead of faulting.
;;;;
;;;; #314: a register can gate writes to bits of it (:fields).
;;;;
;;;; #299, #303: the level can be a bit field of a status register, and a
;;;; region, register, flag or stack can gate reads, writes and fetches apart.
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

;;; A gated control register whose violation vectors to a supervisor handler.

(defmachine privirq
  (register pc :width 8) (register ia :width 8) (register a :width 8)
  (register sp :width 8)
  (register cr :width 8 :privilege supervisor)
  (memory ram :width 8 :addr-width 8
    (region kernel #x00 #x3f :privilege supervisor))
  (stack-pointer sp :memory ram)
  (flags s)
  (privilege :level s :levels (user supervisor) :on-violation (:interrupt 1))
  (interrupts :vector ia :message a :save (pc s) :stack sp :deliver-level supervisor))

(definstruction privirq rdcr
  (encoding (opcode #x10))
  (semantics (set! a cr)))

(definstruction privirq hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(let ((m (make-machine 'privirq)))
  (load-program m (list #x00) :origin #x10)            ; handler: hlt
  (load-program m (list #x10) :origin #x80)            ; user code: rdcr
  (setf (sref m 'pc) #x80 (sref m 'ia) #x10 (sref m 'sp) #x40)
  (multiple-value-bind (reason steps) (run m)
    (format t "~&privirq: ~(~A~) after ~D step~:P, level ~A, a = ~D~%  ~S~%"
            reason steps (privilege-level m) (sref m 'a) (privilege-violation-info m))))

;;; The level is bit 13 of a status register; the kernel region is
;;; execute-only for user code (readable and writable only from supervisor).

(defmachine privsr
  (register pc :width 8) (register a :width 8)
  (register sr :width 16)
  (memory ram :width 8 :addr-width 8
    (region kernel #x00 #x0f :privilege (:read supervisor :write supervisor)))
  (privilege :level sr :shift 13 :width 1 :levels (user supervisor)))

(definstruction privsr nop
  (encoding (opcode #x00))
  (semantics nil))

(dolist (sr '(#x00ff #x20ff))
  (let ((m (make-machine 'privsr)))
    (setf (sref m 'sr) sr)
    (load-program m (list #x00))
    (step-machine m)
    (format t "~&privsr: sr = #x~4,'0X, level ~A, kernel fetch ok, read ~A~%"
            sr (privilege-level m)
            (handler-case (progn (mref m 'ram 1) "ok")
              (privilege-violation (c) (format nil "violates (~(~A~))" (privilege-violation-access c)))))))

;;; #314: user code may set SR's condition-code bits but not its S bit.

(defmachine privfields
  (register pc :width 8) (register a :width 8)
  (register sr :width 16
    :privilege (:fields ((#x2000 supervisor) (#x0700 supervisor :on-write :ignore))))
  (memory ram :width 8 :addr-width 8)
  (privilege :level sr :shift 13 :width 1 :levels (user supervisor)))

(definstruction privfields set-cc (encoding (opcode #x01)) (semantics (set! sr (logior sr #x0001))))
(definstruction privfields set-ipl (encoding (opcode #x02)) (semantics (set! sr (logior sr #x0700))))
(definstruction privfields set-s (encoding (opcode #x03)) (semantics (set! sr (logior sr #x2000))))

(dolist (opcode '(1 2 3))
  (let ((m (make-machine 'privfields)))
    (load-program m (list opcode))
    (format t "~&privfields: opcode ~D -> ~A~%" opcode
            (handler-case (progn (step-machine m) (format nil "sr = #x~4,'0X" (sref m 'sr)))
              (privilege-violation (c) (format nil "~A" c))))))
