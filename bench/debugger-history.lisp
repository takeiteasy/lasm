;;;; sbcl --script bench/debugger-history.lisp [STEPS]

(load (merge-pathnames "../examples/boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine bench-machine
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction bench-machine ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction bench-machine bne
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc operand))))

(definstruction bench-machine sta
  (modes absolute)
  (encoding (opcode #xA9) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(defparameter *source*
  "        ldx #1
loop:   sta $10
        bne loop")

(defun bench-session (steps history)
  (let ((machine (make-machine 'bench-machine)))
    (load-program machine (assemble *source* :machine 'bench-machine :origin #x100))
    (make-debug-session machine :assembly (machine-program machine)
                                :history (and history (+ steps 100)))))

(defun seconds (thunk)
  (let ((start (get-internal-real-time)))
    (funcall thunk)
    (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))

(defun report (label seconds steps)
  (format t "~&~30A ~8,3F s  ~8,2F Msteps/s~%" label seconds (/ steps seconds 1000000.0)))

(defun bench-forward (label steps &key history condition)
  (let ((session (bench-session steps history)))
    (when condition (debug-break session "loop" :condition condition))
    (report label (seconds (lambda () (debug-continue session :max-steps steps))) steps)))

(defun bench-reverse (label steps target)
  (let ((session (bench-session steps t)))
    (debug-continue session :max-steps steps)
    (debug-watch session target :access :write)
    (report label (seconds (lambda () (debug-reverse-continue session))) steps)))

(let* ((steps (parse-integer (or (first (uiop:command-line-arguments)) "200000")))
       (*debug-checkpoint-interval* 256))
  (assert (plusp steps))
  (format t "~&~D steps, checkpoint every ~D~%" steps *debug-checkpoint-interval*)
  (let ((machine (make-machine 'bench-machine)))
    (load-program machine (assemble *source* :machine 'bench-machine :origin #x100))
    (report "run" (seconds (lambda () (run machine :max-steps steps))) steps))
  (bench-forward "continue, no history" steps)
  (bench-forward "continue, history" steps :history t)
  (bench-forward "continue, history, condition" steps :history t :condition "x == 5")
  (bench-reverse "reverse, watch unwritten page" steps #x300)
  (bench-reverse "reverse, watch register write" steps "x"))
