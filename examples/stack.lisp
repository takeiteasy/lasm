;;;; examples/stack.lisp
;;;;
;;;; The M3 milestone target (LASM-plan.md sec. 2): "A stack-based fantasy
;;;; CPU built entirely through the same defmachine/definstruction forms as
;;;; M1/M2, to validate the storage abstraction actually generalizes rather
;;;; than being register-shaped in disguise." STACKFOO below declares no
;;;; general-purpose registers at all -- a PC (see the note below), one data
;;;; stack, and one memory element are enough to assemble and run a real
;;;; counted loop, end to end, through the unmodified M1/M2 pipeline. See
;;;; docs/machine-model.md, docs/semantics.md, docs/emulator.md.
;;;;
;;;; Two things worth calling out that this example pins down:
;;;;
;;;; - PC is still a plain register (emulator.lisp's %RESOLVE-PC convention
;;;;   requires a register literally named PC, or an explicit :PC). "No
;;;;   general-purpose registers" does not mean "no registers at all" -- the
;;;;   ticket's own wording ("enough memory/PC to hold a program") already
;;;;   expects this.
;;;; - LASM-plan.md sec. 3.3's mockup ADD, `(push (+ (pop) (pop)))`, does not
;;;;   compile as written: PUSH/POP take an explicit stack name (`(push value
;;;;   stack-name)` / `(pop stack-name)`), since a machine can declare more
;;;;   than one stack. See "Deviation from the design draft" in
;;;;   docs/semantics.md. ADD below is written the way LASM actually requires:
;;;;   (push (wrap-value (+ (pop ds) (pop ds)) 8) ds).
;;;;
;;;; Bottom line: no register-shaped workarounds were needed anywhere below.
;;;;
;;;; Run with:  sbcl --script examples/stack.lisp

(require :asdf)
;; #75 gave LASM its first dependency (trivial-high-precision-timer, itself
;; depending on CFFI on SBCL) -- both are Quicklisp libraries, so a bare
;; `sbcl --script` run (no ~/.sbclrc) needs Quicklisp bootstrapped explicitly
;; before ASDF can resolve them, same as docs/getting-started.md's install
;; instructions assume.
(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-setup)
      (load quicklisp-setup)
      (error "Quicklisp not found at ~A -- see docs/getting-started.md" quicklisp-setup)))
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

;;; Machine: PC (convention, see above), one data stack, one RAM. No
;;; general-purpose registers, no flags -- nothing below needs them, and
;;; flags carry a live wrap-value/boolean gotcha (#22) not worth walking
;;; into here.

(defmachine stackfoo
  (register pc :width 16)
  (stack ds :width 8 :depth 32)
  (memory ram :width 8 :addr-width 16))

;; PSH/LDM push a value onto DS -- an immediate constant, or a copy of a
;; memory cell -- so arithmetic always operates on the stack even though
;; the running counter/sum live in RAM (there being no registers to hold
;; them in).
(definstruction stackfoo psh
  (modes immediate)
  (encoding (opcode #x01) (operand :mode))
  (semantics (push operand ds)))

(definstruction stackfoo ldm
  (modes absolute)
  (encoding (opcode #x02) (operand :mode))
  (semantics (push (mref machine 'ram operand) ds)))

(definstruction stackfoo sto
  (modes absolute)
  (encoding (opcode #x03) (operand :mode))
  (semantics (setf (mref machine 'ram operand) (pop ds))))

;; ADD/SUB pop both operands and push the (wrapped) result -- the actual
;; shape LASM-plan.md sec. 3.3 was gesturing at, modulo the explicit stack
;; name PUSH/POP require. Second-popped is the left-hand operand, so
;; (ldm a)(psh b)(sub) computes a - b.
(definstruction stackfoo add
  (encoding (opcode #x04))
  (semantics (let ((b (pop ds)) (a (pop ds)))
               (push (wrap-value (+ a b) 8) ds))))

(definstruction stackfoo sub
  (encoding (opcode #x05))
  (semantics (let ((b (pop ds)) (a (pop ds)))
               (push (wrap-value (- a b) 8) ds))))

;; JZ pops its condition off DS and branches (PC-relative) when it was
;; zero -- there being no flags to test instead. JMP is the unconditional
;; counterpart, needed to close the loop back to its top.
(definstruction stackfoo jz
  (modes relative)
  (encoding (opcode #x06) (operand :mode))
  (semantics (let ((v (pop ds)))
               (when (zerop v) (set! pc (+ pc operand))))))

(definstruction stackfoo jmp
  (modes relative)
  (encoding (opcode #x07) (operand :mode))
  (semantics (set! pc (+ pc operand))))

;; No dedicated halt mechanism, same as every other example -- HLT's
;; semantics signal LASM-TRAP, which RUN below catches as a stop reason.
(definstruction stackfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; A counted loop summing 1..5 into ram[$1000] -- 5+4+3+2+1 = 15. The
;; counter lives at ram[$0000], the running sum at ram[$1000]; the data
;; stack is only ever used as scratch space for one instruction's worth of
;; arithmetic, and is empty again at every loop iteration's start/end.
(defparameter *source*
  "        psh #5          ; counter = 5
        sto $0000
        psh #0          ; sum = 0
        sto $1000
loop:   ldm $0000       ; ds: [counter]
        jz end          ; counter == 0 -> done (pops counter)
        ldm $0000       ; ds: [counter]
        ldm $1000       ; ds: [counter, sum]
        add             ; ds: [counter+sum]
        sto $1000       ; sum = counter+sum
        ldm $0000       ; ds: [counter]
        psh #1          ; ds: [counter, 1]
        sub             ; ds: [counter-1]
        sto $0000       ; counter = counter-1
        jmp loop
end:    hlt")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'stackfoo)))
  (format t "  bytes:   ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  symbols: ~{~A=$~4,'0X~^, ~}~%"
          (loop for k being the hash-keys of (assembly-symbols assembly)
                  using (hash-value v)
                collect k collect v))

  (format t "~%Running:~%")
  (let ((m (make-machine 'stackfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  RAM[$1000] (sum) = ~D~%" (mref m 'ram #x1000))
      (format t "  data stack depth = ~D (0 => program is stack-balanced)~%"
              (stack-depth m 'ds)))))
