;;;; examples/debugger.lisp
;;;;
;;;; #76 (M7): the debugger's machine-agnostic API and its reference
;;;; DEBUG-COMMAND dispatcher, driven against the same counter-loop program
;;;; as examples/counter.lisp. Sets a conditional breakpoint on the loop label,
;;;; adds a watchpoint, and inspects registers/flags/memory -- all through
;;;; DEBUG-COMMAND so this doubles as a script of what a DEBUGGER-REPL
;;;; session looks like. See docs/debugger.md.
;;;;
;;;; Run with:  sbcl --script examples/debugger.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(deflexer sixtyfoo-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))

(defparameter *source*
  "count:  ldx #3          ; x = 3
.loop:  dex             ; x -= 1
        bne .loop       ; loop while x != 0
        sta $1000
        hlt             ; stop the emulator loop (see docs/emulator.md)")

(defmachine sixtyfoo
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction sixtyfoo ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction sixtyfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(definstruction sixtyfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(let* ((assembly (assemble *source* :lexer 'sixtyfoo-syntax :machine 'sixtyfoo))
       (machine (make-machine 'sixtyfoo)))
  (load-program machine assembly)

  ;; A DEBUG-SESSION carries the machine plus (optionally) the assembly that
  ;; produced its program -- the latter is what lets a label like ".loop"
  ;; resolve to an address for `break`.
  (let ((session (make-debug-session machine :assembly assembly)))

    (format t "~&== Every command below goes through DEBUG-COMMAND, string in, ==~%")
    (format t "== text out -- exactly what a DEBUGGER-REPL loop dispatches. ==~2%")

    ;; ".loop" is a local label at address 2. TODO: `break count.loop` does
    ;; not resolve, so the address is used instead (#240).
    ;; `if` makes the breakpoint conditional; `watch` stops after an
    ;; instruction reads or writes a register, flag or memory address.
    (dolist (command '("break 2 if x == 1" "info break" "continue"
                        "where" "delete 1" "watch x" "continue" "info break"
                        "delete 2" "continue" "info reg" "x/4 $1000" "print x"))
      (format t "(lasm-db) ~A~%~A" command (debug-command session command)))))
