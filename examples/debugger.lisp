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
  (semantics (set! x operand) (set-flags! (z (zero? x))))
  (cycles 2))

(definstruction sixtyfoo dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x))))
  (cycles 2))

(definstruction sixtyfoo bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand))))
  (cycles 3))

(definstruction sixtyfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x))
  (cycles 4))

(definstruction sixtyfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(let* ((assembly (assemble *source* :lexer 'sixtyfoo-syntax :machine 'sixtyfoo))
       (machine (make-machine 'sixtyfoo)))
  (load-program machine assembly)

  ;; A DEBUG-SESSION carries the machine plus (optionally) the assembly that
  ;; produced its program -- the latter is what lets a label like ".loop"
  ;; resolve to an address for `break`.
  ;; :HISTORY keeps checkpoints so `back` can undo steps.
  (let ((session (make-debug-session machine :assembly assembly :history 1000)))

    (format t "~&== Every command below goes through DEBUG-COMMAND, string in, ==~%")
    (format t "== text out -- exactly what a DEBUGGER-REPL loop dispatches. ==~2%")

    ;; ".loop" is a local label; it is named by its qualified spelling
    ;; (`count.loop`) or as `.loop in count`.
    ;; `if` makes the breakpoint conditional; `watch` stops after an
    ;; instruction reads or writes a register, flag or memory address.
    ;; `step N cycles` runs until N cycles are spent; `back N` undoes N steps.
    (dolist (command '("break count.loop if x == 1" "info break" "continue"
                        "where" "step 4 cycles" "where" "back 2" "where"
                        "delete 1" "watch x" "continue" "info break"
                        "delete 2" "continue" "info reg" "x/4 $1000" "print x"))
      (format t "(lasm-dbg) ~A~%~A" command (debug-command session command)))))
