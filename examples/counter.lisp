;;;; examples/counter.lisp
;;;;
;;;; The M1 milestone target: a counter-loop program
;;;; assembled and run end to end through the whole pipeline -- DEFLEXER's
;;;; syntax, the statement/expression parser, DEFINSTRUCTION's addressing
;;;; modes and encoding, the assembler pass (assembler.lisp), and the
;;;; emulator loop (emulator.lisp) -- ending in a correct final register
;;;; state. See docs/lexer.md, docs/parser.md, docs/instructions.md,
;;;; docs/assembler.md, and docs/emulator.md.
;;;;
;;;; Run with:  sbcl --script examples/counter.lisp

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
  "count:  ldx #10        ; x = 10
.loop:  dex             ; x -= 1
        bne .loop       ; loop while x != 0
        sta $1000
        hlt             ; stop the emulator loop (see docs/emulator.md)")

;; NOTE: ".loop" is a local label by lexer convention (LOCAL-LABEL-PREFIX
;; "."), scoped to its nearest preceding global label (#16, docs/assembler.md)
;; -- "count" here -- so it binds as "count.loop" in the symbol table, not
;; bare ".loop". A local label needs some enclosing global label; see
;; examples/pc-and-scopes.lisp for a program with two routines that each
;; reuse ".loop" without colliding, and for the location-counter symbol "*".

(format t "~&Source:~%~A~2%" *source*)

(format t "Tokens:~%")
(loop for tok across (tokenize *source* :lexer 'sixtyfoo-syntax)
      unless (eq (token-type tok) :eof)
      do (format t "  ~S ~S~@[ = ~S~] (line ~D, col ~D)~%"
                 (token-type tok) (token-text tok)
                 (and (member (token-type tok) '(:number :identifier)) (token-value tok))
                 (token-line tok) (token-column tok)))

(format t "~%Statements:~%")
(dolist (s (parse *source* :lexer 'sixtyfoo-syntax))
  (format t "  line ~D: label=~S mnemonic=~S operands=~D~%"
          (statement-line s) (statement-label s) (statement-mnemonic s)
          (length (statement-operands s))))

;;; Instruction set: enough of SIXTYFOO to assemble and run the program
;;; above end to end.

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

;; No addressing mode, no operand -- HLT signals LASM-TRAP via the existing
;; TRAP semantics primitive rather than needing a dedicated halt mechanism;
;; RUN below catches it and reports :TRAP as the stop reason.
(definstruction sixtyfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(format t "~%Assembling:~%")
(let ((assembly (assemble *source* :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)))
  (format t "  bytes:   ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  symbols: ~{~A=$~4,'0X~^, ~}~%"
          (loop for k being the hash-keys of (assembly-symbols assembly)
                  using (hash-value v)
                collect k collect v))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  X = ~D, Z flag = ~D, RAM[$1000] = ~D~%"
              (sref m 'x) (flag m 'z) (mref m 'ram #x1000)))))
