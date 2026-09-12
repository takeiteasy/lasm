;;;; examples/counter.lisp
;;;;
;;;; Exercises M1's front end and instruction pipeline (lexer.lisp,
;;;; parser.lisp, instruction.lisp): defines a syntax with DEFLEXER and a
;;;; small register machine with DEFINSTRUCTION, then tokenizes and parses a
;;;; hand-written counter-loop program -- the same kind of source the M1
;;;; milestone target (a counter loop assembled and run end to end,
;;;; LASM-plan.md sec. 2) will eventually feed through the full pipeline --
;;;; and, per statement, matches its operand against the instruction's
;;;; addressing mode, encodes it to bytes, and executes it against a live
;;;; machine.
;;;;
;;;; This still stops short of a full assembler pass: labels are not
;;;; resolved (the "bne .loop" branch target is printed as UNRESOLVED-LABEL
;;;; rather than encoded), and there is no statement-list -> byte-vector
;;;; driver or fetch/execute loop over encoded bytes -- that's the assembler
;;;; pass and the emulator loop, respectively. See docs/lexer.md,
;;;; docs/parser.md, and docs/instructions.md.
;;;;
;;;; Run with:  sbcl --script examples/counter.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

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
  "        ldx #10        ; x = 10
.loop:  dex             ; x -= 1
        bne .loop       ; loop while x != 0
        sta $1000")

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

;;; Instruction set: just enough of SIXTYFOO to encode and execute the
;;; program above, given its operand values (the "bne .loop" branch target
;;; is a label -- resolving it belongs to the assembler pass, so it prints
;;; as unresolved below rather than being encoded).

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
  (modes absolute)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc operand))))

(definstruction sixtyfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) x)))

(format t "~%Encoded instructions:~%")
(dolist (s (parse *source* :lexer 'sixtyfoo-syntax))
  (when (statement-mnemonic s)
    (let* ((descriptor (find-instruction 'sixtyfoo (statement-mnemonic s)))
           (mode (instruction-descriptor-mode descriptor)))
      (if (null mode)
          (format t "  line ~D: ~A -> bytes ~S~%"
                  (statement-line s) (statement-mnemonic s)
                  (encode-instruction descriptor nil))
          (let ((ast (match-operand-mode (first (statement-operands s)) mode)))
            (handler-case
                (format t "  line ~D: ~A -> bytes ~S~%"
                        (statement-line s) (statement-mnemonic s)
                        (encode-instruction descriptor (eval-expr-constant ast)))
              (unresolved-label (c)
                (format t "  line ~D: ~A -> operand references unresolved label ~S (label resolution belongs to the assembler pass)~%"
                        (statement-line s) (statement-mnemonic s) (unresolved-label-name c)))))))))

(format t "~%Executing ldx #10 / dex / dex / sta $1000 directly against a machine:~%")
(let ((m (make-machine 'sixtyfoo)))
  (execute-instruction (find-instruction 'sixtyfoo 'ldx) m 10)
  (execute-instruction (find-instruction 'sixtyfoo 'dex) m nil)
  (execute-instruction (find-instruction 'sixtyfoo 'dex) m nil)
  (execute-instruction (find-instruction 'sixtyfoo 'sta) m #x1000)
  (format t "  X = ~D, Z flag = ~D, RAM[$1000] = ~D~%"
          (sref m 'x) (flag m 'z) (mref m 'ram #x1000)))
