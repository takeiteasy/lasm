;;;; examples/counter.lisp
;;;;
;;;; Exercises M1's front end (lexer.lisp, parser.lisp): defines a syntax
;;;; with DEFLEXER, then tokenizes and parses a hand-written counter-loop
;;;; program -- the same kind of source the M1 milestone target (a counter
;;;; loop assembled and run end to end, LASM-plan.md sec. 2) will eventually
;;;; feed through the full pipeline. No instruction set, addressing modes,
;;;; or assembler are involved yet -- this stops at the statement/expression
;;;; AST; see docs/lexer.md and docs/parser.md.
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
