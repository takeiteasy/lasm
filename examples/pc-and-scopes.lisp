;;;; examples/pc-and-scopes.lisp
;;;;
;;;; Two M2 follow-up features (LASM-plan.md sec. 2, tickets #15/#16),
;;;; assembled and run end to end on the same SIXTYFOO-shaped machine as
;;;; examples/counter.lisp:
;;;;
;;;; - The location-counter symbol "*" (docs/parser.md, docs/assembler.md) --
;;;;   an operand can refer to "the address of this statement" without
;;;;   naming a label for it.
;;;; - Local-label scoping (docs/assembler.md#local-label-scoping-16) -- a
;;;;   ".loop" label is scoped to its nearest preceding global label, so two
;;;;   routines can each define their own ".loop" without colliding.
;;;; - .equ / symbol assignment (docs/directives.md#equ, docs/assembler.md#equ--
;;;;   symbol-assignment, #35) -- a named constant computed from "*" and a
;;;;   backward label, occupying no address of its own.
;;;; - Scope-aware symbol listing (docs/listing.md#symbol-table, #37) --
;;;;   PRINT-SYMBOLS groups COUNT_DOWN.LOOP/COUNT_UP.LOOP under their own
;;;;   enclosing global and tags ROUTINE1_SIZE as an .equ, not a label.
;;;;
;;;; Run with:  sbcl --script examples/pc-and-scopes.lisp

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
  "count_down:                    ; routine 1 -- x counts down from 3
        ldx #3
.loop:  dex                     ; -> bound as \"count_down.loop\"
        bne .loop               ; resolves against \"count_down.loop\"

count_up:                       ; routine 2 -- reuses \".loop\" freely
        ldx #5
.loop:  dex                     ; -> bound as \"count_up.loop\", no collision
        bne .loop               ; resolves against \"count_up.loop\"

routine1_size = count_up - count_down  ; \"=\" sugar for .equ (#35) -- both
                                 ; operands are backward labels, already
                                 ; bound by the time this line folds

        hlt                     ; program ends here -- what follows is data,
                                 ; never executed

self:   .word *                 ; \"*\": this word's own address (a
                                 ; location-counter reference, not a label)
                                 ; -- equivalent to \".word self\"")

(format t "~&Source:~%~A~2%" *source*)

;;; Instruction set: SIXTYFOO, same shape as examples/counter.lisp.

(defmachine sixtyfoo-scopes
  (register x :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z))

(definstruction sixtyfoo-scopes ldx
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! x operand) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo-scopes dex
  (encoding (opcode #xCA))
  (semantics (set! x (wrap-value (1- x) 8)) (set-flags! (z (zero? x)))))

(definstruction sixtyfoo-scopes bne
  (modes relative)
  (encoding (opcode #xD0) (operand :mode))
  (semantics (when (zerop z) (set! pc (+ pc operand)))))

(definstruction sixtyfoo-scopes hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(format t "Assembling:~%")
(let ((assembly (assemble *source* :lexer 'sixtyfoo-syntax :machine 'sixtyfoo-scopes)))
  (format t "  bytes:   ~S~%" (coerce (assembly-cells assembly) 'list))
  (format t "  symbols: ~{~A=$~4,'0X~^, ~}~%"
          (loop for k being the hash-keys of (assembly-symbols assembly)
                  using (hash-value v)
                collect k collect v))

  (format t "~%Symbol table, grouped by scope (#37):~%")
  (print-symbols assembly)

  (format t "~%Running:~%")
  (let* ((m (make-machine 'sixtyfoo-scopes))
         (self-address (gethash "self" (assembly-symbols assembly))))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  X = ~D~%" (sref m 'x))
      (format t "  \"self\" = $~4,'0X, .word * there = $~4,'0X (self-referential -- \"*\" == \"self\")~%"
              self-address
              (logior (mref m 'ram self-address)
                      (ash (mref m 'ram (1+ self-address)) 8)))
      (format t "  \"routine1_size\" = ~D bytes (an .equ value, not an address)~%"
              (gethash "routine1_size" (assembly-symbols assembly))))))
