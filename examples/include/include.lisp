;;;; examples/include/include.lisp
;;;;
;;;; A program split across files: main.asm pulls a shared .equ and .macro in
;;;; from defs.asm with .include, and ASSEMBLE-FILE reads main.asm from disk.
;;;; See docs/includes.md and docs/assembler.md.
;;;;
;;;; Run with:  sbcl --script examples/include/include.lisp

(load (merge-pathnames "../boot.lisp" *load-pathname*))

(in-package #:lasm)

(deflexer sixtyfoo-syntax
  (comment-styles (";" :line))
  (number-formats (:hex "$" "0x") (:bin "%" "0b") (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (string-delim "\"")
  (ident-chars :alnum "_.")
  (line-continuation "\\"))

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

(let ((assembly (assemble-file (merge-pathnames "main.asm" *load-pathname*)
                               :lexer 'sixtyfoo-syntax :machine 'sixtyfoo)))
  (format t "bytes: ~S~%" (coerce (assembly-cells assembly) 'list))
  (let ((m (make-machine 'sixtyfoo)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "stopped: ~A after ~D step~:P, X = ~D~%" reason steps (sref m 'x)))))
