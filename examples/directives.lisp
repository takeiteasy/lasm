;;;; examples/directives.lisp
;;;;
;;;; Directives (directive.lisp, #14): a leading .ORG places the whole
;;;; program at a fixed address, .BYTE lays down a small data table (whose
;;;; values are read back through LDA/ADC's ABSOLUTE mode, same as
;;;; examples/modes.lisp), .RES reserves a zero-filled scratch buffer between
;;;; the code and the table -- sized by a .EQU constant (#35) rather than a
;;;; literal -- and .EQU itself also names a repeated immediate value.
;;;;
;;;; Run with:  sbcl --script examples/directives.lisp

(require :asdf)
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))

(in-package #:lasm)

(defmachine sixtyfoo-directives
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (flags z n c))

(definstruction sixtyfoo-directives lda
  (modes (immediate (opcode #xA9) (semantics (set! a operand)))
         (absolute  (opcode #xAD) (semantics (set! a (mref machine 'ram operand))))))

(definstruction sixtyfoo-directives adc
  (modes absolute)
  (encoding (opcode #x6D) (operand :mode))
  (semantics (let ((r (+ a (mref machine 'ram operand) c)))
               (set! a (wrap-value r 8))
               (set-flags! (c (> r 255)) (z (zero? a)) (n (bit-set? a 7))))))

(definstruction sixtyfoo-directives sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction sixtyfoo-directives hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

;; .RES lives after HLT, alongside the data -- PC never flows into it (this
;; machine has no jump/skip instruction to leap over a reserved run sitting
;; in the middle of the code path, so a reserved buffer belongs in the data
;; area, not spliced between two executed instructions).
(defparameter *source*
  "        .equ initial, 10   ; a named constant, no address of its own
        .equ padsize, 2    ; -- pure (no label, no *), so .res may use it
        .org $8000         ; place the whole program at $8000
start:  lda #initial   ; A = 10 (an .equ works as an immediate operand)
        sta scratch    ; RAM[scratch] = 10
        lda scratch    ; A = RAM[scratch] = 10
        adc table      ; A += RAM[table] (5) -- ABSOLUTE, same as sta above
        hlt
scratch: .byte 0
padding: .res padsize  ; two zero-filled scratch bytes, unread by this program
table:   .byte 5, 10, 15")

(format t "~&Source:~%~A~2%" *source*)

(format t "Assembling:~%")
(let ((assembly (assemble *source* :machine 'sixtyfoo-directives)))
  (format t "  origin: $~4,'0X~%" (assembly-origin assembly))
  (format t "  bytes: ~{~2,'0X~^ ~}~%" (coerce (assembly-cells assembly) 'list))

  (format t "~%Running:~%")
  (let ((m (make-machine 'sixtyfoo-directives)))
    (load-program m assembly)
    (multiple-value-bind (reason steps) (run m)
      (format t "  stopped: ~A after ~D step~:P~%" reason steps)
      (format t "  A = ~D~%" (sref m 'a)))))
