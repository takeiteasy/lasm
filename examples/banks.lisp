;;;; examples/banks.lisp
;;;;
;;;; Banked output: .bank places data in banks of a banked ROM window, the
;;;; assembly keeps one image per bank, LOAD-PROGRAM fills every bank, and
;;;; the output bytes are the physical ROM layout.
;;;;
;;;; Run with:  sbcl --script examples/banks.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(defmachine banksfoo
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region romx #x4000 #x40FF :kind :rom :banks 4)))

(definstruction banksfoo bnk
  (modes immediate)
  (encoding (opcode #x01) (operand :mode))
  (semantics (set-bank! romx operand)))

(definstruction banksfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "        bnk #bank(two)
        hlt
        .bank 1
        .org $4000
one:    .byte 11, 12
        .bank 2
        .org $4000
two:    .byte 21, 22
        .byte bank(*)")

(format t "~&Source:~%~A~2%" *source*)

(let* ((assembly (assemble *source* :machine 'banksfoo))
       (m (make-machine 'banksfoo)))
  (format t "~A~%" (listing-text assembly))
  (assert (equal '(1 2) (mapcar #'bank-image-bank (assembly-banks assembly))))
  (assert (zerop (length (assembly-bytes assembly :bank 0))))
  (assert (= 256 (length (assembly-bytes assembly :bank 2))))
  (assert (= (+ 3 (* 3 256)) (length (assembly-bytes assembly))))

  (load-program m assembly)
  (assert (= 0 (current-bank m 'romx)))
  (assert (= 11 (bank-peek m 'romx 1 #x4000)))

  ;; A label breakpoint in banked code carries its bank
  (let ((bp (debug-break (make-debug-session m :assembly assembly) "two")))
    (assert (= 2 (breakpoint-bank bp))))

  (run m)
  (assert (= 2 (current-bank m 'romx)))
  (assert (= 21 (mref m 'ram #x4000)))
  (assert (= 2 (mref m 'ram #x4002)))
  (format t "After running, bank ~D is mapped: ram[$4000] = ~D~%"
          (current-bank m 'romx) (mref m 'ram #x4000)))
