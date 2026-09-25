;;;; examples/regions.lisp
;;;;
;;;; #107: region-mapped memory -- a machine whose address space is split
;;;; into a ROM code region (writes ignored), a RAM data region, and a
;;;; :DEVICE output port whose write handler collects bytes instead of
;;;; touching backing storage at all. MREF/(SETF MREF) (storage.lisp) route
;;;; through whichever region an address falls in; a machine declaring none
;;;; (every earlier example) is unaffected.
;;;;
;;;; Run with:  sbcl --script examples/regions.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

;;; The output port: a :DEVICE region's :WRITE receives the machine and the
;;; absolute address it was written to; it collects the value instead of
;;; storing it anywhere. Declared before DEFMACHINE below purely for
;;; readability -- as a function *designator* (a bare symbol, not #'PORT-
;;; WRITE), it's resolved by FUNCALL at call time rather than DEFMACHINE
;;; time, so the definition order doesn't actually matter.
(defvar *port-output* nil)

(defun port-write (machine address value)
  (declare (ignore machine address))
  (cl:push value *port-output*))

(defmachine regionsfoo
  (register pc :width 16)
  (register a :width 8)
  (memory ram :width 8 :addr-width 16
    (region rom  #x0000 #x0FFF :kind :rom)
    (region data #x1000 #x1FFF)                 ; :RAM, the default
    (region port #x2000 #x2000 :kind :device :write port-write))
  (flags z))

(definstruction regionsfoo lda
  (modes immediate)
  (encoding (opcode #xA2) (operand :mode))
  (semantics (set! a operand) (set-flags! (z (zero? a)))))

(definstruction regionsfoo sta
  (modes absolute)
  (encoding (opcode #x8D) (operand :mode))
  (semantics (setf (mref machine 'ram operand) a)))

(definstruction regionsfoo hlt
  (encoding (opcode #x00))
  (semantics (trap :halt)))

(defparameter *source*
  "lda #65       ; a = 'A'
sta $2000     ; write to the output port
lda #66       ; a = 'B'
sta $2000     ; write to the output port
sta $1000     ; also store to RAM, ordinary
hlt")

(format t "~&Source:~%~A~2%" *source*)

;; A program assembled to load at $0000 (ROM) still loads -- LOAD-PROGRAM
;; burns cells in via %POKE, bypassing the ROM region's write protection
;; entirely, since a ROM image is burned in rather than stored by the CPU.
(let* ((assembly (assemble *source* :machine 'regionsfoo))
       (m (make-machine 'regionsfoo)))
  (load-program m assembly)
  (format t "Loaded ~D cells into ROM at $0000.~%" (length (assembly-cells assembly)))

  (multiple-value-bind (reason steps) (run m)
    (format t "~%Running: stopped ~A after ~D step~:P~%" reason steps)
    (assert (eq :trap reason))

    (format t "Output port received: ~S~%" (reverse *port-output*))
    (assert (equal '(65 66) (reverse *port-output*)))

    (format t "RAM[$1000] = ~D~%" (mref m 'ram #x1000))
    (assert (= 66 (mref m 'ram #x1000)))

    ;; The output port has no backing storage of its own -- MPEEK (which
    ;; never consults a region) reads 0 there, same as any never-written
    ;; cell, confirming the writes never landed in the array.
    (assert (= 0 (mpeek m 'ram #x2000))))

  ;; A CPU store into ROM is silently dropped -- :ON-WRITE defaults to
  ;; :IGNORE. The loaded program is untouched.
  (format t "~%A CPU store into the ROM region is dropped:~%")
  (let ((before (mref m 'ram #x0000)))
    (setf (mref m 'ram #x0000) 99)
    (format t "  RAM[$0000] before=~D after attempted write=~D (unchanged)~%"
            before (mref m 'ram #x0000))
    (assert (= before (mref m 'ram #x0000))))

  ;; RESET zeroes RAM but leaves the burned ROM image alone (#157).
  (reset m)
  (format t "~%After RESET: ROM[$0000]=~D (kept), RAM[$1000]=~D (cleared)~%"
          (mref m 'ram #x0000) (mref m 'ram #x1000))
  (assert (= (aref (assembly-cells assembly) 0) (mref m 'ram #x0000)))
  (assert (= 0 (mref m 'ram #x1000)))

  (format t "~%All assertions passed.~%"))
