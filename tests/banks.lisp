;;;; tests/banks.lisp
;;;; fiveam tests for banked output: the .bank directive, bank images,
;;;; listings, physical-layout output, loading, disassembly, the debugger's
;;;; bank commands and the CLI's --bank option.

(in-package #:lasm)

(fiveam:def-suite banks :in lasm)
(fiveam:in-suite banks)

(defmachine bank-asm-machine
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region romx #x4000 #x40FF :kind :rom :banks 4)))

(definstruction bank-asm-machine nop
  (encoding (opcode #x00))
  (semantics (set! a a)))

(definstruction bank-asm-machine hlt
  (encoding (opcode #x01))
  (semantics (trap :halt)))

(defmachine bank-plain-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defmachine bank-two-region-machine
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region one #x4000 #x40FF :banks 2)
    (region two #x5000 #x50FF :banks 2)))

(definstruction bank-plain-machine nop
  (encoding (opcode #x00))
  (semantics (set! pc pc)))

(definstruction bank-two-region-machine nop
  (encoding (opcode #x00))
  (semantics (set! pc pc)))

(defparameter *bank-source*
  "        nop
        .bank 2
        .org $4000
far:    hlt
        .byte 7, 8
        .bank 1
        .org $4000
        .byte 9
        .org $0010
late:   nop")

(defun %bank-assembly (&optional (source *bank-source*))
  (assemble source :machine 'bank-asm-machine))

(defun %image-head (assembly bank n)
  (subseq (bank-image-cells (assembly-bank-image assembly 'romx bank)) 0 n))

;;; Assembly

(fiveam:test bank-routes-output-by-address
  (let ((a (%bank-assembly)))
    (fiveam:is (= 17 (length (assembly-cells a))))
    (fiveam:is (= 0 (aref (assembly-cells a) 16)))
    (fiveam:is (equalp #(1 7 8 0) (%image-head a 2 4)))
    (fiveam:is (equalp #(9 0 0 0) (%image-head a 1 4)))
    (fiveam:is (equal '(1 2) (mapcar #'bank-image-bank (assembly-banks a))))
    (fiveam:is (= #x4000 (bank-image-origin (first (assembly-banks a)))))
    (fiveam:is (= 256 (length (bank-image-cells (first (assembly-banks a))))))))

(fiveam:test bank-does-not-move-the-origin
  (let ((a (%bank-assembly ".bank 1
.org $4000
.byte 5
.org $0100
nop")))
    (fiveam:is (= #x100 (assembly-origin a)))
    (fiveam:is (= 1 (length (assembly-cells a))))))

(fiveam:test bank-labels-carry-their-bank
  (let* ((a (%bank-assembly))
         (far (assembly-symbol a "far"))
         (late (assembly-symbol a "late")))
    (fiveam:is (= #x4000 (symbol-info-value far)))
    (fiveam:is (eql 2 (symbol-info-bank far)))
    (fiveam:is (eq 'romx (symbol-info-region far)))
    (fiveam:is (null (symbol-info-bank late)))))

(fiveam:test bank-reserve-marks-the-bank
  (let ((a (%bank-assembly ".bank 3
.org $4000
.res 4
.byte 1")))
    (fiveam:is (equalp #(0 0 0 0 1) (%image-head a 3 5)))))

(fiveam:test bank-overlap-is-an-error
  (fiveam:signals assembly-error
    (%bank-assembly ".bank 1
.org $4000
.byte 1, 2
.org $4001
.byte 3"))
  (fiveam:finishes
    (%bank-assembly ".bank 1
.org $4000
.byte 1
.bank 2
.org $4000
.byte 2")))

(fiveam:test bank-out-of-range-is-an-error
  (fiveam:signals assembly-error
    (%bank-assembly ".bank 4
.org $4000
.byte 1")))

(fiveam:test bank-section-may-not-cross-the-region-edge
  (fiveam:signals assembly-error
    (%bank-assembly ".bank 0
.org $40FF
.byte 1, 2")))

(fiveam:test bank-needs-a-banked-region
  (fiveam:signals assembly-error
    (assemble ".bank 1
nop" :machine 'bank-plain-machine)))

(fiveam:test bank-operand-must-be-a-constant
  (fiveam:signals assembly-error
    (%bank-assembly ".bank -1")))

(fiveam:test bank-unselected-output-stays-in-the-main-image
  (let ((a (%bank-assembly ".org $4000
.byte 5")))
    (fiveam:is (null (assembly-banks a)))
    (fiveam:is (= #x4000 (assembly-origin a)))))

(fiveam:test main-backward-org-still-errors-after-banks
  (fiveam:signals assembly-error
    (%bank-assembly "nop
nop
.bank 1
.org $4000
.byte 1
.org $0001
nop")))

;;; Listing and symbols

(fiveam:test bank-listing-prefixes-banked-addresses
  (let ((text (listing-text (%bank-assembly))))
    (fiveam:is (search "02:4000" text))
    (fiveam:is (search "01:4000" text))
    (fiveam:is (search "0010" text))))

(fiveam:test bank-listing-line-at-selects-the-image
  (let ((a (%bank-assembly)))
    (fiveam:is (null (listing-line-at a #x4000)))
    (fiveam:is (eql 4 (listing-line-line (listing-line-at a #x4000 :region 'romx :bank 2))))
    (fiveam:is (eql 8 (listing-line-line (listing-line-at a #x4000 :region 'romx :bank 1))))))

(fiveam:test bank-symbols-text-shows-the-bank
  (fiveam:is (search "02:4000" (symbols-text (%bank-assembly)))))

(fiveam:test bank-data-regions-are-per-image
  (let ((a (%bank-assembly)))
    (fiveam:is (equal '((#x4001 . #x4003)) (assembly-data-regions a :region 'romx :bank 2)))
    (fiveam:is (null (assembly-data-regions a)))))

;;; Output

(fiveam:test bank-bytes-are-the-physical-layout
  (let ((bytes (assembly-bytes (%bank-assembly))))
    (fiveam:is (= (+ 17 (* 3 256)) (length bytes)))
    (fiveam:is (= 9 (aref bytes (+ 17 256))))
    (fiveam:is (= 1 (aref bytes (+ 17 512))))
    (fiveam:is (= 0 (aref bytes 17)))))

(fiveam:test bank-bytes-can-select-one-bank
  (let ((a (%bank-assembly)))
    (fiveam:is (equalp #(1 7 8) (subseq (assembly-bytes a :bank 2) 0 3)))
    (fiveam:is (= 256 (length (assembly-bytes a :bank 2))))
    (fiveam:is (= 0 (length (assembly-bytes a :bank 3))))))

(fiveam:test bank-bytes-need-a-region-when-several-have-output
  (let ((a (assemble ".bank 0
.org $4000
nop
.org $5000
nop" :machine 'bank-two-region-machine)))
    (fiveam:is (equal '(one two) (mapcar #'bank-image-region (assembly-banks a))))
    (fiveam:signals error (assembly-bytes a :bank 0))
    (fiveam:is (= 256 (length (assembly-bytes a :bank 0 :region 'two))))))

(fiveam:test bank-hex-starts-at-the-region-address
  (let ((text (hex-text (%bank-assembly) :bank 2)))
    (fiveam:is (string= ":10400000" (subseq text 0 9)))))

;;; Loading

(fiveam:test bank-load-program-fills-every-bank
  (let ((m (make-machine 'bank-asm-machine)))
    (load-program m (%bank-assembly))
    (fiveam:is (= 0 (current-bank m 'romx)))
    (fiveam:is (= 1 (bank-peek m 'romx 2 #x4000)))
    (fiveam:is (= 7 (bank-peek m 'romx 2 #x4001)))
    (fiveam:is (= 9 (bank-peek m 'romx 1 #x4000)))
    (fiveam:is (= 0 (sref m 'pc)))))

;;; Disassembly

(fiveam:test bank-disassembles-one-image
  (let* ((a (%bank-assembly))
         (text (disassembly-text
                (disassemble-assembly a :machine 'bank-asm-machine :bank 2)
                :origin #x4000)))
    (fiveam:is (search ".org 16384" text))
    (fiveam:is (search "hlt" text))
    (fiveam:is (search ".byte" text))))

;;; Debugger

(defun %bank-session ()
  (let ((m (make-machine 'bank-asm-machine)))
    (load-program m (%bank-assembly))
    (make-debug-session m :assembly (%bank-assembly))))

(fiveam:test debug-info-banks-lists-regions
  (let ((text (debug-command (%bank-session) "info banks")))
    (fiveam:is (search "romx" text))
    (fiveam:is (search "4000-40FF" text))
    (fiveam:is (search "bank 0/4" text))))

(fiveam:test debug-info-banks-without-banked-regions
  (let ((session (make-debug-session (make-machine 'bank-plain-machine))))
    (fiveam:is (search "No banked regions" (debug-command session "info banks")))))

(fiveam:test debug-bank-command-switches-the-mapping
  (let ((session (%bank-session)))
    (fiveam:is (search "romx bank 2" (debug-command session "bank romx 2")))
    (fiveam:is (= 2 (current-bank (debug-session-machine session) 'romx)))
    (fiveam:is (search "bank 2/4" (debug-command session "info banks")))
    (fiveam:is (search "01 07 08" (debug-command session "x/3 $4000")))))

(fiveam:test debug-bank-command-rejects-bad-input
  (let ((session (%bank-session)))
    (fiveam:is (search "out of range" (debug-command session "bank romx 9")))
    (fiveam:is (search "usage" (debug-command session "bank romx")))
    (fiveam:is (search "Error" (debug-command session "bank nosuch 1")))))

(fiveam:test debug-memory-dump-reads-an-unmapped-bank
  (let ((session (%bank-session)))
    (fiveam:is (search "01 07 08" (debug-command session "x/3 2:$4000")))
    (fiveam:is (= 0 (current-bank (debug-session-machine session) 'romx)))
    (fiveam:is (search "09" (debug-command session "x/1 1:$4000")))))

(fiveam:test debug-memory-dump-checks-the-bank-range-first
  (let ((session (%bank-session)))
    (fiveam:is (search "run past" (debug-command session "x/4 2:$40FE")))
    (fiveam:is (search "not in a banked region" (debug-command session "x/4 2:$0000")))
    (fiveam:is (search "x/N: bad address" (debug-command session "x/4 z:$4000")))))

(fiveam:test debug-where-uses-the-mapped-banks-source-line
  (let ((session (%bank-session)))
    (debug-command session "bank romx 2")
    (setf (sref (debug-session-machine session) 'pc) #x4000)
    (fiveam:is (search "far:" (debug-where-text session)))
    (debug-command session "bank romx 1")
    (fiveam:is (search ".byte 9" (debug-where-text session)))))

;;; CLI

(defun %banked-cli-args (command &rest more)
  (list* command (%cli-path "tests/fixtures/cli/banked.asm")
         "-m" (%cli-path "tests/fixtures/cli/banked.lasm") more))

(fiveam:test cli-assemble-writes-the-physical-layout
  (uiop:with-temporary-file (:pathname path :type "bin")
    (multiple-value-bind (status out)
        (%run-cli (append (%banked-cli-args "assemble") (list "-o" (namestring path))))
      (fiveam:is (= 0 status))
      (fiveam:is (search "770 bytes" out))
      (fiveam:is (= 770 (length (%cli-read-bytes path)))))))

(fiveam:test cli-assemble-bank-writes-one-image
  (uiop:with-temporary-file (:pathname path :type "bin")
    (multiple-value-bind (status out)
        (%run-cli (append (%banked-cli-args "assemble")
                          (list "-o" (namestring path) "--bank" "2")))
      (fiveam:is (= 0 status))
      (fiveam:is (search "256 bytes" out))
      (fiveam:is (equalp #(1 7 8) (subseq (%cli-read-bytes path) 0 3))))))

(fiveam:test cli-assemble-bank-hex-uses-region-addresses
  (uiop:with-temporary-file (:pathname path :type "hex")
    (%run-cli (append (%banked-cli-args "assemble")
                      (list "-o" (namestring path) "--format" "hex" "--bank" "2" "--region" "romx")))
    (fiveam:is (string= ":10400000" (subseq (uiop:read-file-string path) 0 9)))))

(fiveam:test cli-run-loads-every-bank
  (multiple-value-bind (status out) (%run-cli (%banked-cli-args "run"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped: trap" out))))

(fiveam:test cli-bank-needs-an-integer
  (fiveam:is (= 2 (%run-cli (append (%banked-cli-args "assemble") (list "--bank" "x"))))))
