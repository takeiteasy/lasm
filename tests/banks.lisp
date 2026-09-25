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

;;; bank(label)

(defmachine bank-op-machine
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16
    (region romx #x4000 #x40FF :kind :rom :banks 4)))

(definstruction bank-op-machine bnk
  (modes immediate)
  (encoding (opcode #x10) (operand :mode))
  (semantics (set-bank! romx operand)))

(defun %bank-op-assembly (source)
  (assemble source :machine 'bank-op-machine))

(fiveam:test bank-operator-folds-to-a-label-bank
  (let ((a (%bank-op-assembly "        bnk #bank(far)
        .byte bank(far), bank(back)
        .bank 1
        .org $4000
back:   .byte 0
        .bank 3
        .org $4000
far:    .byte 0")))
    (fiveam:is (equalp #(#x10 3 3 1) (subseq (assembly-cells a) 0 4)))))

(fiveam:test bank-operator-accepts-local-labels-and-equ
  (let ((a (%bank-op-assembly "        .bank 2
        .org $4000
main:   .byte 0
.loc:   .byte 0
        .bank 0
        .byte bank(.loc), bank(main)
here    = bank(main)
        .byte here")))
    (fiveam:is (equalp #(2 2 2) (subseq (bank-image-cells (assembly-bank-image a 'romx 0)) 2 5)))))

(fiveam:test bank-operator-errors
  (fiveam:signals assembly-error
    (%bank-op-assembly "main:   .byte 0
        .byte bank(main)"))
  (fiveam:signals assembly-error
    (%bank-op-assembly "n = 2
        .byte bank(n)"))
  (fiveam:signals unresolved-label
    (%bank-op-assembly "        .byte bank(nowhere)"))
  (fiveam:signals assembly-error
    (%bank-op-assembly "        .bank 1
        .org $4000
x:      .byte 0
        .set y, x
        .byte bank(y)")))

(fiveam:test bank-operator-is-unresolved-outside-the-assembler
  (fiveam:signals unresolved-label
    (eval-expr-constant (%expr "bank(x)"))))

;;; bank(*)

(fiveam:test bank-here-folds-to-the-selected-bank
  (let ((a (%bank-op-assembly "        .bank 2
        .org $4000
        bnk #bank(*)
        .byte bank(*)
here    = bank(*)
        .byte here")))
    (fiveam:is (equalp #(#x10 2 2 2) (%image-head a 2 4)))))

(fiveam:test bank-here-errors-outside-a-banked-region
  (fiveam:signals assembly-error
    (%bank-op-assembly "        .byte bank(*)"))
  (fiveam:signals assembly-error
    (%bank-op-assembly "        .bank 1
        .org $0100
        .byte bank(*)")))

(fiveam:test bank-here-needs-a-pc
  (fiveam:signals unresolved-location
    (eval-expr-constant (%expr "bank(*)"))))

;;; Bank-qualified breakpoints

(defun %bp-session (&optional (mapped 0))
  (let* ((a (assemble "        .org $3FFF
        nop
        .bank 1
        .org $4000
near:   nop
        .bank 2
        .org $4000
far:    nop" :machine 'bank-asm-machine))
         (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (setf (current-bank m 'romx) mapped)
    (make-debug-session m :assembly a)))

(defun %bp-run-from-entry (session)
  (setf (sref (debug-session-machine session) 'pc) #x3FFF)
  (debug-continue session :max-steps 1))

(fiveam:test label-breakpoint-in-a-bank-stops-only-in-that-bank
  (let ((session (%bp-session 1)))
    (debug-break session "far")
    (fiveam:is (eq :max-steps (%bp-run-from-entry session)))
    (debug-set-bank session 'romx 2)
    (fiveam:is (eq :breakpoint (%bp-run-from-entry session)))))

(fiveam:test explicit-bank-breakpoint-matches-a-label-breakpoint
  (let ((session (%bp-session 1)))
    (fiveam:is (search "Breakpoint 1 at 02:4000" (debug-command session "break 2:$4000")))
    (fiveam:is (search "1: 02:4000" (debug-command session "info break")))
    (fiveam:is (eq :max-steps (%bp-run-from-entry session)))
    (debug-set-bank session 'romx 2)
    (fiveam:is (eq :breakpoint (%bp-run-from-entry session)))))

(fiveam:test unqualified-breakpoint-stops-in-any-bank
  (let ((session (%bp-session 1)))
    (debug-break session #x4000)
    (fiveam:is (eq :breakpoint (%bp-run-from-entry session)))))

(fiveam:test bank-breakpoint-rejects-bad-targets
  (let ((session (%bp-session)))
    (fiveam:is (search "out of range" (debug-command session "break 9:$4000")))
    (fiveam:is (search "not in a banked region" (debug-command session "break 2:$0100")))
    (fiveam:is (search "not in bank 1" (debug-command session "break 1:far")))
    (fiveam:is (search "bad bank" (debug-command session "break z:$4000")))
    (fiveam:is (null (debug-breakpoints session)))))

(fiveam:test bank-breakpoints-at-one-address-coexist
  (let ((session (%bp-session)))
    (debug-break session "near")
    (debug-break session "far")
    (fiveam:is (= 2 (length (debug-breakpoints session))))
    (debug-break session "far")
    (fiveam:is (= 2 (length (debug-breakpoints session))))))

(fiveam:test delete-by-address-removes-every-bank
  (let ((session (%bp-session)))
    (debug-break session "near")
    (debug-break session "far")
    (fiveam:is (search "Deleted" (debug-command session "delete 2:$4000")))
    (fiveam:is (= 1 (length (debug-breakpoints session))))
    (debug-break session "far")
    (fiveam:is (search "Deleted" (debug-command session "delete $4000")))
    (fiveam:is (null (debug-breakpoints session)))))

(fiveam:test until-waits-for-the-bank
  (let ((session (%bp-session 1)))
    (fiveam:is (eq :max-steps (progn (setf (sref (debug-session-machine session) 'pc) #x3FFF)
                                     (debug-continue-to session "far" :max-steps 1))))
    (debug-set-bank session 'romx 2)
    (setf (sref (debug-session-machine session) 'pc) #x3FFF)
    (fiveam:is (eq :until (debug-continue-to session "far" :max-steps 1)))))

;;; Whole-program disassembly

(defparameter *bank-roundtrip-source*
  "        nop
        .bank 2
        .org $4000
far:    hlt
        .byte 7, 8
        .bank 1
        .org $4000
near:   nop
        .res 2
        .bank 3
        .org $4010
        .res 3")

(defun %round-trip (source labels)
  (let* ((a (%bank-assembly source))
         (text (disassembly-text
                (disassemble-assembly a :machine 'bank-asm-machine :bank :all :labels labels)
                :origin (assembly-origin a))))
    (values a (%bank-assembly text) text)))

(defun %same-images-p (a b)
  (and (equalp (assembly-cells a) (assembly-cells b))
       (= (length (assembly-banks a)) (length (assembly-banks b)))
       (every (lambda (x y) (and (eq (bank-image-region x) (bank-image-region y))
                                 (= (bank-image-bank x) (bank-image-bank y))
                                 (equalp (bank-image-cells x) (bank-image-cells y))))
              (assembly-banks a) (assembly-banks b))))

(fiveam:test bank-disassembly-round-trips-without-labels
  (multiple-value-bind (a b text) (%round-trip *bank-roundtrip-source* nil)
    (fiveam:is (%same-images-p a b))
    (fiveam:is (search ".bank 2" text))
    (fiveam:is (search ".bank 3" text))))

(fiveam:test bank-disassembly-round-trips-with-labels
  (multiple-value-bind (a b text) (%round-trip *bank-roundtrip-source* t)
    (fiveam:is (%same-images-p a b))
    (fiveam:is (search "far:" text))
    (fiveam:is (search "near:" text))))

(fiveam:test bank-disassembly-only-labels-its-own-image
  (let* ((a (%bank-assembly))
         (text (disassembly-text
                (disassemble-assembly a :machine 'bank-asm-machine :bank 1)
                :origin #x4000)))
    (fiveam:is (not (search "far:" text)))))

(fiveam:test bank-disassembly-covers-the-listing-extent-only
  (let ((lines (disassemble-assembly (%bank-assembly) :machine 'bank-asm-machine :bank 2)))
    (fiveam:is (= 3 (length lines)))
    (fiveam:is (equal '(romx . 2) (cons (disassembly-line-region (first lines))
                                        (disassembly-line-bank (first lines)))))))

(fiveam:test disassembly-text-rejects-main-lines-after-a-bank
  (let ((a (%bank-assembly)))
    (fiveam:signals error
      (disassembly-text (append (disassemble-assembly a :machine 'bank-asm-machine :bank 2)
                                (disassemble-assembly a :machine 'bank-asm-machine))))))

;;; Live memory disassembly follows the mapped bank

(defparameter *bank-live-source*
  "        nop
        .bank 2
        .org $4000
far:    .byte 1
        .bank 1
        .org $4000
        hlt
        .bank 0
        .org $0010
        .byte 1")

(defun %bank-live-session ()
  (let* ((a (%bank-assembly *bank-live-source*))
         (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (make-debug-session m :assembly a)))

(defun %where-instruction-lines (session pc)
  (setf (sref (debug-session-machine session) 'pc) pc)
  (debug-where-text session :context 1))

(fiveam:test debug-where-renders-data-only-in-the-mapped-bank
  (let ((session (%bank-live-session)))
    (debug-command session "bank romx 2")
    (fiveam:is (search ".byte $1" (%where-instruction-lines session #x4000)))
    (debug-command session "bank romx 1")
    (let ((text (%where-instruction-lines session #x4000)))
      (fiveam:is (search "hlt" text))
      (fiveam:is (not (search ".byte" text))))))

(fiveam:test debug-where-renders-main-image-data-outside-banks
  (let ((session (%bank-live-session)))
    (fiveam:is (search ".byte $1" (%where-instruction-lines session #x0010)))))

(fiveam:test debug-where-substitutes-only-mapped-bank-labels
  (let ((session (%bank-live-session)))
    (debug-command session "bank romx 1")
    (let ((lines (disassemble-memory (debug-session-machine session) :start #x4000 :count 1
                                     :assembly (debug-session-assembly session))))
      (fiveam:is (null (disassembly-line-label (first lines)))))
    (debug-command session "bank romx 2")
    (let ((lines (disassemble-memory (debug-session-machine session) :start #x4000 :count 1
                                     :assembly (debug-session-assembly session))))
      (fiveam:is (equal "far" (disassembly-line-label (first lines)))))))

(fiveam:test debug-where-applies-main-image-only-to-the-bank-it-loaded-into
  (let* ((a (%bank-assembly "        .org $4000
        .byte 1
        .bank 1
        .org $4000
        hlt"))
         (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (flet ((first-text ()
             (disassembly-line-text
              (first (disassemble-memory m :start #x4000 :count 1 :assembly a :labels nil)))))
      (fiveam:is (search ".byte" (first-text)))
      (setf (current-bank m 'romx) 3)
      (fiveam:is (not (search ".byte" (first-text))))
      (setf (current-bank m 'romx) 0)
      (fiveam:is (search ".byte" (first-text)))
      (reset m)
      (fiveam:is (search ".byte" (first-text))))))

(fiveam:test reset-keeps-rom-banks-and-the-program-loaded-into-them
  (let* ((a (%bank-assembly "        .org $4000
        .byte 1
        .bank 1
        .org $4000
        hlt"))
         (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (setf (current-bank m 'romx) 1)
    (reset m)
    (fiveam:is (= 0 (current-bank m 'romx)))
    (fiveam:is (= 1 (mref m 'ram #x4000)))
    (fiveam:is (= 1 (bank-peek m 'romx 1 #x4000)))
    (fiveam:is (eq a (machine-program m)))))

(fiveam:test debug-step-back-keeps-the-loaded-bank
  (let* ((a (%bank-assembly "        .org $4000
        nop
        nop
        .byte 1"))
         (m (make-machine 'bank-asm-machine)))
    (setf (current-bank m 'romx) 2)
    (load-program m a)
    (let ((session (make-debug-session m :assembly a :history 10)))
      (debug-step session)
      (debug-step session)
      (debug-step-back session 2)
      (fiveam:is (eql 2 (gethash 'romx (machine-loaded-banks m))))
      (setf (sref m 'pc) #x4002)
      (fiveam:is (search ".byte" (debug-where-text session :context 1))))))

(fiveam:test debug-step-back-restores-an-unmapped-bank-write
  (let* ((a (%bank-assembly "        nop
        nop
        nop"))
         (m (make-machine 'bank-asm-machine))
         (session (progn (load-program m a) (make-debug-session m :assembly a :history 10))))
    (debug-step session)
    (debug-set session #x4010 5 :bank 1)
    (debug-step session)
    (debug-step-back session 1)
    (fiveam:is (= 5 (bank-peek m 'romx 1 #x4010)))
    (debug-step-back session 1)
    (fiveam:is (= 0 (bank-peek m 'romx 1 #x4010)))))

;;; Main image vs bank image at load

(fiveam:test load-program-rejects-main-output-in-a-bank-it-also-images
  (let ((a (%bank-assembly "        .org $4000
        .byte 1
        .bank 0
        .org $4010
        nop"))
        (m (make-machine 'bank-asm-machine)))
    (fiveam:signals error (load-program m a))
    (fiveam:is (= 0 (bank-peek m 'romx 0 #x4000)))
    (fiveam:is (= 0 (sref m 'pc)))))

(fiveam:test load-program-allows-main-output-in-another-bank
  (let ((a (%bank-assembly "        .org $4000
        .byte 1
        .bank 1
        .org $4010
        nop"))
        (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (fiveam:is (= 1 (bank-peek m 'romx 0 #x4000)))))

(fiveam:test load-program-checks-the-mapping-at-load
  (let ((source "        .org $4000
        .byte 1
        .bank ~D
        .org $4010
        nop")
        (m (make-machine 'bank-asm-machine)))
    (setf (current-bank m 'romx) 1)
    (fiveam:signals error (load-program m (%bank-assembly (format nil source 1))))
    (fiveam:finishes (load-program m (%bank-assembly (format nil source 0))))))

(fiveam:test load-program-ignores-main-padding-across-a-window
  (let ((a (%bank-assembly "        nop
        .org $5000
        nop
        .bank 0
        .org $4000
        hlt"))
        (m (make-machine 'bank-asm-machine)))
    (load-program m a)
    (fiveam:is (= 1 (bank-peek m 'romx 0 #x4000)))))
