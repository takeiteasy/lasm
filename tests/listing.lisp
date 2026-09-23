;;;; tests/listing.lisp
;;;; fiveam tests for listing.lisp (#25): the retained address<->statement
;;;; mapping (ASSEMBLY-LISTING, assembler.lisp) and its rendering/lookup
;;;; entry points. Reuses INSTR-TEST-MACHINE (tests/instruction.lisp) for the
;;;; byte-encoded fixture, same as tests/assembler.lisp, and
;;;; DISASM-WORD-MACHINE (tests/disassembler.lisp) for the cell-width case.

(in-package #:lasm)

(fiveam:def-suite listing :in lasm)
(fiveam:in-suite listing)

;;; Entries match their statements

(fiveam:test listing-entries-match-statements
  (let* ((a (assemble "start: ldx #10
adc $10
end: nop" :machine 'instr-test-machine))
         (entries (assembly-listing a)))
    (fiveam:is (= 3 (length entries)))
    (destructuring-bind (e1 e2 e3) entries
      (fiveam:is (= 0 (listing-line-address e1)))
      (fiveam:is (= 2 (listing-line-size e1)))
      (fiveam:is (= 1 (listing-line-line e1)))
      (fiveam:is (eq :instruction (listing-line-kind e1)))
      (fiveam:is (not (null (listing-line-descriptor e1))))
      (fiveam:is (= 2 (listing-line-address e2)))
      (fiveam:is (= 3 (listing-line-size e2)))
      (fiveam:is (= 2 (listing-line-line e2)))
      (fiveam:is (= 5 (listing-line-address e3)))
      (fiveam:is (= 1 (listing-line-size e3)))
      (fiveam:is (= 3 (listing-line-line e3))))))

(fiveam:test listing-entries-ascending-non-overlapping
  (let* ((a (assemble "ldx #1
adc $10
nop
ldx #2" :machine 'instr-test-machine))
         (entries (assembly-listing a)))
    (loop for (this next) on entries
          while next
          do (fiveam:is (<= (+ (listing-line-address this) (listing-line-size this))
                             (listing-line-address next))))))

;;; .byte/.word (:emit) and .res (:reserve) sizing; .org/.equ contribute no entry

(fiveam:test emit-entry-size-is-width-times-count
  (let* ((a (assemble ".byte 1, 2, 3
.word 100, 200" :machine 'instr-test-machine))
         (entries (assembly-listing a)))
    (fiveam:is (= 2 (length entries)))
    (destructuring-bind (byte-entry word-entry) entries
      (fiveam:is (eq :emit (listing-line-kind byte-entry)))
      (fiveam:is (= 3 (listing-line-size byte-entry)))
      (fiveam:is (eq :emit (listing-line-kind word-entry)))
      (fiveam:is (= 4 (listing-line-size word-entry))))))

(fiveam:test reserve-entry-size-is-count
  (let* ((a (assemble ".res 5" :machine 'instr-test-machine))
         (entries (assembly-listing a)))
    (fiveam:is (= 1 (length entries)))
    (fiveam:is (eq :reserve (listing-line-kind (first entries))))
    (fiveam:is (= 5 (listing-line-size (first entries))))))

(fiveam:test org-and-equ-contribute-no-entry-but-source-line-still-renders
  (let* ((a (assemble "size = 4
.org $10
nop" :machine 'instr-test-machine)))
    (fiveam:is (= 1 (length (assembly-listing a))))
    (let ((text (listing-text a)))
      (fiveam:is (search "size = 4" text))
      (fiveam:is (search ".org $10" text))
      (fiveam:is (search "nop" text)))))

;;; Cells sliced from ASSEMBLY-CELLS (via %LISTING-CELLS, listing.lisp)
;;; reproduce the whole vector, concatenated in entry order

(fiveam:test listing-cells-slices-reproduce-assembly-cells
  (let* ((a (assemble "ldx #10
adc $20
nop" :machine 'instr-test-machine)))
    (fiveam:is (equalp (coerce (assembly-cells a) 'list)
                        (mapcan (lambda (l) (%listing-cells a l)) (assembly-listing a))))))

(fiveam:test listing-cells-slices-account-for-non-zero-origin
  (let* ((a (assemble "ldx #10
adc $20" :machine 'instr-test-machine :origin #x200)))
    (fiveam:is (equalp (coerce (assembly-cells a) 'list)
                        (mapcan (lambda (l) (%listing-cells a l)) (assembly-listing a))))))

;;; LISTING-LINE-AT

(fiveam:test listing-line-at-finds-interior-address
  (let ((a (assemble "ldx #10
adc $20" :machine 'instr-test-machine)))
    (fiveam:is (= 0 (listing-line-address (listing-line-at a 0))))
    (fiveam:is (= 0 (listing-line-address (listing-line-at a 1))))
    (fiveam:is (= 2 (listing-line-address (listing-line-at a 2))))
    (fiveam:is (= 2 (listing-line-address (listing-line-at a 4))))))

(fiveam:test listing-line-at-nil-in-org-gap-and-past-end
  (let ((a (assemble "nop
.org $10
nop" :machine 'instr-test-machine)))
    (fiveam:is (null (listing-line-at a 5)))
    (fiveam:is (null (listing-line-at a 100)))))

;;; LISTING-LINES-FOR-SOURCE-LINE and macro expansion

(fiveam:test lines-for-source-line-distinguishes-macro-invocations
  (let ((a (assemble ".macro two
nop
.endm
two
two" :machine 'instr-test-machine)))
    (fiveam:is (null (listing-lines-for-source-line a 2)))
    (fiveam:is (equal '(0 1) (mapcar #'listing-line-address (assembly-listing a))))
    (fiveam:is (= 4 (listing-line-line (first (assembly-listing a)))))
    (fiveam:is (= 5 (listing-line-line (second (assembly-listing a)))))
    (fiveam:is (every (lambda (entry) (= 2 (listing-line-definition-line entry)))
                      (assembly-listing a)))
    (fiveam:is (= 1 (length (listing-lines-for-source-line a 4))))
    (fiveam:is (= 1 (length (listing-lines-for-source-line a 5))))
    (fiveam:is (search "0000" (listing-text a)))
    (fiveam:is (search "0001" (listing-text a)))))

(fiveam:test nested-macro-listing-points-to-outer-call
  (let* ((a (assemble ".macro inner
nop
.endm
.macro outer
inner
.endm
outer" :machine 'instr-test-machine))
         (entry (first (assembly-listing a))))
    (fiveam:is (= 7 (listing-line-line entry)))
    (fiveam:is (= 2 (listing-line-definition-line entry)))))

(fiveam:test macro-symbols-follow-call-order-and-keep-definition-lines
  (let* ((a (assemble ".macro tagged
tag: nop
.endm
tagged
tagged" :machine 'instr-test-machine))
         (symbols (assembly-symbols-list a :kind :label)))
    (fiveam:is (equal '(4 5) (mapcar #'symbol-info-line symbols)))
    (fiveam:is (equal '(2 2) (mapcar #'symbol-info-definition-line symbols)))
    (fiveam:is (equal '(0 1) (mapcar #'symbol-info-value symbols)))))

(fiveam:test symbols-within-one-macro-call-follow-body-order
  (let* ((a (assemble ".macro pair
first: nop
second: nop
.endm
pair" :machine 'instr-test-machine))
         (symbols (assembly-symbols-list a :kind :label)))
    (fiveam:is (equal '(5 5) (mapcar #'symbol-info-line symbols)))
    (fiveam:is (equal '(2 3) (mapcar #'symbol-info-definition-line symbols)))
    (fiveam:is (equal '(0 1) (mapcar #'symbol-info-value symbols)))))

(fiveam:test nested-macro-invocation-label-keeps-outer-body-line
  (let* ((a (assemble ".macro inner
nop
.endm
.macro outer
here: inner
.endm
outer" :machine 'instr-test-machine))
         (symbol (first (assembly-symbols-list a :kind :label))))
    (fiveam:is (= 7 (symbol-info-line symbol)))
    (fiveam:is (= 5 (symbol-info-definition-line symbol)))))

;;; LISTING-TEXT

(fiveam:test listing-text-with-source-includes-every-line-once
  (let* ((a (assemble "start: ldx #10
adc $20" :machine 'instr-test-machine))
         (text (listing-text a)))
    (fiveam:is (= 1 (count #\Newline text :start (or (search "ldx" text) 0)
                                          :end (search "adc" text))))
    (fiveam:is (search "0000" text))
    (fiveam:is (search "A2 0A" text))
    (fiveam:is (search "ldx #10" text))))

(fiveam:test listing-text-without-source-omits-source-column
  (let* ((stmts (parse "ldx #10"))
         (a (assemble-statements stmts :machine 'instr-test-machine))
         (text (listing-text a)))
    (fiveam:is (null (assembly-source a)))
    (fiveam:is (search "A2 0A" text))
    (fiveam:is (not (search "ldx" text)))))

(fiveam:test listing-text-trailing-newline-adds-no-blank-row
  ;; A source ending in a newline (the common case for a real file) must not
  ;; render one spurious blank row past the last statement, matching
  ;; PARSE's own %SPLIT-LINES treatment of a trailing newline.
  (let* ((a (assemble (format nil "ldx #10~%") :machine 'instr-test-machine))
         (text (listing-text a)))
    (fiveam:is (= 1 (count #\Newline text)))))

;;; Cell hex width follows ASSEMBLY-CELL-WIDTH

(fiveam:test listing-hex-width-follows-cell-width
  (let* ((a8 (assemble "ldx #10" :machine 'instr-test-machine))
         (a16 (assemble "hlt" :machine 'disasm-word-machine)))
    (fiveam:is (search "A2 0A" (listing-text a8)))
    (fiveam:is (not (search "A2 0A0A" (listing-text a8))))
    ;; DISASM-WORD-MACHINE's HLT is a single 16-bit-cell opcode; its 4-digit
    ;; hex rendering must not collapse to 2.
    (let* ((cells (assembly-cells a16))
           (text (listing-text a16)))
      (fiveam:is (search (format nil "~4,'0X" (aref cells 0)) text)))))

;;; Cross-check vs. #21's disassembler -- an instruction-only program (no
;;; trailing .byte data, whose sizes could legitimately diverge from a
;;; from-scratch decode) should agree address-for-address, size-for-size.

(fiveam:test listing-matches-disassembler-addresses-and-sizes
  (let* ((a (assemble "start: ldx #10
adc $20
bra start
nop" :machine 'instr-test-machine))
         (listing (assembly-listing a))
         (disasm (disassemble-assembly a :machine 'instr-test-machine :labels nil)))
    (fiveam:is (= (length listing) (length disasm)))
    (loop for l in listing
          for d in disasm
          do (fiveam:is (= (listing-line-address l) (disassembly-line-address d)))
             (fiveam:is (= (listing-line-size l) (disassembly-line-size d))))))

;;; Symbol table (#37) -- scope-aware lookup, filtering, grouping, and
;;; rendering over ASSEMBLY-SYMBOL-INFO.

(fiveam:test assembly-symbol-looks-up-a-global-and-a-scoped-local
  (let ((a (assemble "loop: nop
.next: nop" :machine 'instr-test-machine)))
    (fiveam:is (string= "loop" (symbol-info-name (assembly-symbol a "loop"))))
    (fiveam:is (null (assembly-symbol a ".next")))  ; unscoped, not found
    (fiveam:is (string= ".next" (symbol-info-name (assembly-symbol a ".next" :scope "loop"))))
    (fiveam:is (null (assembly-symbol a ".missing" :scope "loop")))
    (fiveam:is (null (assembly-symbol a "nonexistent")))))

(fiveam:test assembly-symbols-list-filters-by-kind-and-scope
  (let* ((a (assemble ".equ top, 1
loop: nop
.equ .n, 2
.next: nop" :machine 'instr-test-machine))
         (labels (assembly-symbols-list a :kind :label))
         (equs (assembly-symbols-list a :kind :equ))
         (top-level (assembly-symbols-list a :scope nil))
         (under-loop (assembly-symbols-list a :scope "loop")))
    (fiveam:is (= 2 (length labels)))
    (fiveam:is (every (lambda (s) (eq :label (symbol-info-kind s))) labels))
    (fiveam:is (= 2 (length equs)))
    (fiveam:is (every (lambda (s) (eq :equ (symbol-info-kind s))) equs))
    (fiveam:is (= 2 (length top-level)))  ; "top" and "loop"
    (fiveam:is (every (lambda (s) (null (symbol-info-scope s))) top-level))
    (fiveam:is (= 2 (length under-loop)))  ; ".n" and ".next"
    (fiveam:is (every (lambda (s) (string= "loop" (symbol-info-scope s))) under-loop))))

(fiveam:test assembly-symbols-list-with-no-filter-returns-every-symbol
  (let ((a (assemble ".equ top, 1
loop: nop
.next: nop" :machine 'instr-test-machine)))
    (fiveam:is (= 3 (length (assembly-symbols-list a))))))

(fiveam:test assembly-symbol-groups-buckets-locals-under-their-global
  (let* ((a (assemble ".equ bufsize, 16
start: nop
.loop: nop
delay: nop
.loop: nop" :machine 'instr-test-machine))
         (groups (assembly-symbol-groups a))
         (top (cdr (assoc nil groups)))
         (start-group (cdr (assoc "start" groups :test #'equal)))
         (delay-group (cdr (assoc "delay" groups :test #'equal))))
    (fiveam:is (= 3 (length groups)))  ; nil, "start", "delay"
    (fiveam:is (= 1 (length top)))
    (fiveam:is (string= "bufsize" (symbol-info-name (first top))))
    (fiveam:is (= 2 (length start-group)))
    (fiveam:is (string= "start" (symbol-info-name (first start-group))))
    (fiveam:is (string= ".loop" (symbol-info-name (second start-group))))
    (fiveam:is (= 2 (length delay-group)))
    (fiveam:is (string= "delay" (symbol-info-name (first delay-group))))
    (fiveam:is (string= ".loop" (symbol-info-name (second delay-group))))))

(fiveam:test assembly-symbol-groups-empty-when-no-symbol-info
  ;; An ASSEMBLY built without going through the layout pass at all (or any
  ;; caller that never populated SYMBOL-INFO) still degrades to an empty --
  ;; not erroring -- leading NIL bucket.
  (let ((a (make-assembly :cells #() :symbols (make-hash-table :test 'equal))))
    (fiveam:is (equal '((nil)) (assembly-symbol-groups a)))
    (fiveam:is (null (assembly-symbols-list a)))
    (fiveam:is (null (assembly-symbol a "anything")))))

(fiveam:test symbols-text-renders-every-symbol-grouped
  (let* ((a (assemble ".equ bufsize, 16
start: nop
.loop: nop" :machine 'instr-test-machine))
         (text (symbols-text a)))
    (fiveam:is (search "bufsize" text))
    (fiveam:is (search "start" text))
    (fiveam:is (search ".loop" text))
    (fiveam:is (search "equ" text))
    (fiveam:is (search "label" text))))

(fiveam:test print-symbols-writes-to-stream-and-returns-the-assembly
  (let* ((a (assemble "start: nop" :machine 'instr-test-machine))
         (out (with-output-to-string (s)
                (fiveam:is (eq a (print-symbols a :stream s))))))
    (fiveam:is (search "start" out))))

;;; Data regions (#82)

(fiveam:test assembly-data-regions-cover-emit-and-reserve
  (let ((a (assemble "ldx #1
.byte 1, 2
.res 3
nop
.byte 9" :machine 'instr-test-machine)))
    (fiveam:is (equal '((2 . 7) (8 . 9)) (assembly-data-regions a)))))

(fiveam:test assembly-data-regions-empty-without-data-or-listing
  (fiveam:is (null (assembly-data-regions (assemble "nop" :machine 'instr-test-machine))))
  (fiveam:is (null (assembly-data-regions (make-assembly)))))
