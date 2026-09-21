(in-package #:lasm)

(fiveam:in-suite instruction)

(defmachine fixed-choice-machine
  (register a :width 16)
  (memory ram :width 8 :addr-width 16)
  (instruction-word :width 16
    (field opcode 4)
    (field dst 4)
    (field src 8)))

(defmode fixed-choice-mode
  (one-of (fixed-slot test-fixed-sp word-imm)))

(definstruction fixed-choice-machine fixedchoice
  (modes fixed-choice-mode)
  (encoding
    (opcode 1)
    (for-choice (fixed-slot test-fixed-sp) (field-value dst 3))
    (for-choice (fixed-slot word-imm)
      (operand value :field dst
        (variant (choice word-imm) inline :range (0 2)))))
  (semantics
    (choice-case fixed-slot
      (test-fixed-sp (set! a 1))
      (word-imm (set! a value)))))

(fiveam:test literal-only-one-of-assembles-decodes-and-disassembles
  (let ((assembly (assemble "fixedchoice SP" :machine 'fixed-choice-machine)))
    (fiveam:is (equalp #(0 #x13) (assembly-cells assembly)))
    (multiple-value-bind (descriptor values size choices selections)
        (decode-instruction-at (vector-cell-reader (assembly-cells assembly)) 0 'fixed-choice-machine)
      (fiveam:is (eq 'fixedchoice (intern (instruction-descriptor-name descriptor) :lasm)))
      (fiveam:is (null values))
      (fiveam:is (= 2 size))
      (fiveam:is (null choices))
      (fiveam:is (equal '((fixed-slot . test-fixed-sp)) selections)))
    (fiveam:is (equal '("fixedchoice SP")
                       (mapcar #'disassembly-line-text
                               (disassemble-assembly assembly :machine 'fixed-choice-machine))))))

(fiveam:test literal-only-one-of-selects-the-other-tuple
  (let ((assembly (assemble "fixedchoice #2" :machine 'fixed-choice-machine)))
    (fiveam:is (equalp #(0 #x12) (assembly-cells assembly)))
    (multiple-value-bind (descriptor values size choices selections)
        (decode-instruction-at (vector-cell-reader (assembly-cells assembly)) 0 'fixed-choice-machine)
      (declare (ignore size choices))
      (fiveam:is (equal '(2) values))
      (fiveam:is (equal '((fixed-slot . word-imm)) selections))
      (let ((machine (make-machine 'fixed-choice-machine)))
        (execute-instruction descriptor machine values selections)
        (fiveam:is (= 2 (sref machine 'a)))))))

(defmachine independent-choice-machine
  (register pc :width 16)
  (register r :width 16 :count 8)
  (register observed :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field src 6)
    (field dst 5)
    (field opcode 5)
    (extra-word-order src dst)))

(defmode independent-reg expr)
(defmode independent-index "[" expr "," expr "]")
(defmode independent-literal "#" expr)
(defmode independent-pair
  (one-of independent-reg independent-index independent-literal) ","
  (one-of independent-reg independent-index independent-literal))

(definstruction independent-choice-machine move
  (modes independent-pair)
  (encoding
    (opcode 1)
    (operand dst :field dst
      (variant (choice independent-reg) inline :range (0 7))
      (variant (choice independent-index) inline :range (0 7) :bias 16)
      (variant (choice independent-literal) (extra-word :escape 31)))
    (operand src :field src
      (variant (choice independent-reg) inline :range (0 7))
      (variant (choice independent-index) inline :range (0 7) :bias 16)
      (variant (choice independent-literal) (extra-word :escape 31)))
    (for-choice (dst independent-index) (operand dst-off :trailing-word))
    (for-choice (src independent-index) (operand src-off :trailing-word)))
  (semantics
    (set! observed (+ (or dst-off 0) (or src-off 0)))
    (let ((value (choice-case src
                   (independent-reg (r src))
                   (independent-index (mref machine 'ram (+ (r src) src-off)))
                   (independent-literal src))))
      (choice-case dst
        (independent-reg (set! (r dst) value))
        (independent-index (setf (mref machine 'ram (+ (r dst) dst-off)) value))
        (independent-literal nil)))))

(fiveam:test independently-varying-operands-encode-decode-and-round-trip
  (dolist (case '(("move 0, 1" 0 1 () (0 1) (dst src))
                  ("move [0, 5], 1" 16 1 (5) (0 5 1) (dst dst-off src))
                  ("move 0, [1, 6]" 0 17 (6) (0 1 6) (dst src src-off))
                  ("move [0, 5], [1, 6]" 16 17 (6 5) (0 5 1 6) (dst dst-off src src-off))
                  ("move #100, [1, 55]" 31 17 (55 100) (100 1 55) (dst src src-off))
                  ("move [0, 5], #1000" 16 31 (1000 5) (0 5 1000) (dst dst-off src))))
    (destructuring-bind (source dst src extras values names) case
      (let* ((cells (assembly-cells (assemble source :machine 'independent-choice-machine)))
             (expected (coerce (cons (logior 1 (ash dst 5) (ash src 10)) extras) 'vector)))
        (fiveam:is (equalp expected cells))
        (multiple-value-bind (descriptor decoded size choices)
            (decode-instruction-at (lambda (address) (aref cells address)) 0 'independent-choice-machine)
          (fiveam:is (equal values decoded))
          (fiveam:is (equal names (instruction-descriptor-operand-names descriptor)))
          (fiveam:is (= (length expected) size))
          (fiveam:is (equal (mapcar #'mode-descriptor-name
                                   (nth-value 2 (match-operand-mode
                                                 (statement-operand-tokens (first (parse source)))
                                                 (find-mode-descriptor 'independent-pair))))
                           (mapcar #'word-field-choice-choice choices))))
        (fiveam:is (equalp cells
                          (assembly-cells
                           (assemble (disassembly-text
                                      (disassemble-cells cells :machine 'independent-choice-machine)
                                      :origin 0)
                                     :machine 'independent-choice-machine))))))))

(fiveam:test independent-operands-execute-with-own-offsets
  (dolist (case '(("move 0, 1" 0 0 777)
                  ("move [0, 5], 1" 105 5 777)
                  ("move 0, [1, 6]" 0 6 999)
                  ("move [0, 5], [1, 6]" 105 11 999)))
    (destructuring-bind (source address observed value) case
      (let ((machine (make-machine 'independent-choice-machine)))
        (setf (regref machine 'r 0) 100
              (regref machine 'r 1) 777
              (mref machine 'ram 783) 999)
        (load-program machine (assembly-cells (assemble source :machine 'independent-choice-machine)))
        (step-machine machine)
        (fiveam:is (= observed (sref machine 'observed)))
        (fiveam:is (= value (if (zerop address) (regref machine 'r 0)
                               (mref machine 'ram address))))))))

(fiveam:test for-choice-ownership-validation
  (let* ((mode (find-mode-descriptor 'independent-pair))
         (operands '((operand dst :field dst) (operand src :field src)))
         (left '(for-choice (dst independent-index) (operand dst-off :trailing-word)))
         (right '(for-choice (src independent-index) (operand src-off :trailing-word))))
    (dolist (bad (list (list left) (list left right left)
                      (list left right '(for-choice independent-index (operand off :trailing-word)))
                      (list left '(for-choice (missing independent-index) (operand off :trailing-word)))
                      (list left '(for-choice (src independent-reg) (operand off :trailing-word)))
                      (list left '(for-choice (src independent-index)))
                      (list left '(for-choice (src independent-index) (operand x :trailing-word)
                                             (operand y :trailing-word)))))
      (fiveam:signals error (%parse-for-choice-subclauses mode bad operands)))
    (fiveam:finishes (%parse-for-choice-subclauses mode (list left right) operands))))

(defmode independent-triple
  (one-of independent-reg independent-index) ","
  (one-of independent-reg independent-index) ","
  (one-of independent-reg independent-index))

(fiveam:test three-varying-elements-have-eight-distinct-shapes
  (let ((tuples (%mode-hole-tuples (find-mode-descriptor 'independent-triple))))
    (fiveam:is (= 8 (length tuples)))
    (fiveam:is (equal '(3 4 4 5 4 5 5 6)
                      (mapcar (lambda (tuple) (length (mode-hole-tuple-hole-alternatives tuple))) tuples)))))

(fiveam:test trailing-words-follow-nearest-field-and-keep-local-order
  (let* ((descriptor (first (find-instruction-variants 'independent-choice-machine 'move)))
         (dst (make-word-field-choice :kind :extra-word :width 5 :shift 5))
         (src (make-word-field-choice :kind :inline :width 6 :shift 10))
         (extra (make-word-field-choice :kind :trailing-word)))
    (fiveam:is (equal '(3 4 5 0 1 2)
                      (%word-emit-order descriptor (list dst extra extra src extra extra))))
    (fiveam:is (equal '(1 2 0)
                      (%word-emit-order descriptor (list extra src extra))))))

(defmode inline-primary "canonical" expr)
(defmode inline-alternate "alias" expr)
(defmode inline-another "another" expr)
(defmode inline-alias-mode (one-of inline-primary inline-alternate inline-another))

(definstruction independent-choice-machine aliases
  (modes inline-alias-mode)
  (encoding
    (opcode 2)
    (operand value :field src
      (variant (choice inline-alternate) inline :range (0 7) :bias 8 :alias t)
      (variant (choice inline-primary) inline :range (0 7) :bias 8)
      (variant (choice inline-another) inline :range (0 7) :bias 8 :alias t)))
  (semantics
    (choice-case value
      (inline-primary (set! observed value))
      (inline-alternate (set! observed 999))
      (inline-another (set! observed 888)))))

(fiveam:test inline-aliases-use-canonical-decoding-and-semantics
  (dolist (source '("aliases canonical 5" "aliases alias 5" "aliases another 5"))
    (let* ((cells (assembly-cells (assemble source :machine 'independent-choice-machine)))
           (machine (make-machine 'independent-choice-machine))
           (lines (disassemble-cells cells :machine 'independent-choice-machine)))
      (fiveam:is (equalp (vector (logior 2 (ash 13 10))) cells))
      (fiveam:is (search "canonical$5" (disassembly-line-text (first lines))))
      (fiveam:is (equalp cells (assembly-cells (assemble (disassembly-text lines :origin 0)
                                                       :machine 'independent-choice-machine))))
      (load-program machine cells)
      (step-machine machine)
      (fiveam:is (= 5 (sref machine 'observed))))))

(defmode inline-signed-primary "canonical" expr :signed t)
(defmode inline-signed-alternate "alias" expr :signed t)
(defmode inline-signed-mode (one-of inline-signed-primary inline-signed-alternate))

(definstruction independent-choice-machine salias
  (modes inline-signed-mode)
  (encoding (opcode 3)
    (operand value :field src
      (variant (choice inline-signed-primary) inline :range (-3 3) :bias 1)
      (variant (choice inline-signed-alternate) inline :range (-3 3) :bias 1 :alias t)))
  (semantics (set! observed value)))

(fiveam:test signed-inline-aliases-round-trip-across-zero
  (dolist (value '(-3 -1 0 3))
    (let ((cells (assembly-cells (assemble (format nil "salias alias ~D" value)
                                         :machine 'independent-choice-machine))))
      (fiveam:is (equalp cells (assembly-cells (assemble (format nil "salias canonical ~D" value)
                                                       :machine 'independent-choice-machine))))
      (multiple-value-bind (descriptor values size choices)
          (decode-instruction-at (lambda (address) (aref cells address)) 0 'independent-choice-machine)
        (declare (ignore descriptor size))
        (fiveam:is (equal (list value) values))
        (fiveam:is (eq 'inline-signed-primary (word-field-choice-choice (first choices))))))))

(fiveam:test inline-aliases-reject-incompatible-or-ambiguous-encodings
  (dolist (forms '(((variant (choice inline-primary) inline :range (0 7))
                    (variant (choice inline-alternate) inline :range (1 7) :alias t))
                   ((variant (choice inline-primary) inline :range (0 7))
                    (variant (choice inline-alternate) inline :range (0 7) :bias 1 :alias t))
                   ((variant (choice inline-primary) inline :range (0 7))
                    (variant (choice inline-alternate) inline :range (0 7)))
                   ((variant (choice inline-primary) inline :range (0 7))
                    (variant (choice inline-alternate) inline :range (0 7))
                    (variant (choice inline-another) inline :range (0 7) :alias t))
                   ((variant (choice inline-primary) inline :range (0 7) :alias t))
                   ((variant (range 0 7) inline :alias t))))
    (fiveam:signals error
      (%check-word-variants (mapcar (lambda (form) (%parse-word-variant-form form 'src)) forms)
                            6 'src nil)))
  (fiveam:signals error
    (%check-word-variants
     (list (%parse-word-variant-form '(variant (choice inline-primary) inline :range (0 7)) 'src)
           (%parse-word-variant-form '(variant (choice inline-signed-alternate) inline :range (0 7) :alias t) 'src))
     6 'src nil)))

(defmode indexed-alias "indexed" expr "," expr)
(defmode indexed-alias-mode (one-of independent-index indexed-alias independent-reg))

(definstruction independent-choice-machine ialias
  (modes indexed-alias-mode)
  (encoding (opcode 4)
    (operand value :field src
      (variant (choice independent-reg) inline :range (0 7))
      (variant (choice indexed-alias) inline :range (0 7) :bias 16 :alias t)
      (variant (choice independent-index) inline :range (0 7) :bias 16))
    (for-choice independent-index (operand offset :trailing-word))
    (for-choice indexed-alias (operand alternate-offset :trailing-word)))
  (semantics
    (choice-case value
      (independent-reg (set! observed value))
      (independent-index (set! observed offset))
      (indexed-alias (set! observed alternate-offset)))))

(fiveam:test inline-alias-of-a-longer-alternative-decodes-canonical-shape
  (let* ((cells (assembly-cells (assemble "ialias indexed 2, 123" :machine 'independent-choice-machine)))
         (machine (make-machine 'independent-choice-machine)))
    (fiveam:is (equalp cells (assembly-cells (assemble "ialias [2, 123]" :machine 'independent-choice-machine))))
    (let ((lines (disassemble-cells cells :machine 'independent-choice-machine)))
      (fiveam:is (search "[$2,$7B]" (disassembly-line-text (first lines))))
      (fiveam:is (equalp cells (assembly-cells (assemble (disassembly-text lines :origin 0)
                                                       :machine 'independent-choice-machine)))))
    (load-program machine cells)
    (step-machine machine)
    (fiveam:is (= 123 (sref machine 'observed)))))

(fiveam:test trailing-words-retain-hole-order-without-an-order-clause
  (let ((descriptor (first (find-instruction-variants 'varying-hole-test-machine 'ldv)))
        (extra (make-word-field-choice :kind :trailing-word))
        (src (make-word-field-choice :kind :inline :width 6 :shift 10))
        (dst (make-word-field-choice :kind :inline :width 5 :shift 5)))
    (fiveam:is (equal '(0 1 2 3) (%word-emit-order descriptor (list dst extra src extra))))))

(defmode inline-width "wide" expr :width 2)
(defmode inline-relative "relative" expr :relative t)

(fiveam:test inline-aliases-require-compatible-mode-attributes
  (dolist (pair '((inline-primary inline-width)
                  (inline-signed-primary inline-relative)
                  (inline-primary independent-index)))
    (fiveam:signals error
      (%check-word-variants
       (list (make-word-variant :kind :inline :range '(0 . 7) :choice (first pair))
             (make-word-variant :kind :inline :range '(0 . 7) :choice (second pair) :alias t))
       6 'src nil))))

(defmachine extra-field-machine
  (register pc :width 16)
  (memory ram :width 16 :addr-width 16 :cell-width 16)
  (instruction-word :width 16
    (field src 6) (field dst 5) (field extra 3) (field opcode 2)))

(definstruction extra-field-machine move
  (modes independent-pair)
  (encoding (opcode 1)
    (operand dst :field dst
      (variant (choice independent-reg) inline :range (0 7))
      (variant (choice independent-index) inline :range (0 7) :bias 16)
      (variant (choice independent-literal) (extra-word :escape 31)))
    (operand src :field src
      (variant (choice independent-reg) inline :range (0 7))
      (variant (choice independent-index) inline :range (0 7) :bias 16)
      (variant (choice independent-literal) (extra-word :escape 31)))
    (for-choice (dst independent-index) (operand dst-off :field extra))
    (for-choice (src independent-index) (operand src-off :trailing-word)))
  (semantics nil))

(fiveam:test extra-field-and-trailing-hole-retain-their-own-choices
  (let* ((cells (assembly-cells (assemble "move [0, 5], [1, 6]" :machine 'extra-field-machine)))
         (lines (disassemble-cells cells :machine 'extra-field-machine)))
    (fiveam:is (equalp (vector (logior 1 (ash 5 2) (ash 16 5) (ash 17 10)) 6) cells))
    (fiveam:is (equalp cells (assembly-cells (assemble (disassembly-text lines :origin 0)
                                                     :machine 'extra-field-machine))))))

(fiveam:test registration-checks-each-shape-pair-once
  (let ((original (symbol-function '%check-opcode-decodable!))
        (checks 0)
        (variants (find-instruction-variants 'independent-choice-machine 'move)))
    (unwind-protect
         (progn
           (setf (symbol-function '%check-opcode-decodable!)
                 (lambda (&rest args)
                   (incf checks)
                   (apply original args)))
           (register-instruction-variants! 'independent-choice-machine variants)
           (fiveam:is (= 6 checks))
           (fiveam:is (= 9 (length (find-instruction-variants 'independent-choice-machine 'move)))))
      (setf (symbol-function '%check-opcode-decodable!) original))))

(fiveam:test aliases-cannot-change-the-size-of-extra-holes
  (fiveam:signals error
    (eval '(definstruction independent-choice-machine invalid
             (modes indexed-alias-mode)
             (encoding (opcode 5)
               (operand value :field src
                 (variant (choice independent-reg) inline :range (0 7))
                 (variant (choice independent-index) inline :range (0 7) :bias 16)
                 (variant (choice indexed-alias) inline :range (0 7) :bias 16 :alias t))
               (for-choice independent-index (operand off :trailing-word :cells 1))
               (for-choice indexed-alias (operand off :trailing-word :cells 2)))
             (semantics nil)))))
