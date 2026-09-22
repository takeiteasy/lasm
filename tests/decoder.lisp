(in-package #:lasm)

(fiveam:def-suite decoder :in lasm)
(fiveam:in-suite decoder)

(defmachine decode-cache-machine
  (register pc :width 8)
  (register a :width 8)
  (memory ram :width 8 :addr-width 8)
  (instruction-word :width 8 (field opcode 4) (field value 4)))

(defmode decode-cache-immediate expr)

(definstruction decode-cache-machine loadv
  (modes decode-cache-immediate)
  (encoding (opcode 1)
            (operand value :field value
              (variant (range 0 14) inline)
              (variant :else (extra-word :escape 15))))
  (semantics (set! a value)))

(definstruction decode-cache-machine stop
  (encoding (opcode 2) (field-value value 0))
  (semantics (trap :halt)))

(defmode decode-map-value "mapped" expr)
(defmode decode-other-value "other" expr)
(defmode decode-map-mode (one-of decode-map-value decode-other-value))

(definstruction decode-cache-machine mapped
  (modes decode-map-mode)
  (encoding (opcode 3)
            (operand operand-map :field value
              (variant (choice decode-map-value) inline :range (0 7))
              (variant (choice decode-other-value) inline :range (0 7) :bias 8)))
  (semantics (choice-case operand-map
               (decode-map-value (set! a operand-map))
               (decode-other-value (set! a (+ operand-map 10))))))

(fiveam:test operand-mapping-does-not-capture-an-operand-name
  (let ((machine (make-machine 'decode-cache-machine)))
    (load-program machine #(#x33 #x3c))
    (step-machine machine)
    (fiveam:is (= 3 (sref machine 'a)))
    (step-machine machine)
    (fiveam:is (= 14 (sref machine 'a)))))

(defun scan-decode-word (reader address machine-name)
  (let* ((machine (find-machine-descriptor machine-name))
         (layout (machine-descriptor-instruction-word machine))
         (width-cells (instruction-word-layout-width-cells layout))
         (cell-width (instruction-word-layout-cell-width layout))
         (endian (instruction-word-layout-endian layout))
         (word (%fetch-cells reader address width-cells cell-width endian)))
    (destructuring-bind (name width shift) (instruction-word-field layout 'opcode)
      (declare (ignore name))
      (dolist (descriptor (gethash (ldb (byte width shift) word)
                                   (machine-descriptor-opcodes machine))
                         (values :decode-failure nil nil))
        (multiple-value-bind (values size choices okp)
            (%try-decode-word-candidate reader address width-cells cell-width descriptor word endian)
          (when okp
            (return (values descriptor values size choices
                            (instruction-descriptor-choice-selections descriptor)))))))))

(fiveam:test indexed-decode-agrees-with-candidate-scan-for-every-word
  (dolist (name '(disasm-word-machine independent-choice-machine))
    (let ((cells (make-array 8 :initial-element 123))
          (mismatch nil))
      (dotimes (word 65536)
        (setf (aref cells 0) word)
        (let ((reader (vector-cell-reader cells)))
          (unless (equal (multiple-value-list (scan-decode-word reader 0 name))
                         (multiple-value-list (decode-instruction-at reader 0 name)))
            (setf mismatch word)
            (return))))
      (fiveam:is (null mismatch) "Decode differs for ~S at word ~S" name mismatch))))

(fiveam:test dispatch-build-does-not-read-memory
  (let ((descriptor (find-machine-descriptor 'decode-cache-machine)))
    (setf (machine-descriptor-word-decode-table descriptor) nil)
    (dotimes (pass 2)
      (let ((reads nil))
        (multiple-value-bind (instruction values size)
            (decode-instruction-at (lambda (address)
                                     (cl:push address reads)
                                     (ecase address (0 #x1f) (1 42)))
                                   0 'decode-cache-machine)
          (fiveam:is (string= "LOADV" (instruction-descriptor-name instruction)))
          (fiveam:is (equal '(42) values))
          (fiveam:is (= 2 size))
          (fiveam:is (equal '(0 1) (reverse reads))))))
    (let ((reads nil))
      (fiveam:is (eq :decode-failure
                     (decode-instruction-at (lambda (address)
                                              (cl:push address reads) #x21)
                                            0 'decode-cache-machine)))
      (fiveam:is (equal '(0) reads)))
    (fiveam:signals address-out-of-range
      (decode-instruction-at (vector-cell-reader #(#x1f)) 0 'decode-cache-machine))))

(fiveam:test nested-decode-does-not-overwrite-operands
  (multiple-value-bind (descriptor values size)
      (decode-instruction-at
       (lambda (address)
         (if (zerop address) #x1f
             (progn
               (fiveam:is (equal '(3)
                                (nth-value 1 (decode-instruction-at
                                              (vector-cell-reader #(#x13))
                                              0 'decode-cache-machine))))
               42)))
       0 'decode-cache-machine)
    (declare (ignore descriptor))
    (fiveam:is (equal '(42) values))
    (fiveam:is (= 2 size))))

(fiveam:test dispatch-uses-only-declared-word-bits
  (fiveam:is (equal '(3) (nth-value 1 (decode-instruction-at
                                     (vector-cell-reader #(#x113))
                                     0 'decode-cache-machine)))))

(fiveam:test failed-registration-keeps-the-existing-dispatch
  (decode-instruction-at (vector-cell-reader #(#x13)) 0 'decode-cache-machine)
  (let* ((machine (find-machine-descriptor 'decode-cache-machine))
         (table (machine-descriptor-word-decode-table machine)))
    (fiveam:signals opcode-conflict
      (eval '(definstruction decode-cache-machine collision
               (encoding (opcode 1)) (semantics nil))))
    (fiveam:is (eq table (machine-descriptor-word-decode-table machine)))
    (fiveam:is (equal '(3) (nth-value 1 (decode-instruction-at
                                       (vector-cell-reader #(#x13))
                                       0 'decode-cache-machine))))))

#+sb-thread
(fiveam:test simultaneous-decodes-share-no-operand-scratch
  (setf (machine-descriptor-word-decode-table (find-machine-descriptor 'decode-cache-machine)) nil)
  (let ((threads
          (loop for value below 8
                collect (let ((expected value))
                          (sb-thread:make-thread
                           (lambda ()
                             (loop repeat 100
                                   always (equal (list expected)
                                                 (nth-value 1
                                                            (decode-instruction-at
                                                             (vector-cell-reader (vector #x1f expected))
                                                             0 'decode-cache-machine))))))))))
    (fiveam:is (every #'identity (mapcar #'sb-thread:join-thread threads)))))

(fiveam:test shared-dispatch-observes-code-and-trailing-word-writes
  (let ((first (make-machine 'decode-cache-machine))
        (second (make-machine 'decode-cache-machine)))
    (load-program first #(#x1f 42))
    (load-program second #(#x13))
    (step-machine first)
    (step-machine second)
    (fiveam:is (= 42 (sref first 'a)))
    (fiveam:is (= 3 (sref second 'a)))
    (let ((table (machine-descriptor-word-decode-table (machine-descriptor first))))
      (setf (mref first 'ram 1) 99 (sref first 'pc) 0)
      (step-machine first)
      (fiveam:is (= 99 (sref first 'a)))
      (setf (mref first 'ram 0) #x15 (sref first 'pc) 0)
      (step-machine first)
      (fiveam:is (= 5 (sref first 'a)))
      (fiveam:is (eq table (machine-descriptor-word-decode-table (machine-descriptor second)))))))

(fiveam:test instruction-redefinition-invalidates-cached-success-and-failure
  (eval '(defmachine decode-redefine-machine
           (memory ram :width 8 :addr-width 8)
           (instruction-word :width 8 (field opcode 4) (field value 4))))
  (eval '(definstruction decode-redefine-machine op
           (encoding (opcode 1)) (semantics nil)))
  (let ((reader (vector-cell-reader #(#x10 #x20))))
    (fiveam:is (instruction-descriptor-p (decode-instruction-at reader 0 'decode-redefine-machine)))
    (fiveam:is (eq :decode-failure (decode-instruction-at reader 1 'decode-redefine-machine)))
    (eval '(definstruction decode-redefine-machine op
             (encoding (opcode 2)) (semantics nil)))
    (fiveam:is (eq :decode-failure (decode-instruction-at reader 0 'decode-redefine-machine)))
    (fiveam:is (instruction-descriptor-p (decode-instruction-at reader 1 'decode-redefine-machine)))
    (eval '(defmachine decode-redefine-machine
             (memory ram :width 8 :addr-width 8)
             (instruction-word :width 8 (field opcode 4) (field value 4))))
    (fiveam:is (eq :decode-failure (decode-instruction-at reader 1 'decode-redefine-machine)))))

(defmachine decode-wide-machine
  (memory ram :width 8 :addr-width 8 :endian :big)
  (instruction-word :width 24 (field opcode 8) (field value 16)))

(definstruction decode-wide-machine wide
  (modes decode-cache-immediate)
  (encoding (opcode 1) (operand value :field value))
  (semantics nil))

(fiveam:test wider-words-use-bounded-fallback
  (multiple-value-bind (descriptor values size)
      (decode-instruction-at (vector-cell-reader #(#x01 #x12 #x34)) 0 'decode-wide-machine)
    (fiveam:is (string= "WIDE" (instruction-descriptor-name descriptor)))
    (fiveam:is (equal '(#x1234) values))
    (fiveam:is (= 3 size))
    (fiveam:is (null (machine-descriptor-word-decode-table
                     (find-machine-descriptor 'decode-wide-machine))))))
