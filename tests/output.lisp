;;;; tests/output.lisp
;;;; fiveam tests for output.lisp (#79): raw binary and Intel HEX output.
;;;; Reuses DISASM-TEST-MACHINE and DISASM-WORD-MACHINE (tests/disassembler.lisp).

(in-package #:lasm)

(fiveam:def-suite output :in lasm)
(fiveam:in-suite output)

(defun %cells-assembly (cell-width origin &rest values)
  (make-assembly :cells (make-array (length values) :element-type `(unsigned-byte ,cell-width)
                                                    :initial-contents values)
                 :cell-width cell-width :origin origin))

(defun %hex-record-bytes (line)
  (loop for i from 1 below (length line) by 2
        collect (parse-integer line :start i :end (+ i 2) :radix 16)))

(defun %hex-lines (text)
  (loop with start = 0
        for nl = (position #\Newline text :start start)
        while nl collect (subseq text start nl) do (setf start (1+ nl))))

;;; assembly-bytes

(fiveam:test assembly-bytes-8-bit-cells-are-the-cells
  (let ((a (assemble "ldx #10
hlt" :machine 'disasm-test-machine)))
    (fiveam:is (equalp #(#xA2 10 0) (assembly-bytes a)))))

(fiveam:test assembly-bytes-splits-wide-cells-by-machine-endian
  (let ((a (%cells-assembly 16 0 #x1234 #xABCD)))
    (fiveam:is (equalp #(#x34 #x12 #xCD #xAB) (assembly-bytes a :machine 'disasm-word-machine)))
    (fiveam:is (equalp #(#x12 #x34 #xAB #xCD) (assembly-bytes a :endian :big)))))

(fiveam:test assembly-bytes-assembled-word-program-uses-little-endian
  (let ((a (assemble "hlt" :machine 'disasm-word-machine)))
    (fiveam:is (= 2 (length (assembly-bytes a :machine 'disasm-word-machine))))))

(fiveam:test assembly-bytes-wide-cells-need-an-endian-source
  (fiveam:signals error (assembly-bytes (%cells-assembly 16 0 1))))

(fiveam:test assembly-bytes-rejects-non-byte-multiple-cell-width
  (fiveam:signals error (assembly-bytes (%cells-assembly 12 0 1) :endian :little))
  (fiveam:signals error (hex-text (%cells-assembly 12 0 1) :endian :little)))

;;; bytes-to-cells

(fiveam:test bytes-to-cells-inverts-assembly-bytes
  (dolist (endian '(:little :big))
    (let* ((a (%cells-assembly 16 0 #x1234 #xABCD #x0001))
           (cells (bytes-to-cells (assembly-bytes a :endian endian) 16 :endian endian)))
      (fiveam:is (equalp (assembly-cells a) cells))
      (fiveam:is (equal '(unsigned-byte 16) (array-element-type cells))))))

(fiveam:test bytes-to-cells-rejects-a-partial-cell
  (fiveam:signals error (bytes-to-cells #(1 2 3) 16)))

;;; write-binary

(fiveam:test write-binary-round-trips-through-a-file
  (uiop:with-temporary-file (:pathname path :type "bin")
    (let ((a (assemble "ldx #10
hlt" :machine 'disasm-test-machine)))
      (fiveam:is (equal path (write-binary a path)))
      (fiveam:is (equalp (assembly-bytes a)
                         (with-open-file (in path :element-type '(unsigned-byte 8))
                           (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                             (read-sequence v in)
                             v)))))))

;;; hex-text

(fiveam:test hex-text-one-data-record-and-eof
  (fiveam:is (string= (format nil ":03001000010203E7~%:00000001FF~%")
                      (hex-text (%cells-assembly 8 #x10 1 2 3)))))

(fiveam:test hex-text-empty-program-is-just-eof
  (fiveam:is (string= (format nil ":00000001FF~%")
                      (hex-text (%cells-assembly 8 0)))))

(fiveam:test hex-text-splits-records-at-16-bytes
  (let ((lines (%hex-lines (hex-text (apply #'%cells-assembly 8 0 (loop repeat 20 collect 1))))))
    (fiveam:is (= 3 (length lines)))
    (fiveam:is (string= ":10000000" (subseq (first lines) 0 9)))
    (fiveam:is (string= ":04001000" (subseq (second lines) 0 9)))))

(fiveam:test hex-text-every-record-checksums-to-zero
  (dolist (line (%hex-lines (hex-text (apply #'%cells-assembly 8 #xFFF0 (loop for i below 40 collect i)))))
    (fiveam:is (zerop (mod (reduce #'+ (%hex-record-bytes line)) 256)))))

(fiveam:test hex-text-emits-extended-address-past-64k
  (fiveam:is (string= (format nil ":020000040001F9~%:01000000AA55~%:00000001FF~%")
                      (hex-text (%cells-assembly 8 #x10000 #xAA)))))

(fiveam:test hex-text-record-never-crosses-a-64k-boundary
  (let ((lines (%hex-lines (hex-text (%cells-assembly 8 #xFFFE 1 2 3 4)))))
    (fiveam:is (equal '(":02FFFE000102FE" ":020000040001F9" ":020000000304F7" ":00000001FF")
                      lines))))

(fiveam:test hex-text-word-machine-addresses-count-bytes
  (let ((lines (%hex-lines (hex-text (%cells-assembly 16 2 #x1234) :machine 'disasm-word-machine))))
    (fiveam:is (string= ":02000400" (subseq (first lines) 0 9)))
    (fiveam:is (string= "3412" (subseq (first lines) 9 13)))))

(fiveam:test hex-text-rejects-past-32-bit-range
  (fiveam:signals error (hex-text (%cells-assembly 8 (1- (ash 1 32)) 1 2))))

(fiveam:test hex-text-writes-to-stream-and-returns-nil
  (let ((a (%cells-assembly 8 0 1)))
    (fiveam:is (string= (hex-text a)
                        (with-output-to-string (s)
                          (fiveam:is (null (hex-text a :stream s))))))))

(fiveam:test write-intel-hex-writes-hex-text
  (uiop:with-temporary-file (:pathname path :type "hex")
    (let ((a (%cells-assembly 8 0 1 2)))
      (write-intel-hex a path)
      (fiveam:is (string= (hex-text a) (uiop:read-file-string path))))))

(defmachine mixed-endian-output-test-machine
  (register a :width 8)
  (memory ram :width 16 :addr-width 8 :endian (:big :little 2)))

(fiveam:test assembly-bytes-mixed-endian-machine-orders-bytes-by-inner-order
  (let ((a (%cells-assembly 16 0 #x1234)))
    (fiveam:is (equalp #(#x34 #x12)
                       (assembly-bytes a :machine 'mixed-endian-output-test-machine)))))
