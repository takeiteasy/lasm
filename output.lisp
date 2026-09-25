;;;; output.lisp
;;;; #79 (M7): serializes an ASSEMBLY's cells to a standalone file -- raw
;;;; bytes or Intel HEX -- for tools outside the Lisp image. A cell wider than
;;;; 8 bits splits into bytes ordered by the machine's :endian (#66), the same
;;;; convention %ENCODE-VALUE-CELLS and %FETCH-CELLS use. #174: a width that is
;;;; not a multiple of 8 is written either padded to whole bytes per cell
;;;; (:PAD) or as one continuous bitstream (:BITS).

(in-package #:lasm)

(defun %output-packing (packing)
  (unless (member packing '(:pad :bits))
    (%output-usage-error ":PACKING must be :PAD or :BITS, got ~S" packing))
  packing)

(defun %output-cell-bytes (cell-width)
  (ceiling cell-width 8))

(defun %output-endian (cell-width packing machine memory endian)
  (cond ((if (eq packing :bits) (= cell-width 8) (<= cell-width 8)) :little)
        (endian (%check-endian endian 'output))
        (machine (%endian-byte-order (%machine-endian machine memory)))
        (t (%output-usage-error "a ~D-bit cell needs :MACHINE or :ENDIAN to order its ~A"
                  cell-width (if (eq packing :bits) "bits" "bytes")))))

(defun %stream-bit-position (k width endian)
  "The (VALUES CELL-INDEX CELL-BIT BYTE-INDEX BYTE-BIT) of bit K of the packed stream."
  (multiple-value-bind (cell offset) (floor k width)
    (multiple-value-bind (byte bit) (floor k 8)
      (if (eq endian :big)
          (values cell (- width 1 offset) byte (- 7 bit))
          (values cell offset byte bit)))))

(defun %pack-bits (cells width endian)
  (let ((bytes (make-array (ceiling (* width (length cells)) 8) :element-type '(unsigned-byte 8)
                                                                :initial-element 0)))
    (dotimes (k (* width (length cells)) bytes)
      (multiple-value-bind (cell cell-bit byte byte-bit) (%stream-bit-position k width endian)
        (when (logbitp cell-bit (aref cells cell))
          (setf (aref bytes byte) (logior (aref bytes byte) (ash 1 byte-bit))))))))

(defun %unpack-bits (bytes width endian)
  (let* ((total (* 8 (length bytes)))
         (count (floor total width)))
    (when (>= (- total (* count width)) 8)
      (%output-usage-error "~D bytes is not a whole number of ~D-bit cells" (length bytes) width))
    (let ((cells (make-array count :element-type `(unsigned-byte ,width) :initial-element 0)))
      (dotimes (k (* count width) cells)
        (multiple-value-bind (cell cell-bit byte byte-bit) (%stream-bit-position k width endian)
          (when (logbitp byte-bit (elt bytes byte))
            (setf (aref cells cell) (logior (aref cells cell) (ash 1 cell-bit)))))))))

(defun %bank-image-region (assembly region)
  "REGION, or the one banked region ASSEMBLY has output in when REGION is NIL."
  (or region
      (let ((regions (remove-duplicates (mapcar #'bank-image-region (assembly-banks assembly)))))
        (when (rest regions)
          (%output-usage-error "assembly has output in several banked regions (~{~(~A~)~^, ~}): pass :REGION"
                 regions))
        (first regions))))

(defun %assembly-output-cells (assembly bank region)
  "The cells to write out for ASSEMBLY: BANK of REGION alone when BANK is
given, else the physical layout -- the main image followed, region by region,
by every bank from 0 to the highest used, each padded to the region's size."
  (let ((main (or (assembly-cells assembly) #()))
        (width (assembly-cell-width assembly))
        (images (assembly-banks assembly)))
    (if bank
        (let ((image (assembly-bank-image assembly (%bank-image-region assembly region) bank)))
          (if image (bank-image-cells image) #()))
        (let ((parts (list main)))
          (dolist (name (remove-duplicates (mapcar #'bank-image-region images)))
            (let* ((region-images (remove-if-not (lambda (image) (eq (bank-image-region image) name)) images))
                   (size (length (bank-image-cells (first region-images)))))
              (dotimes (b (1+ (reduce #'max region-images :key #'bank-image-bank)))
                (let ((image (find b region-images :key #'bank-image-bank)))
                  (cl:push (if image
                               (bank-image-cells image)
                               (make-array size :element-type `(unsigned-byte ,width)
                                                :initial-element 0))
                           parts)))))
          (apply #'concatenate `(vector (unsigned-byte ,width)) (nreverse parts))))))

(defun assembly-bytes (assembly &key machine memory endian bank region (packing :pad))
  "ASSEMBLY's cells as a (vector (unsigned-byte 8)). A cell wider than 8 bits
becomes CEILING(CELL-WIDTH/8) bytes, low byte first when ENDIAN is :LITTLE.
ENDIAN defaults to MACHINE's (see %MACHINE-ENDIAN; MEMORY selects the memory
element) and is not needed for cells of 8 bits or fewer.

PACKING is :PAD (default), each cell zero-extended to whole bytes, or :BITS,
the cells laid end to end as one bitstream -- high bit first when ENDIAN is
:BIG, low bit first when :LITTLE -- with the last byte zero-padded. The two are
the same when CELL-WIDTH is a multiple of 8.

Without BANK the bytes are the physical layout: the main image, then each
banked region's banks from 0 to the highest one used, in bank order, every
bank padded to the region's full size. With BANK, only that bank's image of
REGION (which may be omitted when ASSEMBLY has output in one banked region)
is returned, also padded to the region's size; empty if nothing was placed
there."
  (let* ((width (assembly-cell-width assembly))
         (packing (%output-packing packing))
         (endian (%output-endian width packing machine memory endian))
         (cells (%assembly-output-cells assembly bank region)))
    (if (eq packing :bits)
        (%pack-bits cells width endian)
        (let* ((n (%output-cell-bytes width))
               (bytes (make-array (* n (length cells)) :element-type '(unsigned-byte 8))))
          (loop for cell across cells
                for base from 0 by n
                do (dotimes (i n)
                     (setf (aref bytes (+ base i))
                           (ldb (byte 8 (* 8 (if (eq endian :big) (- n 1 i) i))) cell))))
          bytes))))

(defun bytes-to-cells (bytes cell-width &key (endian :little) (packing :pad))
  "Inverse of ASSEMBLY-BYTES: BYTES (a sequence of octets) regrouped into a
(vector (unsigned-byte CELL-WIDTH)). Signals when the byte count is not a whole
number of cells, or under :PAD when a cell's value does not fit CELL-WIDTH
bits. Under :BITS, up to 7 trailing bits are padding; for a CELL-WIDTH below 8
that padding can decode as extra zero cells."
  (let ((endian (%check-endian endian 'output)))
    (if (eq (%output-packing packing) :bits)
        (%unpack-bits bytes cell-width endian)
        (let ((n (%output-cell-bytes cell-width)))
          (unless (zerop (mod (length bytes) n))
            (%output-usage-error "~D bytes is not a whole number of ~D-bit cells" (length bytes) cell-width))
          (let ((cells (make-array (floor (length bytes) n) :element-type `(unsigned-byte ,cell-width))))
            (dotimes (c (length cells) cells)
              (let ((v 0))
                (dotimes (i n)
                  (setf v (logior v (ash (elt bytes (+ (* c n) i))
                                         (* 8 (if (eq endian :big) (- n 1 i) i))))))
                (unless (< v (ash 1 cell-width))
                  (%output-usage-error "cell ~D value ~D does not fit ~D bits" c v cell-width))
                (setf (aref cells c) v))))))))

(defun write-binary (assembly path &key machine memory endian bank region (packing :pad))
  "Write ASSEMBLY-BYTES to PATH as a raw binary file, replacing any existing
file. Returns PATH."
  (let ((bytes (assembly-bytes assembly :machine machine :memory memory :endian endian
                                         :bank bank :region region :packing packing)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    path))

(defconstant +hex-record-bytes+ 16)

(defun %bit-origin-byte (origin width)
  (multiple-value-bind (byte rest) (floor (* origin width) 8)
    (unless (zerop rest)
      (%output-usage-error "origin ~D of ~D-bit cells is not on a byte boundary" origin width))
    byte))

(defun %hex-record (stream type address data)
  (let ((sum (+ (length data) (ldb (byte 8 8) address) (ldb (byte 8 0) address) type)))
    (format stream ":~2,'0X~4,'0X~2,'0X" (length data) address type)
    (loop for b across data
          do (format stream "~2,'0X" b)
             (incf sum b))
    (format stream "~2,'0X~%" (ldb (byte 8 0) (- sum)))))

(defun hex-text (assembly &key stream machine memory endian bank region (packing :pad))
  "Render ASSEMBLY as Intel HEX: 16-byte data records, an extended linear
address record wherever the upper 16 address bits change, and an end-of-file
record. Addresses count bytes from ASSEMBLY-ORIGIN scaled by the cell size, so
on a wider-than-8-bit cell machine they are not the same numbers as its
labels. Under :PACKING :BITS the start is ORIGIN*CELL-WIDTH/8 bytes and
signals when that is not a whole byte. With BANK the records start at that
bank's region address. Keys are ASSEMBLY-BYTES'. Returns the text as a string when STREAM is
NIL (default); otherwise writes to STREAM and returns NIL."
  (let* ((width (assembly-cell-width assembly))
         (bytes (assembly-bytes assembly :machine machine :memory memory :endian endian
                                         :bank bank :region region :packing packing))
         (image (and bank (assembly-bank-image assembly (%bank-image-region assembly region) bank)))
         (origin (if image (bank-image-origin image) (assembly-origin assembly)))
         (start (if (eq packing :bits)
                    (%bit-origin-byte origin width)
                    (* (%output-cell-bytes width) origin)))
         (body (with-output-to-string (s)
                 (unless (< (+ start (length bytes)) (ash 1 32))
                   (%output-usage-error "program ends at byte address ~D, past the 32-bit Intel HEX range"
                          (+ start (length bytes))))
                 (let ((upper 0) (i 0))
                   (loop while (< i (length bytes))
                         do (let* ((address (+ start i))
                                   (hi (ldb (byte 16 16) address))
                                   (low (ldb (byte 16 0) address))
                                   (count (min +hex-record-bytes+ (- (length bytes) i)
                                               (- #x10000 low))))
                              (when (/= hi upper)
                                (setf upper hi)
                                (%hex-record s 4 0 (vector (ldb (byte 8 8) hi) (ldb (byte 8 0) hi))))
                              (%hex-record s 0 low (subseq bytes i (+ i count)))
                              (incf i count))))
                 (%hex-record s 1 0 #()))))
    (if stream (progn (write-string body stream) nil) body)))

(defun write-intel-hex (assembly path &key machine memory endian bank region (packing :pad))
  "Write HEX-TEXT of ASSEMBLY to PATH, replacing any existing file. Returns
PATH."
  (let ((text (hex-text assembly :machine machine :memory memory :endian endian
                                  :bank bank :region region :packing packing)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    path))
