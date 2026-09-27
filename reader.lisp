;;;; reader.lisp
;;;; Restricted Eclector reader for untrusted s-expression files (snapshots,
;;;; item programs): symbols are never interned, reader macros never run, and
;;;; nesting and number sizes are bounded. The caller supplies the function
;;;; that signals its own condition type.

(in-package #:lasm)

;; Eclector reads digits in quadratic time: 200k digits take about 3 s.
(defconstant +reader-max-number-chars+ 20000
  "Longest numeric token the restricted reader accepts.")

(defconstant +reader-max-depth+ 1000
  "Deepest list nesting the restricted reader accepts.")

(defclass restricted-client ()
  ((depth :initform 0 :accessor client-depth)
   (fail :initarg :fail :reader client-fail)
   (bare :initarg :bare :reader client-bare)))

(defun %reader-fail (control &rest args)
  "Signal the running reader's failure condition."
  (apply (client-fail eclector.reader:*client*) control args))

(defmethod eclector.reader:interpret-symbol
    ((client restricted-client) stream package-indicator name internp)
  (declare (ignore stream internp))
  (if (or (null package-indicator)
          (and (eq package-indicator :current) (eq (client-bare client) :uninterned)))
      (make-symbol name)
      (let ((package (case package-indicator
                       ((:keyword :current) (find-package :keyword))
                       (t (find-package package-indicator)))))
        (unless package
          (%reader-fail "no package ~A" package-indicator))
        (multiple-value-bind (symbol status) (find-symbol name package)
          (unless status
            (%reader-fail "unknown symbol ~A in ~A" name (package-name package)))
          symbol))))

(defmethod eclector.reader:find-character ((client restricted-client) (designator string))
  (or (name-char designator)
      (call-next-method)))

(defun %float-token-exponent (token)
  "For a whole-token float with an exponent marker, the values sign, zero-mantissa-p,
marker and exponent (capped at 10^12); NIL for anything else."
  (let ((i 0) (end (length token)) (negative nil) (zero t) (digits 0))
    (flet ((run (pred)
             (loop while (and (< i end) (funcall pred (char token i)))
                   do (when (and (digit-char-p (char token i)) (char/= (char token i) #\0))
                        (setf zero nil))
                      (incf digits)
                      (incf i))))
      (when (and (< i end) (find (char token i) "+-"))
        (setf negative (char= (char token i) #\-))
        (incf i))
      (run #'digit-char-p)
      (when (and (< i end) (char= (char token i) #\.))
        (incf i)
        (run #'digit-char-p))
      (when (and (plusp digits) (< i end) (find (char token i) "esfdlESFDL"))
        (let ((marker (char token i)) (exponent-negative nil))
          (incf i)
          (when (and (< i end) (find (char token i) "+-"))
            (setf exponent-negative (char= (char token i) #\-))
            (incf i))
          (let ((start (or (position #\0 token :start i :test #'char/=) end)))
            (when (and (< i end) (every #'digit-char-p (subseq token i)))
              (let ((magnitude (if (> (- end start) 12)
                                   (expt 10 12)
                                   (if (= start end) 0 (parse-integer token :start start)))))
                (values negative zero marker
                        (if exponent-negative (- magnitude) magnitude))))))))))

(defun %signed-zero (marker negative)
  (let ((zero (coerce 0 (ecase (char-downcase marker)
                          (#\e *read-default-float-format*)
                          (#\s 'short-float)
                          (#\f 'single-float)
                          (#\d 'double-float)
                          (#\l 'long-float)))))
    (if negative (- zero) zero)))

(defmethod eclector.reader:interpret-token :around
    ((client restricted-client) stream token escape-ranges)
  (declare (ignore stream))
  (when (and (> (length token) +reader-max-number-chars+)
             (null escape-ranges)
             (find (char token 0) "+-.0123456789"))
    (%reader-fail "number longer than ~D characters" +reader-max-number-chars+))
  ;; Eclector computes 10^exponent eagerly, so 1d999999999 never returns.
  (multiple-value-bind (negative zero marker exponent)
      (if escape-ranges nil (%float-token-exponent token))
    (cond ((null marker) (call-next-method))
          ((or zero (< exponent (- (+ 324 (length token)))))
           (%signed-zero marker negative))
          ((> exponent (+ 324 (length token)))
           (%reader-fail "float exponent out of range"))
          (t (call-next-method)))))

(defmethod eclector.reader:read-common :around ((client restricted-client) stream eof-error-p eof-value)
  (when (> (incf (client-depth client)) +reader-max-depth+)
    (%reader-fail "nested deeper than ~D" +reader-max-depth+))
  (unwind-protect (call-next-method)
    (decf (client-depth client))))

;;; Position tracking (#362): a second client, mixing in Eclector's
;;; parse-result protocol, records where each cons or symbol started without
;;; changing what is read -- MAKE-EXPRESSION-RESULT returns its RESULT
;;; unchanged, so callers that don't ask for positions see plain data.

(defvar *reader-positions* nil
  "EQ hash table, form -> starting character offset, bound while reading
under READ-RESTRICTED-FORMS' :POSITIONS.")

(defclass restricted-position-client (restricted-client eclector.parse-result:parse-result-client) ())

(defmethod eclector.parse-result:make-expression-result
    ((client restricted-position-client) result children source)
  (declare (ignore children))
  (when (and *reader-positions* (or (consp result) (and (symbolp result) (not (keywordp result)))))
    (setf (gethash result *reader-positions*) (car source)))
  result)

(defvar *restricted-readtable* nil)

(defun %restrict-sharpsign (table)
  "TABLE with every #X dispatch but the ones the restricted reader needs
(numbers, characters, keywords) failing."
  (dotimes (code 128)
    (let ((char (code-char code)))
      (unless (or (digit-char-p char) (member char '(#\\ #\( #\:)))
        (eclector.readtable:set-dispatch-macro-character
         table #\# char
         (lambda (stream char parameter)
           (declare (ignore parameter))
           (%reader-fail "#~A is not allowed at ~D" char (file-position stream)))))))
  table)

(defun %restricted-readtable ()
  "The readtable for snapshots and item files: no #X dispatch beyond numbers/
characters/keywords, and ' ` , are rejected outright (#380: a source file's
readtable, %SOURCE-READTABLE, allows them instead)."
  (or *restricted-readtable*
      (let ((table (%restrict-sharpsign (eclector.readtable:copy-readtable eclector.readtable:*readtable*))))
        (dolist (char '(#\' #\` #\,))
          (eclector.readtable:set-macro-character
           table char
           (lambda (stream char)
             (%reader-fail "~A is not allowed at ~D" char (file-position stream)))))
        (setf *restricted-readtable* table))))

(defvar *source-readtable* nil)

(defun %source-readtable ()
  "The readtable for source files (#380): like %RESTRICTED-READTABLE, but
' ` , ,@ read as quote/quasiquote/unquote/unquote-splicing (WRAP-IN-QUOTE and
friends, specialized on RESTRICTED-SOURCE-CLIENT, build their lists headed by
an uninterned symbol instead of interning CL:QUOTE &c.)."
  (or *source-readtable*
      (setf *source-readtable*
            (%restrict-sharpsign (eclector.readtable:copy-readtable eclector.readtable:*readtable*)))))

;;; Quote/quasiquote for source files only (#380): RESTRICTED-SOURCE-CLIENT
;;; overrides Eclector's WRAP-IN-* so a quoted form's head is an uninterned
;;; symbol, matching every other symbol the restricted reader hands back.
;;; %SOURCE-READTABLE leaves ' ` , ,@ at their standard Eclector meaning, so
;;; only this client, not the readtable, decides how they're represented.

(defclass restricted-source-client (restricted-position-client) ())

(defun %quote-symbol (name)
  (make-symbol name))

(defmethod eclector.reader:wrap-in-quote ((client restricted-source-client) material)
  (list (%quote-symbol "QUOTE") material))

(defmethod eclector.reader:wrap-in-quasiquote ((client restricted-source-client) form)
  (list (%quote-symbol "QUASIQUOTE") form))

(defmethod eclector.reader:wrap-in-unquote ((client restricted-source-client) form)
  (list (%quote-symbol "UNQUOTE") form))

(defmethod eclector.reader:wrap-in-unquote-splicing ((client restricted-source-client) form)
  (list (%quote-symbol "UNQUOTE-SPLICING") form))

(defun %read-restricted (stream fail path bare positions quotes function)
  "Call FUNCTION with a thunk that reads the next form of STREAM, or returns
its second argument's unique end marker, under the restricted reader's
limits. POSITIONS, an EQ hash table or NIL, records each cons or symbol's
starting character offset. QUOTES allows ' ` , ,@ (#380), and requires
POSITIONS."
  (handler-case
      (with-standard-io-syntax
        (let* ((client-class (cond (quotes 'restricted-source-client) (positions 'restricted-position-client)
                                    (t 'restricted-client)))
               (eclector.reader:*client* (make-instance client-class :fail fail :bare bare))
               (eclector.readtable:*readtable* (if quotes (%source-readtable) (%restricted-readtable)))
               (*read-eval* nil)
               (*reader-positions* positions)
               (eof (list nil)))
          (flet ((next () (if (or positions quotes)
                              (eclector.parse-result:read eclector.reader:*client* stream nil eof)
                              (eclector.reader:read stream nil eof))))
            (declare (dynamic-extent #'next))
            (funcall function #'next eof))))
    (lasm-error (e) (error e))
    (storage-condition () (funcall fail "~A: nested too deeply" path))
    (error (e) (funcall fail "~A: unreadable (~A)" path e))))

(defun read-restricted-form (stream fail path &key (bare :keyword) positions quotes)
  "The single form on STREAM, read without interning symbols, evaluating or
building shared structure. FAIL, called with a format control and arguments,
signals the caller's own condition for anything unreadable; conditions it
signals pass through, and PATH names STREAM in messages. A symbol with no
package prefix must already exist as a keyword, or with BARE :UNINTERNED is
made fresh; a prefixed one must already exist. POSITIONS and QUOTES are as in
%READ-RESTRICTED."
  (%read-restricted stream fail path bare positions quotes
                    (lambda (next eof)
                      (let ((form (funcall next)))
                        (unless (eq (funcall next) eof)
                          (funcall fail "trailing data"))
                        (if (eq form eof) :eof form)))))

(defun read-restricted-forms (stream fail path &key (bare :keyword) positions quotes)
  "Every form on STREAM, read as READ-RESTRICTED-FORM reads one."
  (%read-restricted stream fail path bare positions quotes
                    (lambda (next eof)
                      (loop for form = (funcall next)
                            until (eq form eof)
                            collect form))))
