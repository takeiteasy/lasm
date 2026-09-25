;;;; include.lisp
;;;; .include statement helpers (#78). Like .macro (macro.lisp), .include
;;;; splices a range of statements into the program, which no DEFDIRECTIVE
;;;; action can express, so it is recognized by mnemonic text instead of being
;;;; registered in *DIRECTIVES*. PREPROCESS (preprocess.lisp) expands it when
;;;; the statement is reached in an emitting region.
;;;;
;;;; Each included file is parsed with the including file's lexer; its path is
;;;; resolved against the directory of the file that names it, so a nested
;;;; include is relative to its own includer, not the top-level file. A file
;;;; included twice is processed twice.

(in-package #:lasm)

(define-condition include-error (lasm-syntax-error) ()
  (:documentation "Signalled on a malformed .include (no single string
operand, a mode suffix), a target file that does not exist, or a circular
include."))

(defun %include-error (line fmt &rest args)
  (error 'include-error :message (apply #'format nil fmt args) :line line))

(defvar *include-directory* nil
  "Directory .include paths resolve against; NIL means
*DEFAULT-PATHNAME-DEFAULTS*. ASSEMBLE-FILE and PREPROCESS rebind it per
file.")

(defvar *include-sources* nil
  "NIL to read .include targets from disk, or an EQUAL hash table from
namestring to (TRUENAME-NAMESTRING . TEXT): .include then reads only from it,
and a file it does not hold is not found.")

(defvar *include-chain* nil
  "Truenames of the files being included, innermost first, for cycle detection.")

(defun %read-source-file (path)
  "The text of the file at PATH, lines joined with #\\Newline. A missing file
signals the ordinary CL FILE-ERROR."
  (with-open-file (in path)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
            for first = t then nil
            while line
            do (unless first (write-char #\Newline out))
               (write-string line out)))))

(defun %file-directory (truename)
  (make-pathname :name nil :type nil :version nil :defaults truename))

(defun %include-statement-p (statement)
  (let ((mnemonic (statement-mnemonic statement)))
    (and mnemonic (string-equal mnemonic ".include"))))

(defun %include-path (statement)
  (let ((line (statement-line statement))
        (tokens (statement-operand-tokens statement)))
    (when (statement-mode-suffix statement)
      (%include-error line ".include: a mode suffix is not valid here"))
    (unless (and (= (length tokens) 1) (eq (token-type (aref tokens 0)) :string))
      (%include-error line ".include: expected a single quoted path"))
    (token-value (aref tokens 0))))

