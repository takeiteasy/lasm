;;;; include.lisp
;;;; .include statement expansion (#78). Like .macro (macro.lisp), .include
;;;; splices a range of statements into the program, which no DEFDIRECTIVE
;;;; action (a fixed, statically sized, per-statement vocabulary -- see
;;;; directive.lisp) can express, so it is recognized by mnemonic text here
;;;; instead of being registered in *DIRECTIVES*.
;;;;
;;;; EXPAND-INCLUDES runs after PARSE and before EXPAND-MACROS, so an included
;;;; file can define .macro blocks and .equ constants the including file uses.
;;;; Each included file is parsed with the including file's lexer; its path is
;;;; resolved against the directory of the file that names it, so a nested
;;;; include is relative to its own includer, not the top-level file. A file
;;;; included twice is processed twice (no include guards).

(in-package #:lasm)

(define-condition include-error (lasm-syntax-error) ()
  (:documentation "Signalled on a malformed .include (no single string
operand, a mode suffix), a target file that does not exist, a circular
include, or an .include left unexpanded in the statements handed to
ASSEMBLE-STATEMENTS."))

(defun %include-error (line fmt &rest args)
  (error 'include-error :message (apply #'format nil fmt args) :line line))

(defvar *include-directory* nil
  "Directory .include paths resolve against; NIL means
*DEFAULT-PATHNAME-DEFAULTS*. ASSEMBLE-FILE and EXPAND-INCLUDES rebind it per
file.")

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

(defun %expand-include (statement lexer)
  (let* ((line (statement-line statement))
         (path (%include-path statement))
         (file (probe-file (merge-pathnames path (or *include-directory*
                                                     *default-pathname-defaults*)))))
    (unless file
      (%include-error line ".include ~S: file not found" path))
    (when (member file *include-chain* :test #'equal)
      (%include-error line "Circular .include: ~{~A~^ -> ~}"
                      (mapcar #'namestring (reverse (cons file *include-chain*)))))
    (let ((label-statement
            (when (statement-label statement)
              (make-statement :label (statement-label statement)
                              :label-localp (statement-label-localp statement)
                              :line line)))
          ;; TODO: included statements keep their own file's line numbers, so
          ;; assembly diagnostics and listings render them against the
          ;; top-level source text; see #95 (source file name in diagnostics).
          (body (let ((*include-directory* (%file-directory file))
                      (*include-chain* (cons file *include-chain*)))
                  (expand-includes (parse (%read-source-file file) :lexer lexer)
                                   :lexer lexer))))
      (append (and label-statement (list label-statement)) body))))

(defun expand-includes (statements &key (lexer 'default))
  "Replace every .include statement in STATEMENTS (parser.lisp) with the
statements of the named file, parsed with LEXER, recursively. A label on the
.include line is kept as a label-only statement ahead of the included body.
Signals INCLUDE-ERROR on a malformed .include, a missing target, or a
circular include; a missing top-level file is the ordinary CL FILE-ERROR."
  (loop for statement in statements
        if (%include-statement-p statement)
          append (%expand-include statement lexer)
        else collect statement))
