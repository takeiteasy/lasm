;;;; cli.lisp
;;;; #80 (M7): the command-line front end. RUN-CLI is a pure function over an
;;;; argument list so it can be tested without a shell; roswell/lasm.ros is
;;;; the thin executable around it.

(in-package #:lasm)

(define-condition cli-usage-error (error)
  ((message :initarg :message :reader cli-usage-error-message))
  (:report (lambda (c s) (write-string (cli-usage-error-message c) s))))

(defun %usage-error (control &rest args)
  (error 'cli-usage-error :message (apply #'format nil control args)))

(defparameter *cli-usage*
  "usage: lasm COMMAND FILE -m MACHINE.lasm [options]

commands:
  assemble FILE     assemble to a file        [-o OUT] [--format bin|hex] [--origin N]
  run FILE          assemble and run          [--max-steps N] [--cycles N]
  disassemble FILE  disassemble a binary file [--origin N] [--annotate]
                    [--data-region START:END]...
  listing FILE      print an assembly listing [--symbols]

options:
  -m, --machine FILE     machine definition (.lasm), required
  --machine-name NAME    machine to use when FILE defines several
  --lexer NAME           lexer to use when FILE defines several
  --memory NAME          memory element to target
  -h, --help             show this help
")

(defparameter *cli-value-options*
  '(("-m" . :machine-file) ("--machine" . :machine-file)
    ("-o" . :output) ("--output" . :output)
    ("--format" . :format) ("--origin" . :origin)
    ("--machine-name" . :machine-name) ("--lexer" . :lexer) ("--memory" . :memory)
    ("--max-steps" . :max-steps) ("--cycles" . :cycles)))

(defparameter *cli-repeatable-options*
  '(("--data-region" . :data-regions)))

(defparameter *cli-flag-options*
  '(("--symbols" . :symbols) ("--annotate" . :annotate)
    ("-h" . :help) ("--help" . :help)))

(defun %cli-parse (args)
  "Returns (VALUES COMMAND FILE OPTIONS), OPTIONS a plist."
  (let ((options '()) (positional '()))
    (loop while args
          do (let* ((arg (cl:pop args))
                    (value-key (cdr (assoc arg *cli-value-options* :test #'string=)))
                    (repeat-key (cdr (assoc arg *cli-repeatable-options* :test #'string=)))
                    (flag-key (cdr (assoc arg *cli-flag-options* :test #'string=))))
               (cond (value-key
                      (unless args (%usage-error "~A needs a value" arg))
                      (setf (getf options value-key) (cl:pop args)))
                     (repeat-key
                      (unless args (%usage-error "~A needs a value" arg))
                      (setf (getf options repeat-key) (append (getf options repeat-key) (list (cl:pop args)))))
                     (flag-key (setf (getf options flag-key) t))
                     ((and (> (length arg) 1) (char= (char arg 0) #\-))
                      (%usage-error "unknown option ~A" arg))
                     (t (cl:push arg positional)))))
    (setf positional (nreverse positional))
    (when (> (length positional) 2)
      (%usage-error "unexpected argument ~A" (third positional)))
    (values (first positional) (second positional) options)))

(defun %cli-integer (text option)
  (or (ignore-errors
       (cond ((and (> (length text) 1) (char= (char text 0) #\$))
              (parse-integer text :start 1 :radix 16))
             ((and (> (length text) 2) (string-equal "0x" text :end2 2))
              (parse-integer text :start 2 :radix 16))
             (t (parse-integer text))))
      (%usage-error "~A needs an integer, got ~S" option text)))

(defun %cli-option-integer (options key option)
  (let ((text (getf options key)))
    (and text (%cli-integer text option))))

(defun %cli-data-regions (options)
  "OPTIONS' --data-region START:END values as (START . END) conses."
  (mapcar (lambda (text)
            (let* ((colon (position #\: text))
                   (start (and colon (%cli-integer (subseq text 0 colon) "--data-region")))
                   (end (and colon (%cli-integer (subseq text (1+ colon)) "--data-region"))))
              (unless (and start end (< start end))
                (%usage-error "--data-region needs START:END with START < END, got ~S" text))
              (cons start end)))
          (getf options :data-regions)))

;;; Machine definitions

(defun %copy-table (table)
  (let ((copy (make-hash-table :test (hash-table-test table))))
    (maphash (lambda (k v) (setf (gethash k copy) v)) table)
    copy))

(defun %table-keys (table)
  (loop for k being the hash-keys of table collect k))

(defun %cli-named (text table what file)
  (let ((symbol (find-symbol (string-upcase text) '#:lasm)))
    (unless (and symbol (nth-value 1 (gethash symbol table)))
      (error "~A defines no ~A named ~A" file what text))
    symbol))

(defun %cli-pick-machine (file explicit)
  (if explicit
      (%cli-named explicit *machines* "machine" file)
      (let ((names (%table-keys *machines*)))
        (case (length names)
          (0 (error "~A defines no machine" file))
          (1 (first names))
          (t (error "~A defines several machines (~{~(~A~)~^, ~}): pass --machine-name"
                    file names))))))

(defun %cli-pick-lexer (file explicit before)
  (if explicit
      (%cli-named explicit *lexers* "lexer" file)
      (let ((names (set-difference (%table-keys *lexers*) before)))
        (case (length names)
          (0 'default)
          (1 (first names))
          (t (error "~A defines several lexers (~{~(~A~)~^, ~}): pass --lexer" file names))))))

(defun %cli-call-with-definitions (options function)
  "Load OPTIONS' machine file into a private machine table and call FUNCTION
with the chosen machine and lexer names. Loaded definitions never leak into
the calling image."
  (let ((file (or (getf options :machine-file) (%usage-error "-m MACHINE.lasm is required"))))
    (let* ((*machines* (make-hash-table :test 'eq))
           (*lexers* (%copy-table *lexers*))
           (before (%table-keys *lexers*))
           (*package* (find-package '#:lasm)))
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (load file))
      (funcall function
               (%cli-pick-machine file (getf options :machine-name))
               (%cli-pick-lexer file (getf options :lexer) before)))))

;;; Commands

(defun %cli-assemble (file machine lexer options)
  (assemble-file file :machine machine :lexer lexer :memory (%cli-memory options)
                      :origin (or (%cli-option-integer options :origin "--origin") 0)))

(defun %cli-memory (options)
  (let ((name (getf options :memory)))
    (and name (intern (string-upcase name) '#:lasm))))

(defun %cli-read-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun %cli-command-assemble (file machine lexer options out)
  (let* ((format (string-downcase (or (getf options :format) "bin")))
         (extension (cond ((string= format "bin") "bin")
                          ((string= format "hex") "hex")
                          (t (%usage-error "--format must be bin or hex, got ~A" format))))
         (path (or (getf options :output)
                   (namestring (make-pathname :type extension :defaults file))))
         (assembly (%cli-assemble file machine lexer options))
         (memory (%cli-memory options)))
    (funcall (if (string= format "hex") #'write-intel-hex #'write-binary)
             assembly path :machine machine :memory memory)
    (format out "wrote ~A (~D bytes)~%" path
            (length (assembly-bytes assembly :machine machine :memory memory)))
    0))

(defun %cli-command-run (file machine lexer options out)
  (let* ((assembly (%cli-assemble file machine lexer options))
         (memory (%cli-memory options))
         (m (make-machine machine))
         (max-steps (or (%cli-option-integer options :max-steps "--max-steps") 10000))
         (cycles (%cli-option-integer options :cycles "--cycles")))
    (load-program m assembly :memory memory)
    (multiple-value-bind (reason steps condition)
        (if cycles
            (run-for-cycles m cycles :max-steps max-steps :memory memory)
            (run m :max-steps max-steps :memory memory))
      (format out "stopped: ~(~A~) after ~D step~:P, pc = $~4,'0X~%" reason steps (sref m 'pc))
      (when (eq reason :fault)
        (format out "~A~%" condition))
      (if (member reason '(:decode-failure :fault)) 1 0))))

(defun %cli-command-disassemble (file machine lexer options out)
  (let* ((memory (%cli-memory options))
         (cell-width (%machine-cell-width machine memory))
         (endian (if (> cell-width 8)
                       (%endian-byte-order (%machine-endian machine memory))
                       :little))
         (origin (or (%cli-option-integer options :origin "--origin") 0))
         (lines (disassemble-cells (bytes-to-cells (%cli-read-bytes file) cell-width :endian endian)
                                   :machine machine :origin origin :lexer lexer :memory memory
                                   :data-regions (%cli-data-regions options))))
    (if (getf options :annotate)
        (print-disassembly lines :stream out)
        (disassembly-text lines :stream out :origin origin))
    0))

(defun %cli-command-listing (file machine lexer options out)
  (let ((assembly (%cli-assemble file machine lexer options)))
    (print-listing assembly :stream out)
    (when (getf options :symbols)
      (terpri out)
      (print-symbols assembly :stream out))
    0))

(defun run-cli (args &key (out *standard-output*) (err *error-output*))
  "Run the lasm command line over ARGS (a list of strings, without the program
name) and return its exit status: 0 on success, 1 on an assembly, load or run
failure, 2 on a usage error. Output goes to OUT, diagnostics to ERR."
  (handler-case
      (multiple-value-bind (command file options) (%cli-parse args)
        (cond ((or (getf options :help) (null command))
               (write-string *cli-usage* (if (getf options :help) out err))
               (if (getf options :help) 0 2))
              (t
               (let ((handler (cdr (assoc command
                                          '(("assemble" . %cli-command-assemble)
                                            ("run" . %cli-command-run)
                                            ("disassemble" . %cli-command-disassemble)
                                            ("listing" . %cli-command-listing))
                                          :test #'string=))))
                 (unless handler (%usage-error "unknown command ~A" command))
                 (unless file (%usage-error "~A needs a FILE" command))
                 (%cli-call-with-definitions
                  options
                  (lambda (machine lexer)
                    (funcall handler file machine lexer options out)))))))
    (cli-usage-error (c)
      (format err "lasm: ~A~%~%~A" c *cli-usage*)
      2)
    (error (c)
      (format err "lasm: ~A~%" c)
      1)))
