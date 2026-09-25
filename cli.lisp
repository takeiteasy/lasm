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
                    [--bank N] [--region NAME] [--packing pad|bits]
  run [FILE]        assemble and run          [--max-steps N] [--cycles N]
                    [--load-snapshot PATH] [--save-snapshot PATH] [--snapshot-format sexp|binary]
  disassemble FILE  disassemble a binary file [--origin N] [--annotate] [--packing pad|bits]
                    [--cells N] [--data-region START:END]...
  listing FILE      print an assembly listing [--symbols] [--cycle-costs]
  debug [FILE]      assemble and debug        [--break WHERE]... [--commands FILE] [--history N]
                    [--load-snapshot PATH] [--save-snapshot PATH] [--snapshot-format sexp|binary]

FILE may be left out of run and debug with --load-snapshot when the snapshot
was saved from a file: the program is rebuilt from the source it holds.

options:
  -m, --machine FILE     machine definition (.lasm), required
  --machine-name NAME    machine to use when FILE defines several
  --quiet                suppress assembly warnings
  --lexer NAME           lexer to use when FILE defines several
  --memory NAME          memory element to target
  --bank N               write only bank N of a banked region (assemble)
  --region NAME          banked region for --bank when there are several
  --packing pad|bits     cells that are not whole bytes: pad each to bytes (default)
                         or pack them as a bitstream
  --load-snapshot PATH   restore machine state from a snapshot after loading (run, debug)
  --save-snapshot PATH   write machine state and program source to a snapshot at the end (run, debug)
  --snapshot-format F    sexp (readable, default) or binary (compact) for --save-snapshot
  --break WHERE          set a breakpoint before the prompt appears (debug)
  --commands FILE        run debugger commands from FILE first (debug)
  --history N            keep N steps of step-back history (debug)
  --cells N              number of cells in the file (disassemble), to drop bit padding
  -h, --help             show this help
")

(defparameter *cli-value-options*
  '(("-m" . :machine-file) ("--machine" . :machine-file)
    ("-o" . :output) ("--output" . :output)
    ("--format" . :format) ("--origin" . :origin)
    ("--machine-name" . :machine-name) ("--lexer" . :lexer) ("--memory" . :memory)
    ("--bank" . :bank) ("--region" . :region) ("--packing" . :packing) ("--cells" . :cells)
    ("--max-steps" . :max-steps) ("--cycles" . :cycles)
    ("--save-snapshot" . :save-snapshot) ("--load-snapshot" . :load-snapshot)
    ("--snapshot-format" . :snapshot-format)
    ("--history" . :history) ("--commands" . :commands)))

(defparameter *cli-repeatable-options*
  '(("--data-region" . :data-regions) ("--break" . :breaks)))

(defparameter *cli-flag-options*
  '(("--symbols" . :symbols) ("--cycle-costs" . :cycle-costs) ("--annotate" . :annotate)
    ("--quiet" . :quiet)
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
      (%signal-usage-error 'usage-error "~A defines no ~A named ~A" file what text))
    symbol))

(defun %cli-pick-machine (file explicit)
  (if explicit
      (%cli-named explicit *machines* "machine" file)
      (let ((names (%table-keys *machines*)))
        (case (length names)
          (0 (%signal-usage-error 'usage-error "~A defines no machine" file))
          (1 (first names))
          (t (%signal-usage-error 'usage-error "~A defines several machines (~{~(~A~)~^, ~}): pass --machine-name"
                    file names))))))

(defun %cli-pick-lexer (file explicit before)
  (if explicit
      (%cli-named explicit *lexers* "lexer" file)
      (let ((names (set-difference (%table-keys *lexers*) before)))
        (case (length names)
          (0 'default)
          (1 (first names))
          (t (%signal-usage-error 'usage-error "~A defines several lexers (~{~(~A~)~^, ~}): pass --lexer" file names))))))

(defun %cli-call-with-definitions (options function)
  "Load OPTIONS' machine file into a private machine table and call FUNCTION
with the chosen machine and lexer names. Loaded definitions never leak into
the calling image."
  (let ((file (or (getf options :machine-file) (%usage-error "-m MACHINE.lasm is required"))))
    (let* ((*machines* (make-hash-table :test 'eq))
           (*lexers* (%copy-table *lexers*))
           (*modes* (%copy-table *modes*))
           (*machine-modes* (make-hash-table :test 'eq))
           (before (%table-keys *lexers*))
           (*package* (find-package '#:lasm)))
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (load file))
      (funcall function
               (%cli-pick-machine file (getf options :machine-name))
               (and (not (getf options :snapshot-only))
                    (%cli-pick-lexer file (getf options :lexer) before))))))

;;; Commands

(defun %cli-assemble (file machine lexer options)
  (assemble-file file :machine machine :lexer lexer :memory (%cli-memory options)
                      :origin (or (%cli-option-integer options :origin "--origin") 0)))

(defun %cli-memory (options)
  (let ((name (getf options :memory)))
    (and name (intern (string-upcase name) '#:lasm))))

(defun %cli-packing (options)
  (let ((name (getf options :packing)))
    (cond ((null name) :pad)
          ((member name '("pad" "bits") :test #'string-equal) (intern (string-upcase name) :keyword))
          (t (%usage-error "--packing must be pad or bits, got ~A" name)))))

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
         (memory (%cli-memory options))
         (bank (%cli-option-integer options :bank "--bank"))
         (region (and (getf options :region)
                      (intern (string-upcase (getf options :region)) '#:lasm)))
         (packing (%cli-packing options)))
    (funcall (if (string= format "hex") #'write-intel-hex #'write-binary)
             assembly path :machine machine :memory memory :bank bank :region region
                           :packing packing)
    (format out "wrote ~A (~D bytes)~%" path
            (length (assembly-bytes assembly :machine machine :memory memory
                                             :bank bank :region region :packing packing)))
    0))

(defun %cli-snapshot-format (options)
  (let ((name (getf options :snapshot-format)))
    (cond ((null name) :sexp)
          ((member name '("sexp" "binary") :test #'string-equal) (intern (string-upcase name) :keyword))
          (t (%usage-error "--snapshot-format must be sexp or binary, got ~A" name)))))

(defun %cli-program (file machine lexer options)
  "(VALUES ASSEMBLY SNAPSHOT): ASSEMBLY of FILE, or rebuilt from the
--load-snapshot snapshot's embedded source when there is no FILE. SNAPSHOT is
that snapshot, or NIL."
  (let ((path (getf options :load-snapshot)))
    (if file
        (values (%cli-assemble file machine lexer options) (and path (read-snapshot path)))
        (let ((snapshot (read-snapshot path)))
          (values (or (snapshot-assembly snapshot :machine machine)
                      (%snapshot-fail 'snapshot-malformed "~A has no embedded program; pass FILE" path))
                  snapshot)))))

(defun %cli-loaded-machine (assembly machine snapshot)
  "A MACHINE instance with ASSEMBLY loaded, then restored from SNAPSHOT when
there is one."
  (let ((m (make-machine machine)))
    (load-program m assembly :memory (getf (assembly-parameters assembly) :memory))
    (when snapshot
      (restore-snapshot m snapshot))
    m))

(defun %cli-save-snapshot (m assembly options)
  (let ((path (getf options :save-snapshot)))
    (when path
      (write-snapshot (machine-snapshot m :assembly assembly) path
                      :format (%cli-snapshot-format options)))))

(defun %cli-command-run (file machine lexer options out)
  (%cli-snapshot-format options)
  (multiple-value-bind (assembly snapshot) (%cli-program file machine lexer options)
    (%cli-run assembly (%cli-loaded-machine assembly machine snapshot) options out)))

(defun %cli-run (assembly m options out)
  (let* ((memory (getf (assembly-parameters assembly) :memory))
         (max-steps (or (%cli-option-integer options :max-steps "--max-steps") 10000))
         (cycles (%cli-option-integer options :cycles "--cycles")))
    (multiple-value-bind (reason steps condition)
        (if cycles
            (run-for-cycles m cycles :max-steps max-steps :memory memory)
            (run m :max-steps max-steps :memory memory))
      (%cli-save-snapshot m assembly options)
      (format out "stopped: ~(~A~) after ~D step~:P, pc = $~4,'0X~%" reason steps (sref m 'pc))
      (when (or (eq reason :fault)
                (and (eq reason :trap) (eq (lasm-trap-tag condition) :undefined-opcode)))
        (format out "~A~%" condition))
      (when (eq reason :decode-failure)
        (let ((line (machine-listing-line m (sref m 'pc) :memory memory)))
          (when line
            (format out "line ~D: ~A~%" (listing-line-line line)
                    (string-trim '(#\Space #\Tab) (or (listing-line-source-text line assembly) ""))))))
      (if (or (member reason '(:decode-failure :fault))
              (and (eq reason :trap) (eq (lasm-trap-tag condition) :undefined-opcode)))
          1
          0))))

(defun %cli-command-disassemble (file machine lexer options out)
  (let* ((memory (%cli-memory options))
         (cell-width (%machine-cell-width machine memory))
         (endian (if (= cell-width 8)
                       :little
                       (%endian-byte-order (%machine-endian machine memory))))
         (packing (%cli-packing options))
         (origin (or (%cli-option-integer options :origin "--origin") 0))
         (lines (disassemble-cells (bytes-to-cells (%cli-read-bytes file) cell-width :endian endian
                                                                  :packing packing
                                                                  :count (%cli-option-integer options :cells "--cells"))
                                   :machine machine :origin origin :lexer lexer :memory memory
                                   :data-regions (%cli-data-regions options))))
    (if (getf options :annotate)
        (print-disassembly lines :stream out)
        (disassembly-text lines :stream out :origin origin))
    0))

(defun %cli-command-listing (file machine lexer options out)
  (let ((assembly (%cli-assemble file machine lexer options)))
    (print-listing assembly :stream out :cycles (getf options :cycle-costs))
    (when (getf options :symbols)
      (terpri out)
      (print-symbols assembly :stream out))
    0))

(defun %cli-command-debug (file machine lexer options out)
  (let ((history (%cli-option-integer options :history "--history"))
        (in (or (getf options :in) *standard-input*)))
    (when (and history (< history 1))
      (%usage-error "--history needs a positive integer, got ~D" history))
    (%cli-snapshot-format options)
    (multiple-value-bind (assembly snapshot) (%cli-program file machine lexer options)
     ;; TODO: :lexer is not passed, so breakpoint conditions use the default lexer (#285)
     (let ((session (make-debug-session (%cli-loaded-machine assembly machine snapshot)
                                        :assembly assembly
                                        :memory (getf (assembly-parameters assembly) :memory)
                                        :history history)))
      (dolist (where (getf options :breaks))
        (write-string (debug-command session (format nil "break ~A" where)) out))
      (unless (%cli-run-command-file session (getf options :commands) out)
        ;; TODO: plain READ-LINE, no editing or history on a terminal (#284)
        (debugger-repl session :input in :output out))
      (%cli-save-snapshot (debug-session-machine session) assembly options)
      0))))

(defun %cli-run-command-file (session path out)
  "Dispatch each line of the file at PATH, echoing it after the prompt. True
if a line quit the session."
  (when path
    (with-open-file (in path)
      (loop for line = (read-line in nil nil)
            while line
            do (format out "(lasm-dbg) ~A~%" line)
               (when (nth-value 1 (debug-command session line :stream out))
                 (return t))))))

(defun %cli-report-warning (err quiet)
  "A HANDLER-BIND handler that prints an assembly warning to ERR as
FILE:LINE: warning: MESSAGE, or drops it under QUIET."
  (lambda (warning)
    (unless quiet
      (format err "~@[~A:~]~@[~D:~] warning: ~A~%"
              (lasm-warning-file warning) (lasm-warning-line warning)
              (lasm-warning-message warning)))
    (muffle-warning warning)))

(defun run-cli (args &key (in *standard-input*) (out *standard-output*) (err *error-output*))
  "Run the lasm command line over ARGS (a list of strings, without the program
name) and return its exit status: 0 on success, 1 on an assembly, load or run
failure, 2 on a usage error. Debugger commands are read from IN, output goes
to OUT, diagnostics to ERR."
  (handler-case
      (multiple-value-bind (command file options) (%cli-parse args)
        (setf (getf options :in) in)
        (cond ((or (getf options :help) (null command))
               (write-string *cli-usage* (if (getf options :help) out err))
               (if (getf options :help) 0 2))
              (t
               (let ((handler (cdr (assoc command
                                          '(("assemble" . %cli-command-assemble)
                                            ("run" . %cli-command-run)
                                            ("disassemble" . %cli-command-disassemble)
                                            ("listing" . %cli-command-listing)
                                            ("debug" . %cli-command-debug))
                                          :test #'string=))))
                 (unless handler (%usage-error "unknown command ~A" command))
                 (unless file
                   (unless (and (member command '("run" "debug") :test #'string=)
                                (getf options :load-snapshot))
                     (%usage-error "~A needs a FILE~:[~; (or --load-snapshot PATH)~]"
                                   command (member command '("run" "debug") :test #'string=)))
                   (dolist (key '(:origin :lexer :memory))
                     (when (getf options key)
                       (%usage-error "--~(~A~) is taken from the snapshot when FILE is left out" key)))
                   (setf (getf options :snapshot-only) t))
                 (%cli-call-with-definitions
                  options
                  (lambda (machine lexer)
                    (handler-bind ((lasm-warning (%cli-report-warning err (getf options :quiet))))
                      (funcall handler file machine lexer options out))))))))
    (cli-usage-error (c)
      (format err "lasm: ~A~%~%~A" c *cli-usage*)
      2)
    (error (c)
      (format err "lasm: ~A~%" c)
      1)))
