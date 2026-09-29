;;;; diagnostic.lisp
;;;; Positioned diagnostics retain source text and file after the signalling
;;;; call returns. Context handlers fill missing slots without resignalling.

(in-package #:lasm)

;;; Source-line rendering

(defun %nth-source-line (source n)
  "The N'th line (1-indexed) of SOURCE, or NIL if SOURCE has fewer than N
lines. A plain READ-LINE walk -- SOURCE is at most a few hundred lines for
any program LASM realistically assembles, so this needn't be smarter."
  (with-input-from-string (in source)
    (loop for i from 1
          for text = (read-line in nil nil)
          while text
          when (= i n) return text)))

(defun %offset-line-column (text offset)
  "1-based (VALUES LINE COLUMN) of the character OFFSET in TEXT -- used by a
reader that records a form's position as an offset (compiler.lisp, items.lisp)
rather than tracking line and column as it goes, as the lexer does."
  (let ((line 1) (column 1))
    (dotimes (i (min offset (length text)))
      (if (char= (char text i) #\Newline)
          (setf line (1+ line) column 1)
          (incf column)))
    (values line column)))

(defun %slurp-file (path)
  "The text of the file PATH, read as characters (not bytes) so its length
matches the character offsets a stream over it reports."
  (with-open-file (in path)
    (let* ((buffer (make-string (file-length in)))
           (n (read-sequence buffer in)))
      (subseq buffer 0 n))))

(defun diagnostic-text (condition &key (source nil source-supplied-p))
  "Render CONDITION (a LASM-SYNTAX-ERROR) as a diagnostic report: its
position and message, followed -- when source text is available at the
condition's own line -- by that source line and a caret under the offending
column, e.g.:

  line 3, column 5: ldx: operand \"(#5),Y\" matches no addressing mode of ldx
  3 | ldx (#5),Y
    |     ^

SOURCE overrides the condition's stored text. File-backed input renders
PATH:LINE:COLUMN; input without a file renders LINE N, COLUMN C. Without
a line, only the message appears. Without a column, the caret is omitted."
  (let* ((message (lasm-syntax-error-message condition))
         (line (lasm-syntax-error-line condition))
         (column (lasm-syntax-error-column condition))
         (src (if source-supplied-p source (lasm-syntax-error-source condition)))
         (src-line (and src line (%nth-source-line src line)))
         (file (lasm-syntax-error-file condition))
         (definition-line (lasm-syntax-error-definition-line condition))
         (definition-file (lasm-syntax-error-definition-file condition))
         (definition-source (lasm-syntax-error-definition-source condition)))
    (with-output-to-string (out)
      (if line
          (if file
              (format out "~A:~D~@[:~D~]: ~A" file line column message)
              (format out "line ~D~@[, column ~D~]: ~A" line column message))
          (format out "~A" message))
      (when src-line
        (let* ((label (format nil "~D" line))
               (gutter (make-string (length label) :initial-element #\Space)))
          (format out "~%~A | ~A" label src-line)
          (when column
            (format out "~%~A | ~A^" gutter
                    (make-string (max 0 (1- column)) :initial-element #\Space)))))
      (when definition-line
        (if definition-file
            (format out "~%expanded from macro body ~A:~D" definition-file definition-line)
            (format out "~%expanded from macro body line ~D" definition-line))
        (let ((definition-text (and (or definition-source src)
                                    (%nth-source-line (or definition-source src) definition-line))))
          (when definition-text
            (format out "~%~D | ~A" definition-line definition-text)))))))

;;; Conditions

;; Named LASM-SYNTAX-ERROR rather than PARSE-ERROR because CL:PARSE-ERROR is
;; a standard condition type and this package :USEs #:CL. Moved here from
;; storage.lisp so the condition and its renderer live together.
;;; Definition errors: a malformed DEFMACHINE/DEFINSTRUCTION/DEFMODE/DEFLEXER/
;;; DEFDIRECTIVE form, as opposed to a program's own source (LASM-SYNTAX-ERROR).

(defvar *definition-name* nil
  "Name of the definition being built, recorded on DEFINITION-ERRORs.")

(defvar *definition-errors* nil
  "Inside WITH-DEFINITION-ERRORS, the DEFINITION-ERRORs signalled so far,
newest first.")

(defvar *collecting-definition-errors* nil
  "True inside WITH-DEFINITION-ERRORS.")

(define-condition definition-error (lasm-error)
  ((message :initarg :message :initform nil :reader definition-error-message)
   (name :initarg :name :initform nil :reader definition-error-name))
  (:report (lambda (c s) (write-string (definition-error-message c) s))))

(define-condition machine-definition-error (definition-error) ())
(define-condition instruction-definition-error (definition-error) ())
(define-condition mode-definition-error (definition-error) ())
(define-condition lexer-definition-error (definition-error) ())
(define-condition directive-definition-error (definition-error) ())

(defun %definition-error (type control &rest args)
  (let ((condition (make-condition type :message (apply #'format nil control args)
                                        :name *definition-name*)))
    (when *collecting-definition-errors*
      (cl:push condition *definition-errors*))
    (error condition)))

(defun %defmachine-error (control &rest args)
  (apply #'%definition-error 'machine-definition-error control args))

(defun %definstruction-error (control &rest args)
  (apply #'%definition-error 'instruction-definition-error control args))

(defun %defmode-error (control &rest args)
  (apply #'%definition-error 'mode-definition-error control args))

(defun %deflexer-error (control &rest args)
  (apply #'%definition-error 'lexer-definition-error control args))

(defun %defdirective-error (control &rest args)
  (apply #'%definition-error 'directive-definition-error control args))

(defun %call-with-definition-errors (thunk)
  (let ((*collecting-definition-errors* t)
        (*definition-errors* nil))
    (flet ((first-recorded () (car (last *definition-errors*))))
      (multiple-value-prog1
          (handler-bind ((error (lambda (c)
                                  (when (and *definition-errors*
                                             (not (typep c 'definition-error)))
                                    (error (first-recorded))))))
            (funcall thunk))
        (when *definition-errors*
          (error (first-recorded)))))))

(defmacro with-definition-errors (&body body)
  "Run BODY, surfacing the typed DEFINITION-ERROR a definer signalled even when
SBCL's COMPILE-FILE or COMPILE turned it into a compile-time error. Signals the
first recorded DEFINITION-ERROR when BODY returns, or in place of any other
error that escapes BODY. An error BODY handles itself is still re-signalled on
return."
  `(%call-with-definition-errors (lambda () ,@body)))

;;; Under COMPILE-FILE SBCL turns a definer's error into a compile-time error,
;;; and the fasl then signals COMPILED-PROGRAM-ERROR on load. These helpers
;;; keep the failure a typed DEFINITION-ERROR, signalled again when the fasl
;;; loads.

(defun %warn-when-compiling-file (condition)
  (when *compile-file-truename*
    (warn "~A" condition)))

(defun %tolerate-definition-error (thunk failure collect name)
  "Call THUNK at compile time; a DEFINITION-ERROR becomes a warning and is
recorded in the cons FAILURE for the load-time form to replay. COLLECT also
surfaces an error SBCL deferred while compiling THUNK's code; it would report
one a definer handled itself, so only a definer that never does may set it."
  (handler-case (if collect
                    (let ((*definition-name* name))
                      (with-definition-errors (funcall thunk)))
                    (funcall thunk))
    (definition-error (c)
      (setf (car failure) (list (type-of c) (definition-error-message c) (definition-error-name c)))
      (%warn-when-compiling-file c))))

(defun %call-expanding-definition (thunk)
  "Call THUNK, a definer macro's expander. Under COMPILE-FILE a DEFINITION-ERROR
becomes a warning and the expansion is a form that signals it again at load."
  (if *compile-file-truename*
      (handler-case (funcall thunk)
        (definition-error (c)
          (%warn-when-compiling-file c)
          `(error ',(type-of c) :message ,(definition-error-message c)
                                :name ',(definition-error-name c))))
      (funcall thunk)))

(defmacro %expanding-definition (&body body)
  `(%call-expanding-definition (lambda () ,@body)))

(defun %registration-or-failure (failure)
  "The load-time form: the recorded error, signalled again, or the registration."
  (if (car failure)
      (destructuring-bind (type message name) (car failure)
        `(error ',type :message ,message :name ',name))
      '(register)))

(defun %definition-toplevel-form (registration result &key collect name)
  "Toplevel forms that run REGISTRATION at compile time, tolerating a
DEFINITION-ERROR, and at load and eval time, then yield RESULT. A failure at
compile time is signalled again when the fasl loads. See
%TOLERATE-DEFINITION-ERROR for COLLECT."
  (let ((failure (list nil)))
    `(macrolet ((register () ',registration)
                (evaluate-registration (&environment env)
                  (list 'eval (list 'quote (macroexpand-1 '(register) env))))
                (register-or-fail () (%registration-or-failure ',failure)))
       (eval-when (:compile-toplevel)
         (%tolerate-definition-error (lambda () (evaluate-registration)) ',failure ,collect ',name))
       (eval-when (:load-toplevel :execute)
         (register-or-fail)
         ,result))))

;;; Usage errors: a caller misusing the library API or a tool's input, as
;;; opposed to a malformed definition (DEFINITION-ERROR) or program source.

(define-condition usage-error (lasm-error)
  ((message :initarg :message :initform nil :reader usage-error-message))
  (:report (lambda (c s) (write-string (usage-error-message c) s))))

(define-condition debugger-usage-error (usage-error) ())
(define-condition disassembler-usage-error (usage-error) ())
(define-condition output-usage-error (usage-error) ())
(define-condition emulator-usage-error (usage-error) ())

(define-condition lookup-error (usage-error)
  ((name :initarg :name :reader lookup-error-name)))
(define-condition unknown-machine (lookup-error) ())
(define-condition unknown-isa (lookup-error) ())
(define-condition unknown-mode (lookup-error) ())
(define-condition unknown-lexer (lookup-error) ())

(defun %signal-usage-error (type control &rest args)
  (error type :message (apply #'format nil control args)))

(defun %lookup-error (type name control &rest args)
  (error type :name name :message (apply #'format nil control args)))

(defun %debugger-usage-error (control &rest args)
  (apply #'%signal-usage-error 'debugger-usage-error control args))
(defun %disassembler-usage-error (control &rest args)
  (apply #'%signal-usage-error 'disassembler-usage-error control args))
(defun %output-usage-error (control &rest args)
  (apply #'%signal-usage-error 'output-usage-error control args))
(defun %emulator-usage-error (control &rest args)
  (apply #'%signal-usage-error 'emulator-usage-error control args))

(defvar *definition-type* nil
  "Condition type of the definer being expanded, for %DEFINITION-BIND.")

(defmacro %with-definition ((name type) &body body)
  "Run BODY as the definition NAME: DEFINITION-ERRORs record NAME, and a
LOOKUP-ERROR (an unregistered machine, mode or lexer) becomes a TYPE."
  `(let ((*definition-name* ,name)
         (*definition-type* ',type))
     (handler-bind ((lookup-error
                      (lambda (c) (%definition-error ',type "~A" (usage-error-message c)))))
       ,@body)))

(defmacro %with-expanding-definition ((name type) &body body)
  "%WITH-DEFINITION for a definer macro's expander."
  `(%expanding-definition (%with-definition (,name ,type) ,@body)))

(defmacro %definition-bind (lambda-list form &body body)
  "DESTRUCTURING-BIND, but a lambda-list mismatch of FORM inside a definer is
a DEFINITION-ERROR rather than a raw Lisp error. Errors raised by BODY pass
through untouched."
  (let ((binding (gensym "BINDING")) (value (gensym "FORM")) (c (gensym "C"))
        (declarations (loop while (and (consp (first body)) (eq (first (first body)) 'declare))
                            collect (cl:pop body))))
    `(let ((,binding t) (,value ,form))
       (handler-bind ((error (lambda (,c)
                               (when (and ,binding *definition-type*
                                          (not (typep ,c 'lasm-error)))
                                 (%definition-error *definition-type* "Malformed ~S: ~A"
                                                    ,value ,c)))))
         (destructuring-bind ,lambda-list ,value
           ,@declarations
           (setf ,binding nil)
           ,@body)))))

(define-condition lasm-syntax-error (lasm-error)
  ((message :initarg :message :initform nil :reader lasm-syntax-error-message)
   (line :initarg :line :initform nil :accessor lasm-syntax-error-line)
   (column :initarg :column :initform nil :accessor lasm-syntax-error-column)
   (definition-line :initarg :definition-line :initform nil
                    :accessor lasm-syntax-error-definition-line)
   (definition-file :initarg :definition-file :initform nil
                    :accessor lasm-syntax-error-definition-file)
   (definition-source :initarg :definition-source :initform nil
                      :accessor lasm-syntax-error-definition-source)
   (file :initarg :file :initform nil :accessor lasm-syntax-error-file)
   (source :initarg :source :initform nil :accessor lasm-syntax-error-source))
  (:report (lambda (c s) (write-string (diagnostic-text c) s))))

(define-condition lex-error (lasm-syntax-error) ()
  (:documentation "Signalled by TOKENIZE on malformed source text."))

(define-condition parse-failure (lasm-syntax-error) ()
  (:documentation "Signalled by PARSE/PARSE-EXPRESSION on a malformed token stream."))

;; A warning, not an error -- assembly continues once WARN returns
;; (SBCL's default handler prints and resumes). Signalled by ASSEMBLER.LISP's
;; %CHOOSE-VARIANT when two or more syntax-matching candidates of an
;; instruction have the *same* total operand width, so width-based
;; relaxation cannot recover and declaration order alone decides -- the one
;; case "encoding ambiguity" can mean without contradicting the documented
;; declaration-order tiebreak (docs/assembler.md). Never fires on a pair like
;; ZERO-PAGE/ABSOLUTE, whose widths differ.
(define-condition lasm-warning (warning)
  ((message :initarg :message :initform nil :reader lasm-warning-message)
   (line :initarg :line :initform nil :reader lasm-warning-line)
   (file :initarg :file :initform nil :reader lasm-warning-file))
  (:report (lambda (c s)
             (format s "~A~@[ (line ~D)~]" (lasm-warning-message c) (lasm-warning-line c)))))

(define-condition ambiguous-mode (lasm-warning)
  ((mnemonic :initarg :mnemonic :reader ambiguous-mode-mnemonic)
   (chosen :initarg :chosen :reader ambiguous-mode-chosen)
   (alternatives :initarg :alternatives :reader ambiguous-mode-alternatives))
  (:documentation "Signalled when two or more of MNEMONIC's addressing-mode
candidates tie on total operand width -- CHOSEN (a MODE-DESCRIPTOR) is the
one declaration order picked; ALTERNATIVES (a list of MODE-DESCRIPTOR) are
the other tied candidates, in declaration order."))

(define-condition ambiguous-alternative (ambiguous-mode)
  ((hole :initarg :hole :reader ambiguous-alternative-hole)
   (slot :initarg :slot :initform nil :reader ambiguous-alternative-slot))
  (:documentation "Signalled when two or more alternatives of one ONE-OF
element match an operand equally well. HOLE is the element's first hole
index and SLOT its slot name, or NIL. CHOSEN and ALTERNATIVES name the
alternatives' own MODE-DESCRIPTORs."))

(define-condition idle-unwakeable (lasm-warning)
  ((machine :initarg :machine :reader idle-unwakeable-machine))
  (:report (lambda (c s)
             (format s "~S went idle with a zero interrupt vector and nothing queued; ~
every signal is dropped, so it cannot wake" (idle-unwakeable-machine c))))
  (:documentation "Signalled when MACHINE idles while :DROP-ON-ZERO-VECTOR would
drop every signal, leaving no way to wake it short of WAKE-MACHINE."))

(define-condition simple-style-warning (simple-condition style-warning) ()
  (:documentation "A STYLE-WARNING with a format control, as WARN takes on a string."))

(define-condition stale-mode (lasm-warning style-warning)
  ((mode :initarg :mode :reader stale-mode-mode)
   (dependents :initarg :dependents :initform nil :reader stale-mode-dependents)
   (instructions :initarg :instructions :initform nil :reader stale-mode-instructions))
  (:documentation "Signalled when redefining MODE leaves modes that reference it
invalid (DEPENDENTS, innermost first) or leaves compiled instructions built
against its old shape (INSTRUCTIONS, a list of (MACHINE . MNEMONIC))."))

(define-condition stale-backend (lasm-warning style-warning) ()
  (:documentation "Signalled when redefining a backend drops an operation that a
child backend's without-ops still names; the child is rebuilt without the name."))

;;; Source-context propagation

(defmacro with-source-context (source &body body)
  "Run BODY with every LASM-SYNTAX-ERROR it signals given SOURCE, unless it
already carries one -- e.g. a condition from a nested ASSEMBLE-STATEMENTS
call that already saw its own :SOURCE argument keeps that, not this form's.
Fills the slot via HANDLER-BIND and declines (returns normally from the
handler) rather than catching and resignalling, so the condition keeps
propagating with its original type, restarts and dynamic state intact --
this is what lets a condition caught outside BODY's own extent still render
with a source excerpt. SOURCE is evaluated once; a NIL SOURCE leaves every
condition's slot as it was."
  (let ((source-var (gensym "SOURCE")))
    `(let ((,source-var ,source))
       (if ,source-var
           (handler-bind ((lasm-syntax-error
                             (lambda (c)
                               (unless (lasm-syntax-error-source c)
                                 (setf (lasm-syntax-error-source c) ,source-var)))))
             ,@body)
           (progn ,@body)))))

(defmacro with-source-unit (unit &body body)
  "Attach UNIT's file and text to positioned conditions in BODY."
  (let ((unit-var (gensym "UNIT")))
    `(let ((,unit-var ,unit))
       (if ,unit-var
           (handler-bind ((lasm-syntax-error
                            (lambda (c)
                              (unless (lasm-syntax-error-source c)
                                (setf (lasm-syntax-error-source c)
                                      (source-unit-text ,unit-var)))
                              (unless (lasm-syntax-error-file c)
                                (setf (lasm-syntax-error-file c)
                                      (source-unit-file ,unit-var))))))
             ,@body)
           (progn ,@body)))))

;;; Strict operand range

(defvar *strict-operand-range* nil
  "When bound to T, every instruction operand is range-checked at encode
time against its addressing mode's own width -- an out-of-range value
signals ASSEMBLY-ERROR instead of being masked by WRAP-VALUE. Default NIL
preserves the original wrap-on-overflow behavior (a fantasy CPU may
define wraparound as intended). A single addressing mode can opt in without
this global switch via DEFMODE's :STRICT T (mode.lisp) -- see %ENCODE's
:INSTRUCTION branch (assembler.lisp) for where both are checked. This global
switch is the only way to cover an instruction with no addressing mode at
all (a bare (operand :width n) M1-style encoding), since :STRICT lives on a
MODE-DESCRIPTOR.")

;; The definers match their clause heads by symbol identity. Rewriting each
;; head to its lasm symbol lets a machine be defined from any package. Only
;; heads are rewritten: mode names, choice keys, option values and SEMANTICS
;; and CYCLES bodies stay the user's own.
(defparameter *dsl-clause-heads*
  (let ((table (make-hash-table :test #'equal)))
    (dolist (head '(register stack memory flags instruction-word clock-speed reset-pc device
                    stack-pointer interrupts undefined-opcode properties privilege idle
                    without-instructions instruction-cycles without-storage without-devices
                    region field layout extra-word-order
                    modes encoding semantics cycles opcode operand field-value for-choice
                    sub-opcode fallback variant choice sub holes range extra-word
                    comment-styles number-formats label-suffix local-label-prefix string-delim
                    ident-chars line-continuation mode-suffix-separator hole-prefix-separator
                    function-operators location-counter)
                    table)
      (setf (gethash (symbol-name head) table) head))))

(defun %dsl-head (form)
  "FORM with its head replaced by lasm's symbol when it names a DSL word. A form
that is not a list is left for the definer to reject."
  (let ((canonical (and (consp form) (symbolp (first form))
                        (gethash (symbol-name (first form)) *dsl-clause-heads*))))
    (if canonical (cons canonical (rest form)) form)))

(defun %dsl-heads-in (forms)
  (mapcar (lambda (form) (if (consp form) (%dsl-head form) form)) forms))

(defun %dsl-machine-clause (clause)
  (unless (consp clause) (return-from %dsl-machine-clause clause))
  (let ((clause (%dsl-head clause)))
    (if (member (first clause) '(memory instruction-word layout))
        (cons (first clause)
              (mapcar (lambda (item) (if (consp item) (%dsl-machine-clause item) item)) (rest clause)))
        clause)))

(defun %dsl-encoding-subclause (subclause)
  (unless (consp subclause) (return-from %dsl-encoding-subclause subclause))
  (let ((subclause (%dsl-head subclause)))
    (case (first subclause)
      (operand (list* (first subclause) (second subclause)
                      (mapcar (lambda (item)
                                (if (consp item) (%dsl-variant item) item))
                              (cddr subclause))))
      (for-choice (list* (first subclause) (second subclause)
                         (mapcar #'%dsl-encoding-subclause (cddr subclause))))
      (fallback (cons (first subclause) (mapcar #'%dsl-encoding-subclause (rest subclause))))
      (t subclause))))

(defun %dsl-variant (form)
  "An OPERAND's variant, or the atoms and plist values around one, unchanged."
  (unless (consp form) (return-from %dsl-variant form))
  (let ((form (%dsl-head form)))
    (if (eq (first form) 'variant)
        (list* (first form)
               (if (consp (second form)) (%dsl-head (second form)) (second form))
               (%dsl-heads-in (cddr form)))
        form)))

(defun %dsl-instruction-clause (clause)
  (unless (consp clause) (return-from %dsl-instruction-clause clause))
  (let ((clause (%dsl-head clause)))
    (case (first clause)
      (modes (cons (first clause)
                   (mapcar (lambda (mode)
                             (if (consp mode)
                                 (cons (first mode) (mapcar #'%dsl-modes-subclause (rest mode)))
                                 mode))
                           (rest clause))))
      (encoding (cons (first clause) (mapcar #'%dsl-encoding-subclause (rest clause))))
      (t clause))))

(defun %dsl-modes-subclause (subclause)
  (unless (consp subclause) (return-from %dsl-modes-subclause subclause))
  (let ((subclause (%dsl-head subclause)))
    (if (member (first subclause) '(semantics cycles))
        subclause
        (%dsl-encoding-subclause subclause))))
