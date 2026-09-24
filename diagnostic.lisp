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
;; storage.lisp (#74) so the condition and its renderer live together.
;;; Definition errors: a malformed DEFMACHINE/DEFINSTRUCTION/DEFMODE/DEFLEXER/
;;; DEFDIRECTIVE form, as opposed to a program's own source (LASM-SYNTAX-ERROR).

(defvar *definition-name* nil
  "Name of the definition being built, recorded on DEFINITION-ERRORs.")

(defvar *last-definition-error* nil
  "The last DEFINITION-ERROR signalled; lets a caller of COMPILE re-signal one
SBCL turned into a COMPILED-PROGRAM-ERROR.")

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
    (setf *last-definition-error* condition)
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

;; #74: a warning, not an error -- assembly continues once WARN returns
;; (SBCL's default handler prints and resumes). Signalled by ASSEMBLER.LISP's
;; %CHOOSE-VARIANT when two or more syntax-matching candidates of an
;; instruction have the *same* total operand width, so width-based
;; relaxation cannot recover and declaration order alone decides -- the one
;; case "encoding ambiguity" can mean without contradicting the documented
;; declaration-order tiebreak (docs/assembler.md). Never fires on a pair like
;; ZERO-PAGE/ABSOLUTE, whose widths differ.
(define-condition lasm-warning (warning)
  ((message :initarg :message :initform nil :reader lasm-warning-message)
   (line :initarg :line :initform nil :reader lasm-warning-line))
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

;;; Strict operand range (#74, absorbing #28 and #43)

(defvar *strict-operand-range* nil
  "When bound to T, every instruction operand is range-checked at encode
time against its addressing mode's own width -- an out-of-range value
signals ASSEMBLY-ERROR instead of being masked by WRAP-VALUE. Default NIL
preserves #28/#43's original wrap-on-overflow behavior (a fantasy CPU may
define wraparound as intended). A single addressing mode can opt in without
this global switch via DEFMODE's :STRICT T (mode.lisp) -- see %ENCODE's
:INSTRUCTION branch (assembler.lisp) for where both are checked. This global
switch is the only way to cover an instruction with no addressing mode at
all (a bare (operand :width n) M1-style encoding), since :STRICT lives on a
MODE-DESCRIPTOR.")
