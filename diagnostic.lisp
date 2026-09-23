;;;; diagnostic.lisp
;;;; Shared diagnostic-rendering mechanism for the three program-source
;;;; pipeline stages -- lexing (lexer.lisp), parsing (parser.lisp) and
;;;; assembly (assembler.lisp) -- plus the opt-in strict operand-range check
;;;; and the mode-selection ambiguity warning (#74).
;;;;
;;;; LASM-SYNTAX-ERROR (moved here from storage.lisp, where it used to sit
;;;; beside the storage conditions with no renderer of its own) already
;;;; carried a MESSAGE/LINE/COLUMN -- every stage already reports *where*.
;;;; What was missing was rendering that position against the actual source
;;;; line, and naming *what* was found alongside what was expected. Both are
;;;; additive: DIAGNOSTIC-TEXT below degrades to the old one-line report when
;;;; there is no source text or no column, so nothing that already prints a
;;;; LASM-SYNTAX-ERROR changes shape unless SOURCE is available.
;;;;
;;;; A SOURCE slot on the condition itself (not a dynamic variable) is what
;;;; lets a condition escaping ASSEMBLE's dynamic extent -- caught later, by
;;;; a different caller, in a different stack frame -- still render with
;;;; context: WITH-SOURCE-CONTEXT (below) fills the slot via HANDLER-BIND
;;;; and *declines*, so the condition keeps its original identity and
;;;; restarts intact; it isn't wrapped or resignalled.

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

SOURCE, when given, overrides the condition's own SOURCE slot (normally
filled by WITH-SOURCE-CONTEXT at the pipeline's entry points) -- useful for
re-rendering a condition caught with different or additional context.
Degrades to a bare \"message (line N, column C)\" one-liner when there is no
line at all, and omits the caret line when there is a line but no column."
  (let* ((message (lasm-syntax-error-message condition))
         (line (lasm-syntax-error-line condition))
         (column (lasm-syntax-error-column condition))
         (src (if source-supplied-p source (lasm-syntax-error-source condition)))
         (src-line (and src line (%nth-source-line src line)))
         (definition-line (lasm-syntax-error-definition-line condition)))
    (with-output-to-string (out)
      (if line
          (format out "line ~D~@[, column ~D~]: ~A" line column message)
          (format out "~A" message))
      (when src-line
        (let* ((label (format nil "~D" line))
               (gutter (make-string (length label) :initial-element #\Space)))
          (format out "~%~A | ~A" label src-line)
          (when column
            (format out "~%~A | ~A^" gutter
                    (make-string (max 0 (1- column)) :initial-element #\Space)))))
      (when definition-line
        (format out "~%expanded from macro body line ~D" definition-line)
        (let ((definition-text (and src (%nth-source-line src definition-line))))
          (when definition-text
            (format out "~%~D | ~A" definition-line definition-text)))))))

;;; Conditions

;; Named LASM-SYNTAX-ERROR rather than PARSE-ERROR because CL:PARSE-ERROR is
;; a standard condition type and this package :USEs #:CL. Moved here from
;; storage.lisp (#74) so the condition and its renderer live together.
(define-condition lasm-syntax-error (lasm-error)
  ((message :initarg :message :initform nil :reader lasm-syntax-error-message)
   (line :initarg :line :initform nil :accessor lasm-syntax-error-line)
   (column :initarg :column :initform nil :accessor lasm-syntax-error-column)
   (definition-line :initarg :definition-line :initform nil
                    :accessor lasm-syntax-error-definition-line)
   ;; #74: filled in place by WITH-SOURCE-CONTEXT, not passed as an initarg
   ;; at signal time -- the signalling call site (lexer.lisp, parser.lisp,
   ;; mode.lisp, assembler.lisp) never has the whole source text in hand,
   ;; only the entry point does.
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
