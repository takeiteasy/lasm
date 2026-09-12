;;;; assembler.lisp
;;;; The M2 assembler pass: turns a STATEMENT list (parser.lisp) into encoded
;;;; bytes, resolving labels and selecting an addressing mode along the way.
;;;;
;;;; Two passes: pass 1 (layout, %LAYOUT below) walks the statement list once,
;;;; binding every label to an address and choosing each instruction's
;;;; addressing-mode variant (mode.lisp/instruction.lisp) so it can size the
;;;; statement; pass 2 (encode, %ENCODE) evaluates operands against the
;;;; completed symbol table and emits bytes. This is what buys forward
;;;; references (`jmp end` ... `end:`) for free.
;;;;
;;;; M1 sized every statement without looking at its operand at all, since it
;;;; allowed only one mode per instruction. M2 modes can overlap in syntax
;;;; (zero-page and absolute both match a bare `expr`) and differ in size, so
;;;; pass 1 now also selects a mode per statement -- see %CHOOSE-VARIANT.
;;;; Ticket #18 and LASM-plan.md sec. 2 describe M2's two-pass structure as
;;;; "label resolution before final mode/encoding selection"; this does the
;;;; reverse (mode selection in pass 1, labels resolved in pass 2) because
;;;; the literal order needs a relaxation loop -- start every label-bearing
;;;; operand at its narrowest legal mode, re-run layout until addresses stop
;;;; moving. Follow-up ticket: narrow a label-bearing operand's mode once
;;;; layout has converged; %CHOOSE-VARIANT picks the widest legal mode for a
;;;; label-bearing operand instead, which is never invalidated by a later
;;;; layout, so one pass suffices and there is no non-convergence case to
;;;; guard against.
;;;;
;;;; Local labels (a name starting with a non-alphanumeric prefix char, e.g.
;;;; ".loop") are NOT scoped to an enclosing global label in M1/M2 -- they are
;;;; ordinary global names, sharing one flat symbol table with everything
;;;; else. Binding a local label to its enclosing label is a separate ticket
;;;; (#16).

(in-package #:lasm)

;;; Conditions

(define-condition assembly-error (lasm-syntax-error) ()
  (:documentation "Signalled by ASSEMBLE-STATEMENTS on a malformed program:
a duplicate label, or an operand that matches no addressing mode any variant
of the instruction declares. Undefined labels are not this condition -- they
surface as UNRESOLVED-LABEL from EVAL-EXPR, since that condition already
names exactly this failure."))

(defun %assembly-error (line fmt &rest args)
  (error 'assembly-error :message (apply #'format nil fmt args) :line line))

;;; Result

(defstruct assembly
  (bytes nil :type (or null (vector (unsigned-byte 8))))
  (origin 0 :type (integer 0))
  (symbols nil :type (or null hash-table)))  ; string -> address

;;; Pass 1: layout -- size every statement, bind every label, choose modes

(defun %fits-width-p (value width)
  "T if VALUE (a folded constant) fits in WIDTH bytes, either as an unsigned
or a two's-complement signed value -- e.g. both 255 and -1 fit one byte, so a
signed operand like a lo/hi-masked value isn't rejected just because it folds
negative. Accepts the full unsigned range too, so this is NOT the right
predicate for a RELATIVE branch offset (#23) -- see %FITS-SIGNED-WIDTH-P."
  (and (>= value (- (ash 1 (1- (* 8 width)))))
       (< value (ash 1 (* 8 width)))))

(defun %fits-signed-width-p (value width)
  "T if VALUE fits as a two's-complement signed WIDTH-byte integer, i.e.
-(2^(8*width-1)) <= VALUE < 2^(8*width-1). Unlike %FITS-WIDTH-P, this
rejects the unsigned-only range (e.g. +200 does not fit one byte) -- used to
range-check a RELATIVE mode's offset (#23), where wrapping silently instead
of erroring would branch to the wrong address."
  (let ((bound (ash 1 (1- (* 8 width)))))
    (and (>= value (- bound)) (< value bound))))

(defun %choose-variant (statement variants)
  "Pick which of a mnemonic's VARIANTS (instruction-descriptor list,
instruction.lisp) STATEMENT's operand tokens select, and the parsed hole ASTs
for that variant's mode. Two filters, applied in VARIANTS' declaration
order -- so an author should declare narrower/more specific modes before
wider ones that also match their syntax (e.g. zero-page before absolute):

1. Syntax -- keep variants whose mode's pattern matches the operand tokens
   (a no-operand variant's \"pattern\" is simply an empty token run). No
   match at all is an ASSEMBLY-ERROR.
2. Value -- for a variant whose mode holes fold to a label-free constant,
   keep it only if the value fits its operand width; if none of the
   syntax-matching variants fit, fall back to the widest one and let
   ENCODE-INSTRUCTION's existing WRAP-VALUE mask the value, exactly as a
   single-mode M1 instruction always did. If any hole is a label reference
   (value not yet known), pick the *widest* syntax-matching variant instead
   -- it never has to shrink once the label resolves, so this needs no
   relaxation loop. Ties, in both cases, keep declaration order.

Returns (VALUES chosen-descriptor hole-asts)."
  (let* ((tokens (statement-operand-tokens statement))
         (candidates
           (loop for v in variants
                 for mode = (instruction-descriptor-mode v)
                 for (asts okp) = (multiple-value-list
                                    (if mode
                                        (try-match-operand-mode tokens mode)
                                        (values nil (zerop (length tokens)))))
                 when okp collect (list v asts))))
    (when (null candidates)
      (%assembly-error (statement-line statement)
                        "~A: no addressing mode matches this operand"
                        (statement-mnemonic statement)))
    ;; STABLE-SORT, not SORT: ties (equal width) must keep declaration order.
    (let* ((by-width (stable-sort (copy-list candidates) #'>
                                   :key (lambda (c) (or (instruction-descriptor-operand-width
                                                          (first c))
                                                         0))))
           (widest (first by-width))
           (first-mode (instruction-descriptor-mode (first (first candidates))))
           (resolvedp (and
                       ;; A RELATIVE candidate's value is an absolute target,
                       ;; not the encoded offset (that's computed later, in
                       ;; %ENCODE, once every address is known) -- checking it
                       ;; against an operand width here would compare the
                       ;; wrong quantity. Treat it as unresolved so the widest
                       ;; candidate is always chosen, same as a label
                       ;; reference (#23; only matters once a mnemonic
                       ;; declares RELATIVE alongside another mode, see #27).
                       (not (and first-mode (mode-descriptor-relativep first-mode)))
                       (handler-case (progn (mapcar #'eval-expr-constant (second (first candidates)))
                                            t)
                        ;; All candidates share the same operand syntax (just
                        ;; different widths/modes), so whether the value
                        ;; resolves is the same for every candidate -- check
                        ;; once against the first.
                        (unresolved-label () nil)))))
      (if (not resolvedp)
          (values-list widest)
          (let ((fitting (find-if (lambda (c)
                                     (let* ((width (or (instruction-descriptor-operand-width (first c)) 1))
                                            (vals (mapcar #'eval-expr-constant (second c))))
                                       (every (lambda (v) (%fits-width-p v width)) vals)))
                                   candidates)))
            (values-list (or fitting widest)))))))

(defun %layout (statements machine origin)
  "Returns (VALUES symbols sized-statements) where SYMBOLS is a string ->
address hash table and SIZED-STATEMENTS pairs each mnemonic-bearing
statement with its address, chosen INSTRUCTION-DESCRIPTOR, parsed operand
hole ASTs, and source line (the line is carried through for %ENCODE's
RELATIVE-mode range-check error, #23), in order."
  (let ((symbols (make-hash-table :test 'equal))
        (address origin)
        sized)
    (dolist (statement statements)
      (when (statement-label statement)
        (when (nth-value 1 (gethash (statement-label statement) symbols))
          (%assembly-error (statement-line statement)
                            "Duplicate label ~S" (statement-label statement)))
        (setf (gethash (statement-label statement) symbols) address))
      (when (statement-mnemonic statement)
        (let ((variants (find-instruction-variants machine (statement-mnemonic statement))))
          (multiple-value-bind (descriptor asts) (%choose-variant statement variants)
            (cl:push (list address descriptor asts (statement-line statement)) sized)
            (incf address (1+ (or (instruction-descriptor-operand-width descriptor) 0)))))))
    (values symbols (nreverse sized))))

;;; Pass 2: encode -- evaluate operands against the completed symbol table

(defun %relative-offset (address descriptor value line)
  "VALUE is the absolute target address a RELATIVE-mode operand (mode.lisp)
folded to; ADDRESS is this instruction's own address and DESCRIPTOR its
chosen INSTRUCTION-DESCRIPTOR. Returns the signed offset to encode, computed
from the address of the *next* instruction -- STEP-MACHINE (emulator.lisp)
advances PC past the whole instruction before running its semantics, so that
is the base a branch's own (set! pc (+ pc operand)) actually adds to.
Signals ASSEMBLY-ERROR if the offset doesn't fit the operand's width, rather
than silently wrapping to a branch at the wrong address (#23)."
  (let* ((width (or (instruction-descriptor-operand-width descriptor) 1))
         (next-address (+ address 1 width))
         (offset (- value next-address)))
    (unless (%fits-signed-width-p offset width)
      (%assembly-error line
                        "~A: relative branch offset ~D out of range for ~D-byte operand ~
(must be between ~D and ~D)"
                        (instruction-descriptor-name descriptor) offset width
                        (- (ash 1 (1- (* 8 width)))) (1- (ash 1 (1- (* 8 width))))))
    offset))

(defun %encode (sized-statements symbols)
  (let (bytes)
    (dolist (entry sized-statements)
      (destructuring-bind (address descriptor asts line) entry
        (let* ((mode (instruction-descriptor-mode descriptor))
               (value (when mode (eval-expr (first asts) :symbols symbols))))
          (when (and mode (mode-descriptor-relativep mode))
            (setf value (%relative-offset address descriptor value line)))
          (dolist (byte (encode-instruction descriptor value))
            (cl:push byte bytes)))))
    (coerce (nreverse bytes) '(vector (unsigned-byte 8)))))

;;; Entry points

(defun assemble-statements (statements &key machine (origin 0))
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Signals ASSEMBLY-ERROR on a duplicate label or an operand matching
no addressing mode, UNKNOWN-INSTRUCTION on an unregistered mnemonic, and
UNRESOLVED-LABEL (via EVAL-EXPR) on a reference to a label that is never
defined anywhere in STATEMENTS."
  (multiple-value-bind (symbols sized) (%layout statements machine origin)
    (make-assembly :bytes (%encode sized symbols) :origin origin :symbols symbols)))

(defun assemble (source &key machine (lexer 'default) (origin 0))
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end."
  (assemble-statements (parse source :lexer lexer) :machine machine :origin origin))
