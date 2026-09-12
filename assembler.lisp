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
;;;;
;;;; Directives (directive.lisp, #14) are dispatched in %LAYOUT before a
;;;; mnemonic reaches FIND-INSTRUCTION-VARIANTS (which signals on an
;;;; unregistered name, so directive lookup can't be a fallback after that
;;;; call). .ORG (a DIRECTIVE-DESCRIPTOR with ACTION :SET-ORIGIN) moves the
;;;; address counter -- and, if no statement has occupied any address yet,
;;;; the assembly's own ORIGIN -- rather than sizing a statement; .BYTE/.WORD
;;;; (:EMIT) and .RES (:RESERVE) size a statement like an instruction does,
;;;; so pass 1 still sizes everything and pass 2 still only evaluates
;;;; operands against the completed symbol table. Because a directive
;;;; statement can leave a gap (a forward .ORG, or plain non-contiguous
;;;; layout) or contribute no bytes at all (.ORG itself), %ENCODE below
;;;; writes each entry at its own address into a growable byte vector
;;;; instead of accumulating a flat list.

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
2. Value -- for a variant whose mode holes fold to label-free constants,
   keep it only if every value fits its own hole's operand width; if none of
   the syntax-matching variants fit, fall back to the widest one (by total
   operand width) and let ENCODE-INSTRUCTION's existing WRAP-VALUE mask each
   value, exactly as a single-mode M1 instruction always did. If any hole is
   a label reference (value not yet known), that variant is excluded from
   the value filter and only considered as the widest fallback -- it never
   has to shrink once the label resolves, so this needs no relaxation loop.
   Ties, in both cases, keep declaration order. Resolvedness is checked per
   candidate, not once for all of them: two variants of one mnemonic can
   have different hole counts (e.g. a two-register mode alongside a
   one-immediate mode), so whether their holes resolve is not the same
   question for each.

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
    ;; STABLE-SORT, not SORT: ties (equal total width) must keep declaration
    ;; order.
    (let* ((by-width (stable-sort (copy-list candidates) #'>
                                   :key (lambda (c) (instruction-descriptor-total-operand-width
                                                      (first c)))))
           (widest (first by-width))
           (fitting (find-if (lambda (c)
                                (let* ((mode (instruction-descriptor-mode (first c))))
                                  (and
                                   ;; A RELATIVE candidate's value is an
                                   ;; absolute target, not the encoded offset
                                   ;; (that's computed later, in %ENCODE,
                                   ;; once every address is known) --
                                   ;; checking it against an operand width
                                   ;; here would compare the wrong quantity.
                                   ;; Excluded from the fit filter so the
                                   ;; widest candidate is always chosen, same
                                   ;; as a label reference (#23; only matters
                                   ;; once a mnemonic declares RELATIVE
                                   ;; alongside another mode, see #27).
                                   (not (and mode (mode-descriptor-relativep mode)))
                                   (handler-case
                                       (let ((widths (instruction-descriptor-operand-widths
                                                      (first c)))
                                             (vals (mapcar #'eval-expr-constant (second c))))
                                         (every #'%fits-width-p vals widths))
                                     (unresolved-label () nil)))))
                              candidates)))
      (values-list (or fitting widest)))))

;;; Directives (directive.lisp, #14) -- operand parsing and argument folding

(defun %directive-operand-ast (operand)
  "Parse OPERAND's (parser.lisp) TOKENS as a single expression, signalling
PARSE-FAILURE if they don't consume the whole operand -- a directive
operand is always one bare expression, never an addressing-mode pattern."
  (let ((tokens (operand-tokens operand)))
    (multiple-value-bind (ast next-i) (parse-expression tokens)
      (unless (= next-i (length tokens))
        (%parse-error (aref tokens next-i) "Unexpected trailing token in directive operand"))
      ast)))

(defun %directive-args (statement directive)
  "Parse STATEMENT's operands (parser.lisp) into a list of EXPR-* ASTs, one
per operand, after checking their count against DIRECTIVE's arity: exactly
one for a (:FIXED 1) directive (e.g. .ORG, .RES), any number (zero
included) for a :VARIADIC one (e.g. .BYTE, .WORD)."
  (let ((operands (statement-operands statement)))
    (let ((arity (directive-descriptor-arity directive)))
      (unless (eq arity :variadic)
        (destructuring-bind (kind n) arity
          (declare (ignore kind))
          (unless (= (length operands) n)
            (%assembly-error (statement-line statement)
                              "~A: expected ~D operand~:P, got ~D"
                              (statement-mnemonic statement) n (length operands))))))
    (mapcar #'%directive-operand-ast operands)))

(defun %directive-constant-arg (statement directive)
  "Fold DIRECTIVE's single argument (STATEMENT's one operand) to a
label-free constant -- used for .ORG and .RES, whose size/address effect
must be known in pass 1, before any label has resolved. Signals
ASSEMBLY-ERROR (not the bare UNRESOLVED-LABEL EVAL-EXPR-CONSTANT itself
signals) naming the offending directive, since a plain \"unresolved label\"
report wouldn't say why this one operand can't wait for pass 2."
  (let ((ast (first (%directive-args statement directive))))
    (handler-case (eval-expr-constant ast)
      (unresolved-label (c)
        (%assembly-error (statement-line statement)
                          "~A: operand must be a constant expression -- label ~S ~
is not resolvable here (pass 1 has no symbol table yet)"
                          (statement-mnemonic statement) (unresolved-label-name c))))))

(defun %apply-origin-directive (statement directive address asm-origin emitted-p)
  "Apply a :SET-ORIGIN directive (.ORG) at layout time. Returns (VALUES
new-address new-asm-origin): before any statement has occupied an address
(EMITTED-P NIL), .ORG moves both the address counter and the assembly's own
ORIGIN (so a leading .ORG places the whole program, and LOAD-PROGRAM's
ASSEMBLY-ORIGIN default, emulator.lisp, lands it there); afterwards it only
pads forward -- a backward move is ambiguous (overwrite? truncate?) so it
signals ASSEMBLY-ERROR instead of guessing."
  (let ((value (%directive-constant-arg statement directive)))
    (cond
      ((not emitted-p) (values value value))
      ((>= value address) (values value asm-origin))
      (t (%assembly-error (statement-line statement)
                           ".org cannot move the address counter backward (from ~D to ~D)"
                           address value)))))

(defun %bind-label! (statement symbols address)
  (when (statement-label statement)
    (when (nth-value 1 (gethash (statement-label statement) symbols))
      (%assembly-error (statement-line statement)
                        "Duplicate label ~S" (statement-label statement)))
    (setf (gethash (statement-label statement) symbols) address)))

(defun %layout (statements machine origin)
  "Returns (VALUES symbols sized-entries final-address asm-origin). SYMBOLS
is a string -> address hash table. SIZED-ENTRIES is, in order, one tagged
entry per mnemonic-bearing statement that occupies address space:
  (:instruction address descriptor asts line)
  (:emit        address width asts line)
  (:reserve     address count line)
-- %ENCODE below dispatches on the leading keyword. A .ORG statement
\(directive.lisp\) contributes no entry -- it only moves the address counter
\(and, before anything else has been laid out, ASM-ORIGIN -- see
%APPLY-ORIGIN-DIRECTIVE\). FINAL-ADDRESS is the address counter's value
after the last statement, i.e. ORIGIN/ASM-ORIGIN plus the assembled size;
ASM-ORIGIN is ORIGIN unless a leading .ORG moved it."
  (let ((symbols (make-hash-table :test 'equal))
        (address origin)
        (asm-origin origin)
        (emitted-p nil)
        sized)
    (dolist (statement statements)
      (let* ((mnemonic (statement-mnemonic statement))
             (directive (and mnemonic (find-directive-descriptor mnemonic))))
        (cond
          ;; .ORG binds this statement's own label (if any) to the address
          ;; it moves *to*, not the address before the move -- so
          ;; "foo: .org $8000" binds FOO to $8000.
          ((and directive (eq (directive-descriptor-action directive) :set-origin))
           (multiple-value-bind (new-address new-origin)
               (%apply-origin-directive statement directive address asm-origin emitted-p)
             (setf address new-address asm-origin new-origin))
           (%bind-label! statement symbols address))
          (t
           (%bind-label! statement symbols address)
           (when mnemonic
             (cond
               (directive
                (ecase (directive-descriptor-action directive)
                  (:reserve
                   (let ((count (%directive-constant-arg statement directive)))
                     (when (minusp count)
                       (%assembly-error (statement-line statement)
                                        "~A: count must not be negative" mnemonic))
                     (cl:push (list :reserve address count (statement-line statement)) sized)
                     (incf address count)
                     (setf emitted-p t)))
                  (:emit
                   (let* ((asts (%directive-args statement directive))
                          (width (directive-descriptor-width directive)))
                     (cl:push (list :emit address width asts (statement-line statement)) sized)
                     (incf address (* width (length asts)))
                     (setf emitted-p t)))))
               (t
                (let ((variants (find-instruction-variants machine mnemonic)))
                  (multiple-value-bind (descriptor asts) (%choose-variant statement variants)
                    (cl:push (list :instruction address descriptor asts (statement-line statement)) sized)
                    (incf address (1+ (instruction-descriptor-total-operand-width descriptor)))
                    (setf emitted-p t))))))))))
    (values symbols (nreverse sized) address asm-origin)))

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
  (let* ((width (instruction-descriptor-total-operand-width descriptor))
         (next-address (+ address 1 width))
         (offset (- value next-address)))
    (unless (%fits-signed-width-p offset width)
      (%assembly-error line
                        "~A: relative branch offset ~D out of range for ~D-byte operand ~
(must be between ~D and ~D)"
                        (instruction-descriptor-name descriptor) offset width
                        (- (ash 1 (1- (* 8 width)))) (1- (ash 1 (1- (* 8 width))))))
    offset))

(defun %make-growable-bytes (size)
  (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer size
                    :initial-element 0))

(defun %ensure-bytes-length (bytes n)
  "Grow the adjustable vector BYTES (%MAKE-GROWABLE-BYTES) to at least N
elements, zero-filling the new tail -- a directive statement can leave a
gap (a forward .ORG, #14) that no earlier entry ever writes, so the
accumulator can't be a flat push-then-reverse list the way M1/M2's
contiguous instruction stream could."
  (when (> n (length bytes))
    (adjust-array bytes n :fill-pointer n :initial-element 0))
  bytes)

(defun %encode (sized-entries symbols origin final-address)
  "Evaluate SIZED-ENTRIES (%LAYOUT's tagged output) against the completed
symbol table SYMBOLS and write each entry's bytes at its own address (minus
ORIGIN) into a byte vector sized to FINAL-ADDRESS - ORIGIN. A gap between
entries -- a forward .ORG, or a .RESERVE's run -- is left zero-filled by
%ENSURE-BYTES-LENGTH's growth rather than written explicitly."
  (let ((bytes (%make-growable-bytes (max 0 (- final-address origin)))))
    (dolist (entry sized-entries)
      (ecase (first entry)
        (:instruction
         (destructuring-bind (kind address descriptor asts line) entry
           (declare (ignore kind))
           (let* ((mode (instruction-descriptor-mode descriptor))
                  (values (mapcar (lambda (ast) (eval-expr ast :symbols symbols)) asts)))
             (when (and mode (mode-descriptor-relativep mode))
               ;; %CHECK-RELATIVE-MODE-HOLES (instruction.lisp) guarantees a
               ;; RELATIVE mode has exactly one hole, so VALUES here is
               ;; always a single-element list.
               (setf values (list (%relative-offset address descriptor (first values) line))))
             (loop with i = (- address origin)
                   for byte in (encode-instruction descriptor values)
                   do (setf (aref bytes i) byte) (incf i)))))
        (:emit
         (destructuring-bind (kind address width asts line) entry
           (declare (ignore kind line))
           (loop with i = (- address origin)
                 for ast in asts
                 do (dolist (byte (%encode-value-bytes (eval-expr ast :symbols symbols) width))
                      (setf (aref bytes i) byte) (incf i)))))
        (:reserve
         ;; Zero-filled -- %MAKE-GROWABLE-BYTES/%ENSURE-BYTES-LENGTH already
         ;; zero-initialize every element, so there is nothing to write here
         ;; beyond making sure the run is covered (relevant when a .RESERVE
         ;; is the very last statement, so no later write grows the vector
         ;; past it).
         (destructuring-bind (kind address count line) entry
           (declare (ignore kind line))
           (%ensure-bytes-length bytes (- (+ address count) origin))))))
    (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes)))

;;; Entry points

(defun assemble-statements (statements &key machine (origin 0))
  "Assemble a STATEMENT list (parser.lisp) targeting MACHINE into an
ASSEMBLY. Signals ASSEMBLY-ERROR on a duplicate label, an operand matching
no addressing mode, a malformed or backward-moving directive (#14, e.g.
.ORG with a label operand or one that moves the address counter
backward), UNKNOWN-INSTRUCTION on an unregistered mnemonic, and
UNRESOLVED-LABEL (via EVAL-EXPR) on a reference to a label that is never
defined anywhere in STATEMENTS. ORIGIN is the assembly's starting address
unless a leading .ORG (before any other statement occupies an address)
moves it -- see ASSEMBLY-ORIGIN."
  (multiple-value-bind (symbols sized final-address asm-origin) (%layout statements machine origin)
    (make-assembly :bytes (%encode sized symbols asm-origin final-address)
                   :origin asm-origin :symbols symbols)))

(defun assemble (source &key machine (lexer 'default) (origin 0))
  "Tokenize and parse SOURCE with LEXER (lexer.lisp/parser.lisp), then
ASSEMBLE-STATEMENTS the result targeting MACHINE. See ASSEMBLE-STATEMENTS
for the conditions this can signal, plus LEX-ERROR/PARSE-FAILURE from the
front end."
  (assemble-statements (parse source :lexer lexer) :machine machine :origin origin))
