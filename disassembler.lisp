;;;; disassembler.lisp
;;;; #21 (M7): a disassembler derived from the same DEFINSTRUCTION specs used
;;;; for assembly, built on DECODE-INSTRUCTION-AT (decoder.lisp) -- the same
;;;; pure decode step STEP-MACHINE (emulator.lisp) uses, so this cannot
;;;; decode an encoding differently than the emulator does.
;;;;
;;;; No function here is named DISASSEMBLE: CL:DISASSEMBLE is already in
;;;; scope via (:USE #:CL), and this package shadows only PUSH/POP
;;;; (package.lisp) as a last resort -- a third shadow isn't warranted, so
;;;; the entry points below are named DISASSEMBLE-CELLS/-ASSEMBLY/-MEMORY
;;;; instead.
;;;;
;;;; Three-stage pipeline: (1) %DISASSEMBLE-RAW-LINES walks a READ-CELL
;;;; closure (decoder.lisp) from ORIGIN to END, calling DECODE-INSTRUCTION-AT
;;;; at each address and collecting one DISASSEMBLY-LINE per instruction or
;;;; undecodable cell, with no text rendered yet; (2) %LINE-STARTS/
;;;; %REVERSE-SYMBOLS compute, from the finished line list, which addresses
;;;; are real instruction boundaries a label may legitimately point at; (3)
;;;; %RENDER-LINES! renders each line's TEXT and, when a symbol table was
;;;; given, its LABEL. Splitting label resolution into its own post-pass
;;;; (rather than resolving inline during the walk) is what lets an operand
;;;; naming a *later* address still render as a label -- ordinary forward
;;;; references, the common case.
;;;;
;;;; DECODE FAILURE, mid-stream: rather than stopping at the first
;;;; undecodable cell (which would make this useless on any program with a
;;;; trailing .BYTE data table -- the common case, not the corner case),
;;;; %DISASSEMBLE-RAW-LINES emits a one-cell data line (".byte $XX") and
;;;; advances by exactly one cell, for both failure shapes DECODE-INSTRUCTION-AT
;;;; can produce -- an unregistered opcode/unmatched word field
;;;; (:DECODE-FAILURE) and a condition signalled by READ-CELL itself off the
;;;; end of the buffer (ADDRESS-OUT-OF-RANGE, from a truncated trailing
;;;; instruction). A word-encoded machine could instead consolidate an
;;;; unmatched-field failure into one WIDTH-CELLS-wide ".word" line (the
;;;; whole word was already read successfully to reach that failure) -- left
;;;; as one-cell-at-a-time for both failure shapes uniformly, since a
;;;; genuinely truncated buffer cannot safely assume WIDTH-CELLS more cells
;;;; are readable, and a single fallback path is less to get wrong than two.
;;;; The resulting cells are still exactly re-assemblable, just as several
;;;; ".byte" lines rather than one ".word" line.
;;;;
;;;; DATA REGIONS (#82): a caller may declare (START . END) cell ranges, END
;;;; exclusive, as data. Every cell inside one renders as a ".byte" line with
;;;; no decode attempted. A decode that succeeds but would run into a region
;;;; is discarded for a ".byte" line as well, so an instruction never spans
;;;; the boundary. DISASSEMBLE-ASSEMBLY derives its regions from the
;;;; assembly's own .byte/.word/.res statements, so this straddle case only
;;;; arises for hand-declared regions.
;;;;
;;;; ROUND-TRIP FIDELITY -- the honest scope (see docs/disassembler.md):
;;;; ASSEMBLE -> DISASSEMBLE-* -> ASSEMBLE reproduces identical cells when
;;;; the input came from ASSEMBLE on the same machine, with :LABELS NIL and
;;;; :SUFFIXES T. It does NOT generally hold for: (a) a word-encoded
;;;; machine's bytes that spent an extra word on a value that would fit
;;;; inline -- %CHOOSE-VARIANT (assembler.lisp) always picks the narrowest
;;;; fit and there is no forced-variant syntax for a word machine's
;;;; inline-vs-extra-word choice (unlike a byte machine's mode :SUFFIX); (b)
;;;; a program with forward references, whose original assembly may have
;;;; settled on a wider encoding than a from-scratch pass over the final
;;;; values would pick (%CHOOSE-VARIANT's relaxation floor is monotone
;;;; across layout passes) -- re-assembling disassembled text re-runs layout
;;;; from scratch and can legitimately produce fewer cells; (c) comments,
;;;; macros, .EQU names, label names absent from :SYMBOLS, code/data
;;;; boundaries (unless declared, see DATA REGIONS above), and
;;;; original number radix/formatting -- none of these survive encoding at
;;;; all, so none can be recovered.

(in-package #:lasm)

;;; Disassembly line

(defstruct disassembly-line
  (address 0 :type (integer 0))
  (size 1 :type (integer 1))
  (cells nil :type list)                                   ; raw cells consumed, in address order
  (descriptor nil :type (or null instruction-descriptor))   ; NIL = undecodable data
  (values nil :type list)                                  ; decoded operand values, hole order
  ;; DECODE-INSTRUCTION-AT's matched per-hole record, hole order -- a
  ;; WORD-FIELD-CHOICE on a word-encoded machine (#104), the descriptor's own
  ;; SUB-CHOICES bare mode-name symbols on a byte-encoded one with a
  ;; hole-selected sub-opcode (#126), or NIL throughout for undecodable data
  ;; or a descriptor with no such selector, same as VALUES.
  ;; %RENDER-OPERAND-TEXT reads a hole's own matched ONE-OF alternative (via
  ;; %MATCHED-CHOICE-NAME, instruction.lisp, which normalizes either shape)
  ;; to render that alternative's own syntax instead of always a ONE-OF's
  ;; first one -- see #117/#126.
   (choices nil :type list)
  (choice-selections nil :type list)
  (label nil :type (or null string))                        ; a symbol bound to this address, or NIL
  (text nil :type (or null string)))                        ; rendered source line, sans label

;;; Stage 1: decode

(defun %normalize-data-regions (regions)
  "REGIONS, a list of (START . END) cell ranges (END exclusive), validated and
returned sorted by START with overlapping or adjacent ranges merged."
  (let ((sorted (sort (mapcar (lambda (r)
                                (unless (and (consp r) (typep (car r) '(integer 0)) (typep (cdr r) '(integer 0))
                                             (< (car r) (cdr r)))
                                  (error "DISASSEMBLE: data region ~S is not (START . END) with 0 <= START < END" r))
                                (cons (car r) (cdr r)))
                              regions)
                      #'< :key #'car))
        merged)
    (dolist (r sorted)
      (if (and merged (<= (car r) (cdr (first merged))))
          (setf (cdr (first merged)) (max (cdr r) (cdr (first merged))))
          (cl:push r merged)))
    (nreverse merged)))

(defun %disassemble-raw-lines (read-cell origin end machine-name memory &optional data-regions)
  "Walk READ-CELL from ORIGIN to END (exclusive), decoding one instruction
at a time via DECODE-INSTRUCTION-AT (decoder.lisp) and collecting one
DISASSEMBLY-LINE per instruction or undecodable cell -- see this file's
header comment for the mid-stream decode-failure and data-region policies.
TEXT and LABEL are left NIL; %RENDER-LINES! fills them in once every line's
address is known."
  (let ((regions (%normalize-data-regions data-regions))
        lines)
    (flet ((data-line (address)
             "Emit a one-cell data line at ADDRESS; NIL when it is unreadable."
             (multiple-value-bind (cell okp)
                 (handler-case (values (funcall read-cell address) t)
                   (address-out-of-range () (values nil nil)))
               (when okp
                 (cl:push (make-disassembly-line :address address :size 1 :cells (list cell)) lines))
               okp)))
      (loop with address = origin
            while (< address end)
            do (loop while (and regions (<= (cdr (first regions)) address))
                     do (cl:pop regions))
               (let ((region-start (and regions (car (first regions)))))
                 (if (and region-start (<= region-start address))
                     (if (data-line address) (incf address) (setf address end))
                         (multiple-value-bind (descriptor values size choices choice-selections)
                         (handler-case (decode-instruction-at read-cell address machine-name :memory memory)
                           (address-out-of-range () (values :decode-failure nil nil nil)))
                       (cond
                         ((or (eq descriptor :decode-failure)
                              (and region-start (> (+ address size) region-start)))
                          ;; ADDRESS itself unreadable (not merely a later cell
                          ;; of a truncated multi-cell instruction): nothing
                          ;; more to disassemble.
                          (if (data-line address) (incf address) (setf address end)))
                         (t
                          (let ((cells (loop for i below size collect (funcall read-cell (+ address i)))))
                            (cl:push (make-disassembly-line :address address :size size :cells cells
                                                              :descriptor descriptor :values values :choices choices
                                                              :choice-selections choice-selections)
                                     lines)
                            (incf address size))))))))
      (nreverse lines))))

;;; Stage 2: which addresses may a label legitimately point at

(defun %line-starts (lines)
  (let ((h (make-hash-table)))
    (dolist (l lines) (setf (gethash (disassembly-line-address l) h) t))
    h))

(defun %reverse-symbols (symbols line-starts &optional symbol-info)
  "Value -> name, for substituting a symbol name into rendered output.

When SYMBOL-INFO (an ASSEMBLY-SYMBOL-INFO table, #37) is given, it alone is
reversed -- each entry already carries its own QUALIFIED-NAME and VALUE, so
SYMBOLS is not even consulted here, and a caller may pass SYMBOL-INFO with
SYMBOLS NIL and still get every real label. Only entries whose
SYMBOL-INFO-KIND is :LABEL are reversed, so an .EQU's folded value never
aliases onto an instruction address that happens to equal it (#81) --
LINE-STARTS is not consulted in this case either, since a real label's
address is correct to render regardless of whether it starts a decoded
line.

Without SYMBOL-INFO (the legacy path, for a caller that only has a bare
ASSEMBLY-SYMBOLS table, string -> value), the discriminator doesn't exist,
so this falls back to the original mitigation: SYMBOLS is reversed
restricted to values that are LINE-STARTS, limiting the worst case (an
.EQU colliding with an unrelated instruction address) without eliminating
it, and unable to render a real label whose own address isn't itself a
decoded line's start.

Several names sharing one address (unusual, but not prevented by the
assembler) break ties by STRING< for a deterministic choice."
  (let ((by-value (make-hash-table)))
    (if symbol-info
        (maphash (lambda (name info)
                   (declare (ignore name))
                   (when (eq :label (symbol-info-kind info))
                     (cl:push (symbol-info-qualified-name info)
                              (gethash (symbol-info-value info) by-value))))
                 symbol-info)
        (when symbols
          (maphash (lambda (name value)
                     (when (and (integerp value) (gethash value line-starts))
                       (cl:push name (gethash value by-value))))
                   symbols)))
    (let ((result (make-hash-table)))
      (maphash (lambda (value names)
                 (setf (gethash value result) (first (sort (copy-list names) #'string<))))
               by-value)
      result)))

;;; Stage 3: render

(defun %hex-prefix (lexer)
  "The first prefix string of LEXER's :HEX NUMBER-FORMAT (lexer.lisp), or
NIL if LEXER declares none -- e.g. \"$\" for the DEFAULT lexer."
  (let* ((descriptor (find-lexer-descriptor lexer))
         (fmt (find :hex (lexer-descriptor-number-formats descriptor) :key #'number-format-name)))
    (and fmt (first (number-format-prefixes fmt)))))

(defun %render-value (value lexer &key label)
  "Render one decoded operand VALUE as re-lexable source text: LABEL
verbatim when given; a negative VALUE in plain decimal (unary-minus before a
hex literal is not a verified-lexable form); otherwise hex via LEXER's own
:HEX prefix (%HEX-PREFIX), falling back to decimal when LEXER declares no
hex format."
  (cond
    (label label)
    ((minusp value) (format nil "~D" value))
    (t (let ((prefix (%hex-prefix lexer)))
         (if prefix (format nil "~A~X" prefix value) (format nil "~D" value))))))

(defun %operand-render-values (descriptor values address size)
  "VALUES as DECODE-INSTRUCTION-AT returned them, adjusted for rendering: the
one hole DESCRIPTOR's own RELATIVE-HOLE-INDEX names (#130), if any, holds a
signed offset from the *next* instruction (assembler.lisp's %RELATIVE-OFFSET
computes it the same way on encode), so it renders as the absolute branch
target ADDRESS + SIZE + VALUE -- what a bare :RELATIVE expr operand's own
source syntax expects on re-assembly. Every other hole -- including every
hole of a descriptor with no relative hole at all -- renders as decoded.
This is per hole, not per whole descriptor: a multi-hole pattern (#130) may
have a relative hole alongside ordinary siblings, and only the relative
hole's own value gets this adjustment, mirroring %CHOOSE-VARIANT's and
%ENCODE's own per-hole treatment of the same index (assembler.lisp)."
  (let ((relative-index (instruction-descriptor-relative-hole-index descriptor)))
    (if relative-index
        (loop for v in values
              for i from 0
              collect (if (eql i relative-index) (+ address size v) v))
        values)))

(defun %render-operand-text (mode render-values lexer reverse-symbols &optional hole-choices hole-elements alias-elements
                               choice-selections)
  "Walk MODE's PATTERN (mode.lisp) in declaration order, emitting each
:LITERAL element verbatim and consuming one of RENDER-VALUES per :EXPR
hole -- concatenated with no separator, since a mode's own literals already
carry any punctuation (e.g. INDIRECT-Y's pattern renders \"($10),Y\", not
\"( $10 ) , Y\"). Values are paired by hole order, never by
INSTRUCTION-DESCRIPTOR-OPERAND-NAMES -- an unnamed field's entry there is
NIL.

HOLE-ELEMENTS, when given, is #143's own hole-aligned record -- one
STORAGE-ELEMENT (or NIL) per hole, from a (operand ... :register ELEM)
subclause's own ELEM (instruction.lisp's INSTRUCTION-DESCRIPTOR-OPERAND-
REGISTERS, resolved once by %HOLE-ELEMENTS below), popped at each :EXPR in
lockstep with RENDER-VALUES/HOLE-CHOICES, :ONE-OF recursion included. A
non-NIL element's #72 alias for the hole's own decoded value
(REGISTER-ALIAS-AT, storage.lisp) wins over a label of the same numeric
value -- an ordinary label bound to the address 0 or 5 is otherwise
indistinguishable from a register index of 0 or 5 -- and an out-of-range
index (REGISTER-ALIAS-AT returning NIL, e.g. a field wider than the bank)
falls through to hex like any unaliased value, never blank.

HOLE-CHOICES, when given, is DECODE-INSTRUCTION-AT's own hole-aligned matched
per-hole record -- a WORD-FIELD-CHOICE list on a word-encoded machine (#104)
or a bare mode-name-symbol/NIL list (the descriptor's own SUB-CHOICES) on a
byte-encoded one with a hole-selected sub-opcode (#126) -- one entry per hole
in RENDER-VALUES, parallel to it, NIL for a hole with no such record. A
:ONE-OF element peeks the entry for its own first hole before recursing,
via %MATCHED-CHOICE-NAME (instruction.lisp), which normalizes either shape:
a non-NIL result names the ONE-OF alternative that was actually matched at
assemble time (mode.lisp's hole-aligned CHOICES, threaded through a
CHOICE-selected word field or a hole-selected sub-opcode alike), and that
alternative's own pattern is rendered instead of always the first. Every
:EXPR hole this walk crosses -- whether directly or inside a :ONE-OF's
chosen alternative -- pops one entry off HOLE-CHOICES in lockstep with
RENDER-VALUES, keeping both lists aligned to the same hole position
throughout the walk.

Falls back to always rendering the first alternative -- #103's original
behaviour -- whenever HOLE-CHOICES is NIL (the default, and still true on a
byte-encoded machine whose descriptor declares no hole-selected sub-opcode
selector, where no per-hole record exists at all) or carries no non-NIL
entry for this particular hole -- a decoded word simply carries no record to
disambiguate with. #118: a word-encoded field mixing CHOICE-selected and
value-selected variants stamps every variant's own WORD-FIELD-CHOICE-CHOICE
(instruction.lisp's %CHECK-WORD-VARIANT-CHOICES!), so a value-selected row on
a *mixed* field still names its own alternative here, same as a CHOICE-
selected one -- only a field with no CHOICE variant at all still leaves this
NIL, and a hole with a CHOICE-selected field elsewhere in the same
instruction but no record of its own is unaffected either way. #126's byte
path has no mixed case: every alternative of a sub-selected hole is claimed
by construction (%CHECK-BYTE-SUB-VARIANTS!)."
  (with-output-to-string (s)
    (let ((vals render-values) (choices hole-choices) (elements hole-elements))
      (labels ((render-pattern (pattern)
                 (dolist (el pattern)
                   (ecase (first el)
                     (:literal (write-string (second el) s))
                      (:expr (let ((register (second el)))
                               (cl:pop choices)
                             (let* ((v (cl:pop vals))
                                    (element (cl:pop elements))
                                    (alias (if register
                                               (let ((owner (and alias-elements
                                                                  (loop for candidate being the hash-values of alias-elements
                                                                        when (string-equal (storage-element-name candidate)
                                                                                          register)
                                                                          return candidate))))
                                                 (and owner (register-alias-at owner v)))
                                               (and element (register-alias-at element v)))))
                               (write-string (%render-value v lexer :label (or alias (gethash v reverse-symbols))) s))))
                      (:one-of
                       (let ((alt-name (or (%matched-choice-name choices 0)
                                           (cdr (assoc (%one-of-slot el) choice-selections))
                                           (first (%one-of-alternatives el)))))
                         (render-pattern (mode-descriptor-pattern (find-mode-descriptor alt-name)))))))))
        (render-pattern (mode-descriptor-pattern mode))))))

;; TODO: this does a FIND-MACHINE-DESCRIPTOR/GETHASH pair per rendered line
;; even when OPERAND-REGISTERS is entirely NIL (the common case, every
;; machine before #143) -- a (WHEN (SOME #'IDENTITY ...) ...) short-circuit
;; would skip the lookup there; left as the straightforward version since
;; %RENDER-LINE is not the emulator's hot path.
(defun %hole-elements (descriptor)
  "DESCRIPTOR's own OPERAND-REGISTERS (#143), hole-aligned, resolved from
storage-element names to STORAGE-ELEMENTs -- NIL throughout for a descriptor
with no :REGISTER hole, same shape as OPERAND-NAMES/OPERAND-WIDTHS."
  (let ((table (machine-descriptor-table (find-machine-descriptor (instruction-descriptor-machine descriptor)))))
    (mapcar (lambda (r) (and r (gethash r table))) (instruction-descriptor-operand-registers descriptor))))

(defun %mnemonic-suffix-text (descriptor lexer)
  "The gas-style forced-mode suffix (mode.lisp's DEFMODE :SUFFIX, e.g. \"w\")
DESCRIPTOR's mode should render with its mnemonic, or NIL when none applies:
the mode declares no :SUFFIX, the mnemonic has only one registered variant
(nothing for a suffix to disambiguate), or LEXER declares no
MODE-SUFFIX-SEPARATOR to write it with."
  (let* ((mode (instruction-descriptor-mode descriptor))
         (suffix (and mode (mode-descriptor-suffix mode))))
    (and suffix
         (rest (find-instruction-variants (instruction-descriptor-machine descriptor)
                                           (instruction-descriptor-name descriptor)))
         (lexer-descriptor-mode-suffix-separator (find-lexer-descriptor lexer))
         suffix)))

(defun %render-mnemonic (descriptor lexer suffixes)
  (let ((name (string-downcase (instruction-descriptor-name descriptor)))
        (suffix (and suffixes (%mnemonic-suffix-text descriptor lexer))))
    (if suffix
        (format nil "~A~A~A" name (lexer-descriptor-mode-suffix-separator (find-lexer-descriptor lexer)) suffix)
        name)))

(defun %render-line (descriptor values address size lexer suffixes reverse-symbols choices choice-selections)
  (let ((mnemonic (%render-mnemonic descriptor lexer suffixes))
        (mode (instruction-descriptor-mode descriptor)))
    (if mode
        (format nil "~A ~A" mnemonic
                (%render-operand-text mode (%operand-render-values descriptor values address size)
                                       lexer reverse-symbols choices (%hole-elements descriptor)
                                        (machine-descriptor-register-alias-elements
                                         (find-machine-descriptor (instruction-descriptor-machine descriptor)))
                                        choice-selections))
        mnemonic)))

(defun %data-line-text (cell lexer)
  (format nil ".byte ~A" (%render-value cell lexer)))

(defun %render-lines! (lines lexer labels suffixes symbols &optional symbol-info)
  "Destructively fill in each of LINES' TEXT (always) and LABEL (only when
LABELS is true and SYMBOLS names this line's address -- restricted to
%REVERSE-SYMBOLS' line-start rule unless SYMBOL-INFO (#37) is given, in
which case only real :LABEL entries are reversed and the line-start
restriction is dropped, per %REVERSE-SYMBOLS). Returns LINES."
  (let* ((line-starts (%line-starts lines))
         (reverse-symbols (if (and labels (or symbols symbol-info))
                               (%reverse-symbols symbols line-starts symbol-info)
                               (make-hash-table))))
    (dolist (l lines)
      (let ((name (gethash (disassembly-line-address l) reverse-symbols)))
        (when name (setf (disassembly-line-label l) name)))
      (setf (disassembly-line-text l)
            (if (disassembly-line-descriptor l)
                (%render-line (disassembly-line-descriptor l) (disassembly-line-values l)
                               (disassembly-line-address l) (disassembly-line-size l)
                                lexer suffixes reverse-symbols (disassembly-line-choices l)
                                (disassembly-line-choice-selections l))
                (%data-line-text (first (disassembly-line-cells l)) lexer))))
    lines))

;;; Entry points

(defun disassemble-cells (cells &key machine (origin 0) end symbols symbol-info (lexer 'default)
                                     (labels t) (suffixes t) memory data-regions)
  "Disassemble a bare sequence CELLS (e.g. an ASSEMBLY-CELLS vector) as if
mapped into address space starting at ORIGIN, through address END (exclusive,
default ORIGIN + (LENGTH CELLS)). MACHINE (a machine name, required)
resolves instruction specs and, when it declares more than one memory
element, MEMORY selects which one's word layout/cell width apply.

SYMBOLS, when given (e.g. an ASSEMBLY-SYMBOLS table), supplies label names
for LABELS (default T) to substitute into a line's own label and into any
operand value that names another line's address -- see %REVERSE-SYMBOLS.
SYMBOL-INFO, when given (an ASSEMBLY-SYMBOL-INFO table, #37), is self-
sufficient -- SYMBOLS need not be passed alongside it -- and resolves the
label/.EQU ambiguity SYMBOLS alone cannot: only real :LABEL entries
substitute, and the line-start restriction %REVERSE-SYMBOLS otherwise
applies is dropped, since a real label's address is always correct to
render (#81). Pass it whenever available; DISASSEMBLE-ASSEMBLY does so
automatically. SUFFIXES (default T) renders a gas-style forced mode suffix
(e.g. \"lda.w\") when needed for re-assembly fidelity; LEXER (default
'DEFAULT) selects the surface syntax operand numbers and suffixes render in.
DATA-REGIONS (#82), a list of (START . END) absolute cell ranges with END
exclusive, are rendered as \".byte\" lines without decoding.

Returns a list of DISASSEMBLY-LINE, ascending by address. See this file's
header comment for the mid-stream decode-failure policy and the honest scope
of round-trip fidelity."
  (unless machine (error "DISASSEMBLE-CELLS: :MACHINE is required"))
  (let* ((end (or end (+ origin (length cells))))
         (read-cell (vector-cell-reader cells :origin origin :end end))
         (lines (%disassemble-raw-lines read-cell origin end machine memory data-regions)))
    (%render-lines! lines lexer labels suffixes symbols symbol-info)))

(defun disassemble-assembly (assembly &key machine (lexer 'default) (labels t) (suffixes t) memory
                                           (data-regions :auto))
  "DISASSEMBLE-CELLS over an ASSEMBLY (assembler.lisp), pulling CELLS,
ORIGIN, SYMBOLS, and SYMBOL-INFO (#37) off it directly -- the natural way to
round-trip ASSEMBLE's own output, and the reason its label substitution
never suffers the .EQU-aliasing ambiguity #81 tracks for a bare-SYMBOLS
caller. Signals if ASSEMBLY's own ASSEMBLY-CELL-WIDTH does not match
MACHINE's declared cell width, mirroring LOAD-PROGRAM's own check
(emulator.lisp) for the same mismatch.

DATA-REGIONS (#82) defaults to :AUTO, ASSEMBLY-DATA-REGIONS (listing.lisp);
pass NIL to decode everything, or an explicit list to override."
  (unless machine (error "DISASSEMBLE-ASSEMBLY: :MACHINE is required"))
  (let ((target-width (%machine-cell-width machine memory))
        (source-width (assembly-cell-width assembly)))
    (unless (= target-width source-width)
      (error "DISASSEMBLE-ASSEMBLY on machine ~S: assembly's cell width (~D) does not ~
match the machine's cell width (~D)" machine source-width target-width)))
  (disassemble-cells (assembly-cells assembly)
                      :machine machine :origin (assembly-origin assembly)
                      :symbols (assembly-symbols assembly)
                      :symbol-info (assembly-symbol-info assembly)
                      :lexer lexer :labels labels :suffixes suffixes :memory memory
                      :data-regions (if (eq data-regions :auto)
                                        (assembly-data-regions assembly)
                                        data-regions)))

(defun disassemble-memory (machine &key memory start count symbols symbol-info (lexer 'default)
                                        (labels t) (suffixes t) data-regions)
  "DISASSEMBLE-CELLS over MACHINE's live MEMORY (a MACHINE runtime instance,
storage.lisp) from address START through START + COUNT (exclusive). START
and COUNT are both required -- unlike DISASSEMBLE-CELLS' END, there is no
sane default for \"the whole address space\" of a live machine. MEMORY
defaults per %RESOLVE-MEMORY, same convention as LOAD-PROGRAM/STEP-MACHINE.
SYMBOL-INFO (#37), when available (e.g. from the ASSEMBLY that produced this
memory's contents), resolves the label/.EQU ambiguity and works standalone,
without SYMBOLS -- see DISASSEMBLE-CELLS. DATA-REGIONS (#82) is as there.

#107: reads via MACHINE-PEEK-READER, not MACHINE-CELL-READER -- disassembly
is inspection, not execution, so it must not trigger a :DEVICE region's
:READ side effects merely by disassembling across it."
  (unless (and start count)
    (error "DISASSEMBLE-MEMORY: :START and :COUNT are both required"))
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (memory (%resolve-memory machine-name memory))
         (read-cell (machine-peek-reader machine memory))
         (end (+ start count))
         (lines (%disassemble-raw-lines read-cell start end machine-name memory data-regions)))
    (%render-lines! lines lexer labels suffixes symbols symbol-info)))

;;; Text output

(defun disassembly-text (lines &key stream origin (indent "        "))
  "Render LINES (DISASSEMBLY-LINE list) as re-assemblable source text: a
leading \".org <origin>\" when ORIGIN is given and non-zero, each line's own
label on its own line immediately before it, and each line's rendered TEXT
indented by INDENT. Returns the text as a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL."
  (let ((body (with-output-to-string (s)
                (when (and origin (/= origin 0))
                  (format s ".org ~D~%" origin))
                (dolist (l lines)
                  (when (disassembly-line-label l)
                    (format s "~A:~%" (disassembly-line-label l)))
                  (format s "~A~A~%" indent (disassembly-line-text l))))))
    (if stream (progn (write-string body stream) nil) body)))

(defun print-disassembly (lines &key (stream *standard-output*) origin)
  "Print LINES (DISASSEMBLY-LINE list) as an address/cells/text listing for
human reading -- not re-assemblable source (see DISASSEMBLY-TEXT for that).
ORIGIN is accepted, unused, to keep the same call shape as DISASSEMBLY-TEXT
convenient at a call site that has one on hand. Returns LINES.

KNOWN ISSUE: the cell field below is hardcoded to 2 hex digits (~2,'0X),
right for an 8-bit cell but too narrow for a wider one -- on a word-encoded
machine (e.g. dcpu16, 16-bit cells) a cell needing fewer than 4 digits
prints unpadded (\"3E8\", not \"03E8\") instead of a fixed width. LISTING-TEXT
(listing.lisp, #25) gets this right, sizing its own hex field from
ASSEMBLY-CELL-WIDTH -- (CEILING CELL-WIDTH 4) digits -- but this predates it
and hasn't been fixed to match; tracked as a follow-up ticket."
  (declare (ignore origin))
  (dolist (l lines)
    (format stream "~4,'0X  ~{~2,'0X~^ ~}~24T~A~%"
            (disassembly-line-address l) (disassembly-line-cells l) (disassembly-line-text l)))
  lines)
