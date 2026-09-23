;;;; listing.lisp
;;;; #25 (M7): renders an ASSEMBLY's LISTING (assembler.lisp) as a
;;;; conventional address/cells/source listing and answers address<->line
;;;; lookups against it, built on the address<->statement mapping
;;;; ASSEMBLE-STATEMENTS now retains instead of discarding once %ENCODE has
;;;; run.
;;;;
;;;; Macro expansion records the outermost invocation line as the source line
;;;; and retains the emitted body's definition line separately. Source-line
;;;; lookups return all entries emitted by that line, in address order.
;;;;
;;;; RENDERING -- when ASSEMBLY-SOURCE is present, LISTING-TEXT walks the
;;;; source line by line (1-based) rather than the entry list directly, so a
;;;; line with no LISTING-LINE at all (a comment, a label-only line, .ORG,
;;;; .EQU, .SET -- none of which occupy address space) still appears, with blank
;;;; address/cells columns, giving a complete listing rather than one with
;;;; silent gaps. With no source (ASSEMBLY-SOURCE NIL -- e.g. an ASSEMBLY
;;;; built via ASSEMBLE-STATEMENTS with no :SOURCE), this degrades to an
;;;; entry-ordered listing with the source column omitted entirely, since
;;;; there is no source text to index into.
;;;;
;;;; CELLS -- a LISTING-LINE (assembler.lisp) stores no cells of its own;
;;;; LISTING-TEXT slices them out of ASSEMBLY-CELLS at
;;;; [address - origin, address - origin + size) on demand, which
;;;; %ENSURE-CELLS-LENGTH (assembler.lisp) already guarantees is in bounds
;;;; for every entry, a trailing .RES run included. The hex field width is
;;;; sized from ASSEMBLY-CELL-WIDTH -- (CEILING CELL-WIDTH 4) digits per
;;;; cell -- rather than hardcoded to 2, so a 16-bit-cell machine (e.g.
;;;; dcpu16) renders full-width cells instead of truncating them (see
;;;; PRINT-DISASSEMBLY in disassembler.lisp, which does hardcode 2 digits --
;;;; a pre-existing bug tracked separately, not fixed here). A long cell run
;;;; (a sizeable .RES) is elided to keep one entry to one line.

(in-package #:lasm)

;;; Lookup

(defun listing-line-at (assembly address)
  "The LISTING-LINE in ASSEMBLY-LISTING whose [ADDRESS-of-entry,
ADDRESS-of-entry + SIZE) run contains ADDRESS, or NIL if ADDRESS falls in a
gap (e.g. a forward .ORG's pad) or past the end. A linear scan over
ASSEMBLY-LISTING -- fine at the program sizes LASM currently targets; a
follow-up ticket tracks an address-indexed structure if that ever matters."
  (find-if (lambda (l) (<= (listing-line-address l) address
                            (1- (+ (listing-line-address l) (listing-line-size l)))))
            (assembly-listing assembly)))

(defun listing-lines-for-source-line (assembly line &key file)
  "Entries emitted by LINE, in address order. Without FILE, select the
top-level source. With FILE, select that included path; repeated includes
contribute entries from each occurrence."
  (remove-if-not (lambda (l)
                   (and (= line (listing-line-line l))
                        (if file
                            (equal (listing-line-file l) (namestring (pathname file)))
                            (or (null (assembly-source-unit assembly))
                                (eq (listing-line-source-unit l)
                                    (assembly-source-unit assembly))))))
                 (assembly-listing assembly)))

;;; Data regions (#82)

(defun assembly-data-regions (assembly)
  "The (START . END) cell ranges, END exclusive, ASSEMBLY's .byte/.word/.res
statements occupy, ascending, adjacent runs merged -- the :DATA-REGIONS
DISASSEMBLE-ASSEMBLY passes by default. Empty when ASSEMBLY-LISTING is NIL."
  (let (regions)
    (dolist (l (assembly-listing assembly))
      (when (and (member (listing-line-kind l) '(:emit :reserve))
                 (plusp (listing-line-size l)))
        (let ((start (listing-line-address l))
              (end (+ (listing-line-address l) (listing-line-size l))))
          (if (and regions (= start (cdr (first regions))))
              (setf (cdr (first regions)) end)
              (cl:push (cons start end) regions)))))
    (nreverse regions)))

;;; Rendering

(defun %listing-hex-digits (cell-width)
  "Hex digits needed to render one CELL-WIDTH-bit cell -- (CEILING
CELL-WIDTH 4), so an 8-bit cell prints 2 digits and a 16-bit cell (dcpu16)
prints 4, rather than hardcoding 2 the way PRINT-DISASSEMBLY does."
  (ceiling cell-width 4))

(defparameter *listing-max-cells-shown* 8
  "LISTING-TEXT elides a LISTING-LINE's cell column to this many cells (plus
an ellipsis) when its SIZE exceeds it -- a large .RES run would otherwise
spill a single line across the whole listing width for no diagnostic
benefit, since every shown cell would be the same zero.")

(defun %listing-cells (assembly line)
  "LINE's own encoded cells, sliced out of ASSEMBLY-CELLS -- see this file's
header comment on why LISTING-LINE stores no cells of its own."
  (let* ((cells (assembly-cells assembly))
         (origin (assembly-origin assembly))
         (start (- (listing-line-address line) origin)))
    (coerce (subseq cells start (+ start (listing-line-size line))) 'list)))

(defun %listing-cells-text (assembly line digits)
  (let ((cells (%listing-cells assembly line)))
    (if (> (length cells) *listing-max-cells-shown*)
        (format nil "~{~V,'0X~^ ~} ..." (loop for c in (subseq cells 0 *listing-max-cells-shown*)
                                              collect digits collect c))
        (format nil "~{~V,'0X~^ ~}" (loop for c in cells collect digits collect c)))))

(defun %listing-row (stream addr-text cells-text source-text)
  "Write one ADDR/CELLS/SOURCE row to STREAM. ~24T (not nested inside a
FORMAT NIL for the source column alone) tabs from STREAM's own current
column, so a CELLS-TEXT run past column 24 (a wide cell run) still pushes
SOURCE-TEXT out rather than the reverse -- computing ~24T inside a separate
FORMAT NIL call would tab from that substring's own column 0 instead,
misaligning every row after the first with a longer cells column."
  (if source-text
      (format stream "~8A  ~A~24T~A~%" addr-text cells-text source-text)
      (format stream "~8A  ~A~%" addr-text cells-text)))

(defun %split-source-lines (source)
  "SOURCE split on newlines into a list of lines, 1-based index == line
number -- the same convention TOKEN-LINE/STATEMENT-LINE use elsewhere. A
trailing newline contributes no extra (empty) final line, matching
%SPLIT-LINES' own treatment of one (parser.lisp) -- otherwise a real
source file, which almost always ends in a newline, would render one
spurious blank row past its last statement."
  (let (lines (start 0))
    (loop for pos = (position #\Newline source :start start)
          do (cl:push (subseq source start pos) lines)
             (if pos (setf start (1+ pos)) (return)))
    (when (and (rest lines) (string= "" (first lines)))
      (cl:pop lines))
    (nreverse lines)))

(defun %listing-text-from-source (assembly stream digits)
  "LISTING-TEXT's rendering path when ASSEMBLY-SOURCE is present -- walk
source lines 1..N, printing each LISTING-LINE entry (there may be several,
or none, per line -- see this file's header comment) before repeating a
data-only line's own text with blank columns when a line has no entry."
  (let* ((source-lines (%split-source-lines (assembly-source assembly)))
         (by-line (make-hash-table)))
    (dolist (l (assembly-listing assembly))
      (cl:push l (gethash (listing-line-line l) by-line)))
    (maphash (lambda (k v) (setf (gethash k by-line) (nreverse v))) by-line)
    (loop for text in source-lines
          for n from 1
          for entries = (gethash n by-line)
          do (if entries
                 (dolist (entry entries)
                   (%listing-row stream (format nil "~4,'0X" (listing-line-address entry))
                                 (%listing-cells-text assembly entry digits) text))
                 (%listing-row stream "" "" text)))))

(defun %listing-text-entries-only (assembly stream digits)
  "LISTING-TEXT's rendering path when ASSEMBLY-SOURCE is NIL -- no source
text to index into, so this just walks ASSEMBLY-LISTING in address order
with the source column omitted entirely."
  (dolist (l (assembly-listing assembly))
    (%listing-row stream (format nil "~4,'0X" (listing-line-address l))
                  (%listing-cells-text assembly l digits) nil)))

(defun %listing-entry-index (assembly)
  (let ((index (make-hash-table :test 'eq)))
    (dolist (entry (assembly-listing assembly))
      (let* ((unit (listing-line-source-unit entry))
             (by-line (or (gethash unit index)
                          (setf (gethash unit index) (make-hash-table)))))
        (cl:push entry (gethash (listing-line-line entry) by-line))))
    index))

(defun %listing-text-from-unit (assembly stream digits unit index &optional labels)
  (let* ((lines (%split-source-lines (source-unit-text unit)))
         (by-line (gethash unit index)))
    (loop for text in lines
          for line from 1
          for entries = (nreverse (gethash line by-line))
          for marked = (if labels
                           (format nil "~A~D | ~A"
                                   (if (source-unit-file unit)
                                       (format nil "~A:" (source-unit-file unit))
                                       "line ")
                                   line text)
                           text)
          do (if entries
                 (dolist (entry entries)
                   (%listing-row stream (format nil "~4,'0X" (listing-line-address entry))
                                 (%listing-cells-text assembly entry digits) marked))
                 (%listing-row stream "" "" marked))
             (dolist (child (gethash line (source-unit-children unit)))
               (%listing-text-from-unit assembly stream digits child index t)))))

(defun listing-text (assembly &key stream)
  "Render ASSEMBLY's LISTING (assembler.lisp, #25) as a conventional
assembler listing: address, encoded cells, and (when ASSEMBLY-SOURCE is
present) the original source line, one row per source line. See this file's
header comment for how a line with no LISTING-LINE entry (a comment, .ORG,
.EQU, a label-only line) still renders with blank address/cells columns,
how a macro invocation's line repeats once per expanded statement, and how
this degrades to an entry-ordered listing with no source column when
ASSEMBLY-SOURCE is NIL. Returns the text as a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL."
  (let* ((digits (%listing-hex-digits (assembly-cell-width assembly)))
         (body (with-output-to-string (s)
                 (if (assembly-source assembly)
                     (if (assembly-source-unit assembly)
                         (%listing-text-from-unit assembly s digits
                                                  (assembly-source-unit assembly)
                                                  (%listing-entry-index assembly)
                                                  (or (source-unit-file (assembly-source-unit assembly))
                                                      (plusp (hash-table-count
                                                              (source-unit-children
                                                               (assembly-source-unit assembly))))))
                         (%listing-text-from-source assembly s digits))
                     (%listing-text-entries-only assembly s digits)))))
    (if stream (progn (write-string body stream) nil) body)))

(defun print-listing (assembly &key (stream *standard-output*))
  "LISTING-TEXT written to STREAM (default *STANDARD-OUTPUT*) -- parallel to
PRINT-DISASSEMBLY (disassembler.lisp). Returns ASSEMBLY."
  (listing-text assembly :stream stream)
  assembly)

;;; Symbol table (#37) -- scope- and kind-aware lookup and listing over
;;; ASSEMBLY-SYMBOL-INFO, built alongside ASSEMBLY-SYMBOLS by the assembler
;;; (assembler.lisp) to answer "what's defined in this scope, and is it a
;;; label or an .EQU" without ASSEMBLY-SYMBOLS itself having to stop being a
;;; flat string -> value table. Local keys contain a reserved separator.
;;;
;;; Every function below degrades gracefully (returns NIL or an empty
;;; result) when ASSEMBLY-SYMBOL-INFO is itself NIL -- callers assembling by
;;; hand or from an older code path never crash on a missing table.
;;;
;;; Symbols sort by invocation line and binding order. The definition line
;;; remains available for macro provenance.
;;;
;;; PERFORMANCE -- ASSEMBLY-SYMBOLS-LIST is a full MAPHASH-and-sort per call,
;;; and ASSEMBLY-SYMBOL-GROUPS additionally calls ASSEMBLY-SYMBOL (itself a
;;; hash lookup) once per scope from inside a SORT key function -- fine at
;;; the program sizes LASM currently targets, same tradeoff LISTING-LINE-AT
;;; already makes (#88) and no worse; a follow-up ticket tracks an
;;; address/scope-indexed structure if either ever shows up as a hot path.

(defun assembly-symbol (assembly name &key scope)
  "Return SYMBOL-INFO for NAME. SCOPE selects a local under that global;
without it, NAME identifies a global or top-level assignment."
  (let ((info (assembly-symbol-info assembly)))
    (and info (gethash (if scope (%qualify-local scope name 0) name) info))))

(defun assembly-symbols-list (assembly &key kind (scope :any scope-given-p))
  "Every SYMBOL-INFO in ASSEMBLY, in SYMBOL-INFO-LINE order (see this file's
header comment on why line order, not address order). KIND, when given
  (:LABEL, :EQU, or :SET), restricts to that kind. SCOPE, when given, restricts to
symbols whose SYMBOL-INFO-SCOPE is SCOPE -- pass SCOPE NIL for top-level
symbols (globals and top-level .EQUs); omitting SCOPE entirely means no
scope filter at all. Empty (not NIL-as-absent) when ASSEMBLY-SYMBOL-INFO is
NIL or nothing matches."
  (let ((info (assembly-symbol-info assembly))
        result)
    (when info
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (when (and (or (null kind) (eq kind (symbol-info-kind v)))
                            (or (not scope-given-p)
                                (equal (symbol-info-scope v) scope)))
                   (cl:push v result)))
               info))
    (sort result (lambda (a b)
                   (if (= (symbol-info-line a) (symbol-info-line b))
                       (< (symbol-info-order a) (symbol-info-order b))
                       (< (symbol-info-line a) (symbol-info-line b)))))))

(defun assembly-symbol-groups (assembly)
  "ASSEMBLY's symbols (#37) grouped by enclosing scope, as an alist of
(GLOBAL-NAME . SYMBOL-INFO-LIST) -- the ticket's ask: a listing / source-map
pass can group locals under their enclosing global label rather than print
a flat, ambiguous list. One entry per global label that has at least one
local (or itself), the global's own SYMBOL-INFO heading its list followed
by its locals (SYMBOL-INFO-LINE order); a leading (NIL . ...) entry holds
every top-level symbol (a global with no locals still appears here via its
own binding, plus every top-level .EQU) -- present, though possibly empty,
even when ASSEMBLY-SYMBOL-INFO is NIL. Entries after the leading NIL bucket
are ordered by their global's own SYMBOL-INFO-LINE."
  (let ((all (assembly-symbols-list assembly))
        (top nil)
        (by-scope (make-hash-table :test 'equal))
        scopes)
    (dolist (s all)
      (when (and (symbol-info-scope s)
                 (not (nth-value 1 (gethash (symbol-info-scope s) by-scope))))
        (cl:push (symbol-info-scope s) scopes))
      (if (symbol-info-scope s)
          (cl:push s (gethash (symbol-info-scope s) by-scope))
          (cl:push s top)))
    ;; A global that heads a scope (i.e. has at least one local) belongs in
    ;; that scope's own entry, not the top-level bucket, even though its own
    ;; SYMBOL-INFO-SCOPE is NIL like any other top-level symbol -- matched by
    ;; NAME against SCOPES (a list of global names with at least one local).
    ;; Safe against a same-named top-level .EQU shadowing this filter: a
    ;; top-level .EQU and a global share one flat unqualified namespace
    ;; (%BIND-SYMBOL!'s duplicate check), so an .EQU can never have the same
    ;; NAME as a global that also has locals -- one or the other would
    ;; already have signalled ASSEMBLY-ERROR at bind time.
    (setf top (remove-if (lambda (s) (member (symbol-info-name s) scopes :test #'string=)) top))
    (cons (cons nil (nreverse top))
          (mapcar (lambda (scope)
                    (let ((global (assembly-symbol assembly scope)))
                      (cons scope
                            (append (and global (list global))
                                    (sort (nreverse (gethash scope by-scope))
                                          #'< :key #'symbol-info-line)))))
                  (sort scopes #'<
                        :key (lambda (scope)
                               (let ((g (assembly-symbol assembly scope)))
                                 (if g (symbol-info-line g) 0))))))))

(defun %symbol-value-text (info digits)
  "INFO's VALUE rendered DIGITS-wide hex for a :LABEL (an address, matching
LISTING-TEXT's own address rendering), or plain decimal for an assignment (not an
address, so hex width has no natural meaning)."
  (if (eq (symbol-info-kind info) :label)
      (format nil "~V,'0X" digits (symbol-info-value info))
      (format nil "~D" (symbol-info-value info))))

(defun symbols-text (assembly &key stream)
  "Render ASSEMBLY's symbol table (#37), grouped by scope
(ASSEMBLY-SYMBOL-GROUPS): top-level symbols first, then each global label
with its locals indented underneath, each row naming a symbol, its value
(hex for a :LABEL, decimal for an assignment), and its KIND. Returns the text as a
string when STREAM is NIL (default); otherwise writes to STREAM and returns
NIL. Empty string/no output when ASSEMBLY-SYMBOL-INFO is NIL."
  (let* ((digits (%listing-hex-digits (assembly-cell-width assembly)))
         (groups (assembly-symbol-groups assembly))
         (body (with-output-to-string (s)
                 (dolist (group groups)
                   (destructuring-bind (scope . symbols) group
                     (declare (ignore scope))
                     (dolist (sym symbols)
                       (format s "~:[  ~;~]~A~24T~A~30T~(~A~)~%"
                               (null (symbol-info-scope sym))
                               (symbol-info-name sym)
                               (%symbol-value-text sym digits)
                               (symbol-info-kind sym))))))))
    (if stream (progn (write-string body stream) nil) body)))

(defun print-symbols (assembly &key (stream *standard-output*))
  "SYMBOLS-TEXT written to STREAM (default *STANDARD-OUTPUT*) -- parallel to
PRINT-LISTING/PRINT-DISASSEMBLY. Returns ASSEMBLY."
  (symbols-text assembly :stream stream)
  assembly)
