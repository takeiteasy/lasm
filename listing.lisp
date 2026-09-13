;;;; listing.lisp
;;;; #25 (M7): renders an ASSEMBLY's LISTING (assembler.lisp) as a
;;;; conventional address/cells/source listing and answers address<->line
;;;; lookups against it, built on the address<->statement mapping
;;;; ASSEMBLE-STATEMENTS now retains instead of discarding once %ENCODE has
;;;; run.
;;;;
;;;; LOOKUP DIRECTION -- address->line is a function (LISTING-LINE-AT: an
;;;; address belongs to at most one entry's [address, address+size) run),
;;;; but line->address is not: EXPAND-MACROS' %SUBSTITUTE-STATEMENT
;;;; (macro.lisp, #33) copies each expanded statement's LINE from the
;;;; *macro body's own definition*, unchanged by substitution -- so every
;;;; invocation of a two-line macro produces two fresh statements both still
;;;; tagged with their original line inside the .macro/.endm block, not the
;;;; call site. Two invocations of the same macro therefore both contribute
;;;; a LISTING-LINE at the *same* body line, at two different addresses --
;;;; one source line legitimately owning several entries.
;;;; LISTING-LINES-FOR-SOURCE-LINE returns all of them, in address order; a
;;;; listing built from source text (LISTING-TEXT below) prints the body
;;;; line's own text once per entry found for it, grouped by line rather
;;;; than strictly by address -- the call site's own line, having expanded
;;;; to no entry of its own, shows once with blank columns, same as any
;;;; other address-free statement.
;;;;
;;;; RENDERING -- when ASSEMBLY-SOURCE is present, LISTING-TEXT walks the
;;;; source line by line (1-based) rather than the entry list directly, so a
;;;; line with no LISTING-LINE at all (a comment, a label-only line, .ORG,
;;;; .EQU -- none of which occupy address space) still appears, with blank
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

(defun listing-lines-for-source-line (assembly line)
  "Every LISTING-LINE in ASSEMBLY-LISTING whose LINE slot is LINE, in
address order -- a list, not a single entry, since a macro invocation's
expanded statements all carry the invocation's own source line (see this
file's header comment). Empty (not NIL-as-absent -- just an empty list) when
LINE occupies no address space (a comment, a label-only line, .ORG, .EQU) or
names no line in this ASSEMBLY at all."
  (remove-if-not (lambda (l) (= line (listing-line-line l))) (assembly-listing assembly)))

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
                     (%listing-text-from-source assembly s digits)
                     (%listing-text-entries-only assembly s digits)))))
    (if stream (progn (write-string body stream) nil) body)))

(defun print-listing (assembly &key (stream *standard-output*))
  "LISTING-TEXT written to STREAM (default *STANDARD-OUTPUT*) -- parallel to
PRINT-DISASSEMBLY (disassembler.lisp). Returns ASSEMBLY."
  (listing-text assembly :stream stream)
  assembly)
