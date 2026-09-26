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
;;;; dcpu16) renders full-width cells instead of truncating them, as
;;;; PRINT-DISASSEMBLY (disassembler.lisp) does per line. A long cell run
;;;; (a sizeable .RES) is elided to keep one entry to one line.

(in-package #:lasm)

;;; Lookup

(defun %same-image-p (line region bank)
  (and (eq (listing-line-region line) region)
       (eql (listing-line-bank line) bank)))

(defun listing-line-at (assembly address &key region bank)
  "The LISTING-LINE in ASSEMBLY-LISTING whose [ADDRESS-of-entry,
ADDRESS-of-entry + SIZE) run contains ADDRESS, or NIL if ADDRESS falls in a
gap (e.g. a forward .ORG's pad) or past the end. REGION and BANK select an
entry placed in that bank of a banked region; by default only main-image
entries match. A linear scan over ASSEMBLY-LISTING -- fine at the program
sizes LASM currently targets; ticket 295 tracks an address-indexed
structure if that ever matters."
  (find-if (lambda (l) (and (%same-image-p l region bank)
                            (<= (listing-line-address l) address
                                (1- (+ (listing-line-address l) (listing-line-size l))))))
            (assembly-listing assembly)))

(defun %loaded-program-covers-p (machine program address)
  "True when PROGRAM's main image, or a .BANK image mapped in on MACHINE,
holds ADDRESS."
  (or (<= (loaded-program-origin program) address (1- (%loaded-program-end program)))
      (some (lambda (image)
              (and (eql (current-bank machine (bank-image-region image)) (bank-image-bank image))
                   (<= (bank-image-origin image) address
                       (+ (bank-image-origin image) (length (bank-image-cells image)) -1))))
            (assembly-banks (loaded-program-assembly program)))))

(defun %loaded-program-at (machine address memory)
  "The newest of MACHINE's LOADED-PROGRAMs holding ADDRESS of MEMORY, any
memory when MEMORY is NIL."
  (find-if (lambda (program)
             (and (or (null memory) (eq memory (loaded-program-memory program)))
                  (%loaded-program-covers-p machine program address)))
           (machine-programs machine)))

(defun machine-program-at (machine address &key memory)
  "The ASSEMBLY of the newest program loaded into MACHINE that holds ADDRESS,
in MEMORY when given, or NIL."
  (let ((program (%loaded-program-at machine address memory)))
    (and program (loaded-program-assembly program))))

(defun %machine-image (machine address memory assembly)
  "As (VALUES ASSEMBLY PROGRAM): the assembly to look ADDRESS up in and the
LOADED-PROGRAM giving its load offset and memory, NIL for an assembly the
machine never loaded. NIL when ASSEMBLY is not the program MEMORY holds.
Without ASSEMBLY, the program holding ADDRESS."
  (if assembly
      (let ((loaded (remove-if-not (lambda (p) (eq assembly (loaded-program-assembly p))) (machine-programs machine))))
        (if loaded
            (let ((program (find-if (lambda (p) (or (null memory) (eq memory (loaded-program-memory p))))
                                    loaded)))
              (and program (values assembly program)))
            (values assembly nil)))
      (let ((program (%loaded-program-at machine address memory)))
        (and program (values (loaded-program-assembly program) program)))))

(defun %machine-image-address (machine address memory program)
  "ADDRESS translated into PROGRAM's address space (unchanged without one),
and the banked region name and bank mapped there (NIL for the main image), as
(VALUES LISTED REGION BANK)."
  (let* ((descriptor (machine-descriptor machine))
         (memory (or memory
                     (and program (loaded-program-memory program))
                     (%resolve-memory (machine-descriptor-name descriptor) nil)))
         (element (descriptor-element descriptor memory))
         (region (find-if (lambda (r) (and (memory-region-banks r)
                                           (<= (memory-region-start r) address
                                               (memory-region-end r))))
                          (storage-element-regions element)))
         (name (and region (memory-region-name region))))
    (values (if program (- address (%loaded-program-offset program)) address)
            name
            (and name (current-bank machine name)))))

(defun machine-listing-line (machine address &key memory assembly)
  "The LISTING-LINE for ADDRESS in MACHINE's MEMORY, or NIL: the entry in the
bank mapped at ADDRESS when it lies in a banked region, else (or failing
that) the main image's. ASSEMBLY defaults to the newest program LOAD-PROGRAM
retained that holds ADDRESS. For a loaded ASSEMBLY, MEMORY must be a memory
it was loaded into, and ADDRESS is translated by its load offset."
  (multiple-value-bind (assembly program) (%machine-image machine address memory assembly)
    (when assembly
      (multiple-value-bind (listed region bank) (%machine-image-address machine address memory program)
        (or (and region (listing-line-at assembly listed :region region :bank bank))
            (listing-line-at assembly listed))))))

(defun listing-line-source-text (line assembly)
  "The source text of LISTING-LINE LINE: from its own included source unit
when it has one, else from ASSEMBLY-SOURCE. NIL if unavailable."
  (let ((text (cond ((listing-line-source-unit line)
                     (source-unit-text (listing-line-source-unit line)))
                    (t (assembly-source assembly)))))
    (and text (nth (1- (listing-line-line line)) (%split-source-lines text)))))

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

(defun assembly-data-regions (assembly &key region bank)
  "The (START . END) cell ranges, END exclusive, ASSEMBLY's .byte/.word/.res
statements occupy, ascending, adjacent runs merged -- the :DATA-REGIONS
DISASSEMBLE-ASSEMBLY passes by default. REGION and BANK select a bank's
image instead of the main one. Empty when ASSEMBLY-LISTING is NIL."
  (let (regions)
    (dolist (l (assembly-listing assembly))
      (when (and (%same-image-p l region bank)
                 (member (listing-line-kind l) '(:emit :reserve))
                 (plusp (listing-line-size l)))
        (let ((start (listing-line-address l))
              (end (+ (listing-line-address l) (listing-line-size l))))
          (if (and regions (= start (cdr (first regions))))
              (setf (cdr (first regions)) end)
              (cl:push (cons start end) regions)))))
    (nreverse regions)))

;;; Rendering

(defun %listing-hex-digits (cell-width)
  "Hex digits needed to render one CELL-WIDTH-bit cell."
  (ceiling cell-width 4))

(defparameter *listing-max-cells-shown* 8
  "LISTING-TEXT elides a LISTING-LINE's cell column to this many cells (plus
an ellipsis) when its SIZE exceeds it -- a large .RES run would otherwise
spill a single line across the whole listing width for no diagnostic
benefit, since every shown cell would be the same zero.")

(defun assembly-bank-image (assembly region bank)
  "ASSEMBLY's BANK-IMAGE for BANK of REGION, or NIL if nothing was placed there."
  (find-if (lambda (image) (and (eq (bank-image-region image) region)
                                (eql (bank-image-bank image) bank)))
           (assembly-banks assembly)))

(defun %listing-address-text (line)
  "LINE's address column: AAAA, or BB:AAAA for an entry in a bank."
  (if (listing-line-bank line)
      (format nil "~2,'0X:~4,'0X" (listing-line-bank line) (listing-line-address line))
      (format nil "~4,'0X" (listing-line-address line))))

(defun %listing-cells (assembly line)
  "LINE's own encoded cells, sliced out of ASSEMBLY-CELLS (or its bank's
image) -- see this file's header comment on why LISTING-LINE stores no cells
of its own."
  (let* ((image (and (listing-line-bank line)
                     (assembly-bank-image assembly (listing-line-region line)
                                          (listing-line-bank line))))
         (cells (if image (bank-image-cells image) (assembly-cells assembly)))
         (origin (if image (bank-image-origin image) (assembly-origin assembly)))
         (start (- (listing-line-address line) origin)))
    (coerce (subseq cells start (+ start (listing-line-size line))) 'list)))

(defun %listing-cells-text (assembly line digits)
  (let ((cells (%listing-cells assembly line)))
    (if (> (length cells) *listing-max-cells-shown*)
        (format nil "~{~V,'0X~^ ~} ..." (loop for c in (subseq cells 0 *listing-max-cells-shown*)
                                              collect digits collect c))
        (format nil "~{~V,'0X~^ ~}" (loop for c in cells collect digits collect c)))))

(defvar *listing-cycles* nil
  "True while LISTING-TEXT renders its cycles column (#180).")

(defparameter *listing-cycles-width* 5
  "Width of the cycles column, its trailing gap included.")

(defun %listing-cycles-text (entry)
  "ENTRY's cycles column: its declared cost, followed by + when semantics may
add more at run time. Blank for anything but an instruction."
  (let ((descriptor (and entry (listing-line-descriptor entry))))
    (if descriptor
        (format nil "~D~:[~;+~]" (%descriptor-cycle-cost descriptor)
                (instruction-descriptor-variable-cycles descriptor))
        "")))

(defun %listing-row (stream addr-text cells-text source-text &optional entry)
  "Write one ADDR/CELLS/SOURCE row to STREAM. ~24T (not nested inside a
FORMAT NIL for the source column alone) tabs from STREAM's own current
column, so a CELLS-TEXT run past column 24 (a wide cell run) still pushes
SOURCE-TEXT out rather than the reverse -- computing ~24T inside a separate
FORMAT NIL call would tab from that substring's own column 0 instead,
misaligning every row after the first with a longer cells column."
  (let ((cycles (if *listing-cycles*
                    (format nil "~VA" *listing-cycles-width* (%listing-cycles-text entry))
                    "")))
    (if source-text
        (format stream "~8A  ~A~A~VT~A~%" addr-text cycles cells-text
                (+ 24 (length cycles)) source-text)
        (format stream "~8A  ~A~A~%" addr-text cycles cells-text))))

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
                   (%listing-row stream (%listing-address-text entry)
                                 (%listing-cells-text assembly entry digits) text entry))
                 (%listing-row stream "" "" text)))))

(defun %listing-text-entries-only (assembly stream digits)
  "LISTING-TEXT's rendering path when ASSEMBLY-SOURCE is NIL -- no source
text to index into, so this just walks ASSEMBLY-LISTING in address order
with the source column omitted entirely."
  (dolist (l (assembly-listing assembly))
    (%listing-row stream (%listing-address-text l)
                  (%listing-cells-text assembly l digits) nil l)))

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
                   (%listing-row stream (%listing-address-text entry)
                                 (%listing-cells-text assembly entry digits) marked entry))
                 (%listing-row stream "" "" marked))
             (dolist (child (gethash line (source-unit-children unit)))
               (%listing-text-from-unit assembly stream digits child index t)))))

(defun listing-text (assembly &key stream cycles)
  "Render ASSEMBLY's LISTING (assembler.lisp, #25) as a conventional
assembler listing: address, encoded cells, and (when ASSEMBLY-SOURCE is
present) the original source line, one row per source line. See this file's
header comment for how a line with no LISTING-LINE entry (a comment, .ORG,
.EQU, a label-only line) still renders with blank address/cells columns,
how a macro invocation's line repeats once per expanded statement, and how
this degrades to an entry-ordered listing with no source column when
ASSEMBLY-SOURCE is NIL. Returns the text as a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL.

CYCLES (#180), when true, adds a column after the address holding each
instruction's declared cycle cost (%DESCRIPTOR-CYCLE-COST), marked with a +
when its semantics call ELAPSE, so the real cost is only
known at run time. Data and non-emitting rows leave it blank."
  (let* ((digits (%listing-hex-digits (assembly-cell-width assembly)))
         (*listing-cycles* cycles)
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

(defun print-listing (assembly &key (stream *standard-output*) cycles)
  "LISTING-TEXT written to STREAM (default *STANDARD-OUTPUT*) -- parallel to
PRINT-DISASSEMBLY (disassembler.lisp). Returns ASSEMBLY."
  (listing-text assembly :stream stream :cycles cycles)
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
;;; Symbols sort by binding order (SYMBOL-INFO-ORDER), which follows include
;;; and macro expansion order, so equal line numbers in different files never
;;; interleave. FILE/LINE give the invocation site and DEFINITION-FILE/
;;; DEFINITION-LINE the macro body site.
;;;
;;; PERFORMANCE -- ASSEMBLY-SYMBOLS-LIST is a full MAPHASH-and-sort per call,
;;; and ASSEMBLY-SYMBOL-GROUPS additionally calls ASSEMBLY-SYMBOL (itself a
;;; hash lookup) once per scope from inside a SORT key function -- fine at
;;; the program sizes LASM currently targets, same tradeoff LISTING-LINE-AT
;;; already makes (#88) and no worse; ticket 295 tracks an
;;; address/scope-indexed structure if either ever shows up as a hot path.

(defun assembly-symbol (assembly name &key scope)
  "Return SYMBOL-INFO for NAME. SCOPE selects a local under that global;
without it, NAME identifies a global or top-level assignment."
  (let ((info (assembly-symbol-info assembly)))
    (and info (gethash (if scope (%qualify-local scope name 0) name) info))))

(defun assembly-symbols-list (assembly &key kind (scope :any scope-given-p))
  "Every SYMBOL-INFO in ASSEMBLY, in SYMBOL-INFO-ORDER (binding order; see this
file's header comment). KIND, when given
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
    (sort result #'< :key #'symbol-info-order)))

(defun assembly-symbol-groups (assembly)
  "ASSEMBLY's symbols (#37) grouped by enclosing scope, as an alist of
(GLOBAL-NAME . SYMBOL-INFO-LIST) -- the ticket's ask: a listing / source-map
pass can group locals under their enclosing global label rather than print
a flat, ambiguous list. One entry per global label that has at least one
local (or itself), the global's own SYMBOL-INFO heading its list followed
by its locals (binding order); a leading (NIL . ...) entry holds
every top-level symbol (a global with no locals still appears here via its
own binding, plus every top-level .EQU) -- present, though possibly empty,
even when ASSEMBLY-SYMBOL-INFO is NIL. Entries after the leading NIL bucket
are ordered by their global's own binding order."
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
                                          #'< :key #'symbol-info-order)))))
                  (sort scopes #'<
                        :key (lambda (scope)
                               (let ((g (assembly-symbol assembly scope)))
                                 (if g (symbol-info-order g) 0))))))))

(defun assembly-label-at (assembly address &key region bank)
  "The nearest :LABEL at or before ADDRESS in ASSEMBLY, as (VALUES SYMBOL-INFO
OFFSET), or NIL when none precedes it. REGION and BANK select a bank of a
banked region; by default only main-image labels match. A local label wins a
tie with its global, then the later binding."
  ;; TODO: full symbol sort per call, address-indexed lookup (ticket 295)
  (let (best)
    (dolist (info (assembly-symbols-list assembly :kind :label))
      (when (and (<= (symbol-info-value info) address)
                 (eq (symbol-info-region info) region)
                 (eql (symbol-info-bank info) bank)
                 (or (null best)
                     (>= (symbol-info-value info) (symbol-info-value best))))
        (setf best info)))
    (and best (values best (- address (symbol-info-value best))))))

(defun machine-label-at (machine address &key memory assembly)
  "The nearest label at or before ADDRESS in MACHINE's MEMORY, as
ASSEMBLY-LABEL-AT's (VALUES SYMBOL-INFO OFFSET) -- the banked image first, then
the main one, resolving ADDRESS and ASSEMBLY as MACHINE-LISTING-LINE does."
  (multiple-value-bind (assembly program) (%machine-image machine address memory assembly)
    (when assembly
      (multiple-value-bind (listed region bank) (%machine-image-address machine address memory program)
        (multiple-value-bind (info offset)
            (and region (assembly-label-at assembly listed :region region :bank bank))
          (if info
              (values info offset)
              (assembly-label-at assembly listed)))))))

(defun label-offset-text (info offset)
  "INFO's qualified name, with +OFFSET when OFFSET is not zero."
  (format nil "~A~[~:;+~:*~D~]" (symbol-info-qualified-name info) offset))

(defun %symbol-value-text (info digits)
  "INFO's VALUE rendered DIGITS-wide hex for a :LABEL (an address, matching
LISTING-TEXT's own address rendering), or plain decimal for an assignment (not an
address, so hex width has no natural meaning)."
  (if (eq (symbol-info-kind info) :label)
      (format nil "~@[~2,'0X:~]~V,'0X" (symbol-info-bank info) digits (symbol-info-value info))
      (format nil "~D" (symbol-info-value info))))

(defun %symbol-site-text (file line)
  (if file (format nil "~A:~D" file line) (format nil "line ~D" line)))

(defun %symbol-location-text (info)
  "INFO's invocation site, plus its macro body site when it has one."
  (let ((site (%symbol-site-text (symbol-info-file info) (symbol-info-line info)))
        (body (symbol-info-definition-line info)))
    (if body
        (format nil "~A (body ~A)" site (%symbol-site-text (symbol-info-definition-file info) body))
        site)))

(defun symbols-text (assembly &key stream)
  "Render ASSEMBLY's symbol table (#37), grouped by scope
(ASSEMBLY-SYMBOL-GROUPS): top-level symbols first, then each global label
with its locals indented underneath, each row naming a symbol, its value
(hex for a :LABEL, decimal for an assignment), its KIND, and its source
location (file:line, plus the macro body site when expanded). Returns the text as a
string when STREAM is NIL (default); otherwise writes to STREAM and returns
NIL. Empty string/no output when ASSEMBLY-SYMBOL-INFO is NIL."
  (let* ((digits (%listing-hex-digits (assembly-cell-width assembly)))
         (groups (assembly-symbol-groups assembly))
         (body (with-output-to-string (s)
                 (dolist (group groups)
                   (destructuring-bind (scope . symbols) group
                     (declare (ignore scope))
                     (dolist (sym symbols)
                       (format s "~:[  ~;~]~A~24T~A~30T~(~A~)~40T~A~%"
                               (null (symbol-info-scope sym))
                               (symbol-info-name sym)
                               (%symbol-value-text sym digits)
                               (symbol-info-kind sym)
                               (%symbol-location-text sym))))))))
    (if stream (progn (write-string body stream) nil) body)))

(defun print-symbols (assembly &key (stream *standard-output*))
  "SYMBOLS-TEXT written to STREAM (default *STANDARD-OUTPUT*) -- parallel to
PRINT-LISTING/PRINT-DISASSEMBLY. Returns ASSEMBLY."
  (symbols-text assembly :stream stream)
  assembly)
