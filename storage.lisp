;;;; storage.lisp
;;;; Storage descriptors and runtime state for lasm machines.

(in-package #:lasm)

;;; Conditions

(define-condition lasm-error (error) ())

(define-condition storage-error (lasm-error)
  ((machine :initarg :machine :reader storage-error-machine)
   (name :initarg :name :reader storage-error-name)))

(define-condition unknown-storage (storage-error) ()
  (:report (lambda (c s)
             (format s "Unknown storage element ~S on machine ~S"
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition address-out-of-range (storage-error)
  ((address :initarg :address :reader address-out-of-range-address))
  (:report (lambda (c s)
             (format s "Address ~S out of range for memory ~S on machine ~S"
                     (address-out-of-range-address c)
                     (storage-error-name c) (storage-error-machine c)))))

;; #107: signalled by (SETF MREF) for a store into a :ROM region declaring
;; :ON-WRITE :ERROR -- the default :ON-WRITE :IGNORE silently drops the
;; store instead (a ROM's writes are just discarded, matching real ROM
;; behavior); :ERROR is the opt-in for catching a program that shouldn't be
;; storing there. Mirrors ADDRESS-OUT-OF-RANGE's shape.
(define-condition memory-write-protected (storage-error)
  ((address :initarg :address :reader memory-write-protected-address))
  (:report (lambda (c s)
             (format s "Write to address ~S rejected by read-only region on ~
memory ~S on machine ~S"
                     (memory-write-protected-address c)
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition stack-overflow (storage-error) ()
  (:report (lambda (c s)
             (format s "Stack overflow on ~S (machine ~S)"
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition stack-underflow (storage-error) ()
  (:report (lambda (c s)
             (format s "Stack underflow on ~S (machine ~S)"
                     (storage-error-name c) (storage-error-machine c)))))

;; #50: signalled by STACK-REF/(SETF STACK-REF) for an OFFSET outside the
;; stack's live region -- distinct from STACK-UNDERFLOW (which is specifically
;; "popped an empty stack") since an out-of-range indexed access is a
;; different program bug, e.g. reading three deep into a stack that only has
;; one live entry. Mirrors ADDRESS-OUT-OF-RANGE's shape.
(define-condition stack-index-out-of-range (storage-error)
  ((index :initarg :index :reader stack-index-out-of-range-index))
  (:report (lambda (c s)
             (format s "Stack index ~S out of range for stack ~S on machine ~S"
                     (stack-index-out-of-range-index c)
                     (storage-error-name c) (storage-error-machine c)))))

(define-condition stack-pointer-out-of-range (storage-error)
  ((value :initarg :value :reader stack-pointer-out-of-range-value))
  (:report (lambda (c s)
             (format s "Stack pointer ~S out of range for stack ~S on machine ~S"
                     (stack-pointer-out-of-range-value c)
                     (storage-error-name c) (storage-error-machine c)))))

;; #13: signalled by REGREF/(SETF REGREF) for an INDEX outside a banked
;; register's [0, count) range. Mirrors STACK-INDEX-OUT-OF-RANGE's shape.
(define-condition register-index-out-of-range (storage-error)
  ((index :initarg :index :reader register-index-out-of-range-index))
  (:report (lambda (c s)
             (format s "Register index ~S out of range for register ~S on machine ~S"
                     (register-index-out-of-range-index c)
                     (storage-error-name c) (storage-error-machine c)))))

;; Signalled by (SETF CURRENT-BANK) and the BANK-PEEK accessors for a bank
;; index outside a banked region's [0, banks) range. NAME is the region.
(define-condition bank-out-of-range (storage-error)
  ((bank :initarg :bank :reader bank-out-of-range-bank))
  (:report (lambda (c s)
             (format s "Bank ~S out of range for region ~S on machine ~S"
                     (bank-out-of-range-bank c)
                     (storage-error-name c) (storage-error-machine c)))))

;; #108: signalled by DEVICE-AT's checked callers (DETACH-DEVICE, DEVICE-
;; INFO, DEVICE-SEND) for a bus INDEX that is out of range or a detached
;; hole (DETACH-DEVICE leaves one rather than compacting the bus, so a
;; running program's cached indices stay valid -- see device.lisp). Not a
;; STORAGE-ERROR: a device isn't a storage element, so there is no NAME to
;; report, only the bus INDEX.
(define-condition no-such-device (lasm-error)
  ((machine :initarg :machine :reader no-such-device-machine)
   (index :initarg :index :reader no-such-device-index))
  (:report (lambda (c s)
             (format s "No device at bus index ~S on machine ~S"
                     (no-such-device-index c) (no-such-device-machine c)))))

;; Generalized trap primitive placeholder. M6 replaces this with a full
;; interrupt/exception model (deftrap/definterrupt, vectors, priority);
;; for now `trap` just signals this condition with a tag and optional data.
(define-condition lasm-trap (lasm-error)
  ((tag :initarg :tag :reader lasm-trap-tag)
   (data :initarg :data :initform nil :reader lasm-trap-data))
  (:report (lambda (c s) (format s "Trap: ~S ~S" (lasm-trap-tag c) (lasm-trap-data c)))))

;; #109: signalled by SIGNAL-INTERRUPT (interrupt.lisp) when a machine's
;; (interrupts ...) clause declares :ON-OVERFLOW :ERROR (the default) and
;; the pending queue is already at its declared :QUEUE depth. Mirrors
;; STACK-OVERFLOW's shape -- MACHINE names the machine, no further detail is
;; needed since a queue has no addressable slots to report.
(define-condition interrupt-queue-full (lasm-error)
  ((machine :initarg :machine :reader interrupt-queue-full-machine))
  (:report (lambda (c s)
             (format s "Interrupt queue full on machine ~S"
                     (interrupt-queue-full-machine c)))))

;; LASM-SYNTAX-ERROR, LEX-ERROR and PARSE-FAILURE (formerly defined here) now
;; live in diagnostic.lisp, loaded immediately after this file -- they moved
;; there to sit alongside DIAGNOSTIC-TEXT, their shared report renderer (#74).

;;; Storage element descriptors

(defstruct storage-element
  (name nil :type symbol)
  (kind nil :type (member :register :stack :memory :flag))
  (width nil :type (or null (integer 1)))
  ;; :count > 1 marks a banked/array register (e.g. CHIP8's V0-VF, #13). A
  ;; banked register allocates :count cells (MAKE-STORAGE-SLOT) and is
  ;; accessed by runtime index through REGREF/(SETF REGREF), not SREF --
  ;; SREF is scalar-only and errors on a banked element. WITH-MACHINE-
  ;; BINDINGS (semantics.lisp) binds a :count 1 register as a symbol-macro
  ;; but a :count > 1 register as a MACROLET expanding to REGREF, since
  ;; symbol-macrolet can't express an indexed form like (V x).
  (count 1 :type (integer 1))
  ;; #72: an optional :names (A B C ...) on a banked register clause, one
  ;; alias symbol per bank cell in index order -- CHIP8's V0-VF, DCPU-16's
  ;; A/B/C/X/Y/Z/I/J. NIL when the clause declares none. MACHINE-DESCRIPTOR-
  ;; REGISTER-ALIASES (below) is the flat name -> index table built from
  ;; this; NAMES itself is kept on the element for error messages and for
  ;; WITH-MACHINE-BINDINGS (semantics.lisp) to walk when binding each
  ;; alias's symbol-macro.
  (names nil :type list)
  (depth nil :type (or null (integer 1)))       ; stacks
  (addr-width nil :type (or null (integer 1)))  ; memory
  (cell-width nil :type (or null (integer 1)))  ; memory
  (endian nil :type (or null keyword cons))     ; memory, :little/:big or (outer inner group), #66
  ;; #107: sub-ranges of a memory element with distinct access behavior --
  ;; ROM (writes discarded or rejected), a device window (reads/writes
  ;; forwarded to handlers instead of touching backing storage). NIL on every
  ;; machine before this ticket and on any memory element declaring no
  ;; (region ...) forms, which is what keeps MREF/(SETF MREF)'s no-region
  ;; path a single NULL test with no added indirection. Declaration order
  ;; doesn't matter for lookup (%REGION-AT scans all of them), only for
  ;; MACHINE-MODEL.MD's rendering of them.
  (regions nil :type list))

;; #107: one declared (region NAME start end ...) form inside a memory
;; clause -- see PARSE-MEMORY-CLAUSE (machine.lisp) for how a DEFMACHINE
;; form becomes this. START/END are both inclusive and within the memory
;; element's own address range; regions never overlap (machine.lisp checks
;; this once, at DEFMACHINE time, so %REGION-AT never has to worry about
;; more than one match).
;;   :RAM    -- ordinary backing-array storage, same as no region at all.
;;              Useful to name a sub-range, or to bank it with :BANKS.
;;   :ROM    -- reads hit backing storage; writes are dropped (:ON-WRITE
;;              :IGNORE, the default) or signal MEMORY-WRITE-PROTECTED
;;              (:ON-WRITE :ERROR). LOAD-PROGRAM/the debugger burn a ROM
;;              image in via %POKE, which bypasses this -- a ROM image is
;;              burned, not stored by the CPU.
;;   :DEVICE -- reads and writes are forwarded to READ/WRITE instead of
;;              touching backing storage at all; a device region with no
;;              READ reads as 0, one with no WRITE discards the store. READ
;;              and WRITE are function designators (a symbol naming a
;;              function, or a function object) called as (FUNCALL READ
;;              MACHINE ADDRESS) and (FUNCALL WRITE MACHINE ADDRESS VALUE) --
;;              both the *absolute* address, not a region-relative offset.
;;              A symbol, not #'NAME, is the form to write in a DEFMACHINE
;;              clause -- DEFMACHINE quotes its whole clause body
;;              (see its EVAL-WHEN expansion), so #'NAME there would freeze
;;              to the literal list (FUNCTION NAME) rather than an actual
;;              function; a bare symbol survives quoting unevaluated and
;;              FUNCALL resolves it to the live function at call time. Kept
;;              to a plain function-designator pair rather than a device
;;              object so #108's device model can layer over this hook
;;              without this ticket knowing devices exist.
;; BANKS, on a :RAM or :ROM region, replaces the region's window of backing
;; storage with that many separate arrays, one live at a time (machine.lisp's
;; MACHINE-BANKS holds them and the live index). NIL when not banked.
(defstruct memory-region
  (name nil :type symbol)
  (start nil :type (integer 0))
  (end nil :type (integer 0))
  (kind :ram :type (member :ram :rom :device))
  (banks nil :type (or null (integer 1)))
  (on-write :ignore :type (member :ignore :error))     ; :rom only
  (read nil :type (or null symbol function))           ; :device only
  (write nil :type (or null symbol function)))         ; :device only

;; #108: a machine's declared (device ...) clause (machine.lisp) -- identity
;; (the ID/VERSION/MANUFACTURER triple an HWQ-style instruction reads back,
;; #135's confirmed shape) plus four hooks, each a function *designator*
;; (a bare symbol, not #'NAME) for the same quoting reason MEMORY-REGION's
;; READ/WRITE are (see that struct's comment and DEFMACHINE's docstring,
;; machine.lisp). INIT/TICK/RECEIVE/DETACH are all optional -- a device
;; declaring none is inert but still enumerable.
;;   INIT    (machine device) -> value, stored as the runtime DEVICE's own
;;           STATE -- called once by %ATTACH-DEVICE-DESCRIPTOR, both at
;;           MAKE-MACHINE and on every RESET (storage.lisp).
;;   TICK    (machine device cycles) -- called once per STEP-MACHINE
;;           (emulator.lisp) with that step's own cycle cost.
;;   RECEIVE (machine device) -- an HWI-style message send (DEVICE-SEND,
;;           device.lisp); a device with no RECEIVE ignores it.
;;   DETACH  (machine device) -- called by DETACH-DEVICE just before the
;;           bus slot is cleared to a hole.
;;   SAVE    (machine device) -> plain readable data, stored by
;;           MACHINE-SNAPSHOT (snapshot.lisp) as the device's state.
;;   LOAD    (machine device data) -- called by RESTORE-SNAPSHOT on a freshly
;;           INIT'd device with the data SAVE returned. A device without
;;           both hooks is re-INIT'd on restore and carries no saved state.
(defstruct device-descriptor
  (name nil :type symbol)
  (id 0 :type (integer 0))
  (version 0 :type (integer 0))
  (manufacturer 0 :type (integer 0))
  (init nil :type (or null symbol function))
  (tick nil :type (or null symbol function))
  (receive nil :type (or null symbol function))
  (detach nil :type (or null symbol function))
  (save nil :type (or null symbol function))
  (load nil :type (or null symbol function)))

;; #109: a machine's declared (interrupts ...) clause (machine.lisp) -- the
;; vector/message/save registers are held here as plain symbol names by
;; PARSE-INTERRUPTS-CLAUSE, then resolved against the machine's own storage
;; elements by %FINISH-INTERRUPT-MODEL once ELEMENTS is known (the same
;; two-pass split as %FINISH-INSTRUCTION-WORD-LAYOUT), so every use of a
;; slot below except VECTOR/MESSAGE/SAVE/STACK-NAME/MASK-FLAG can assume
;; those names are already valid.
;;   VECTOR       register holding the handler address written to PC.
;;   MESSAGE      register a delivered signal's DATA is written to.
;;   SAVE         list of register/flag names pushed, in order, before
;;                MESSAGE/VECTOR are written -- INTERRUPT-RETURN (semantics.
;;                lisp) pops them in reverse.
;;   STACK-NAME   which STACK element SAVE pushes onto/INTERRUPT-RETURN pops
;;                from; NIL defers to the machine's sole stack the same way
;;                WITH-MACHINE-BINDINGS's PUSH/POP do.
;;   QUEUE-DEPTH  max pending signals; a positive integer.
;;   ON-OVERFLOW  policy when SIGNAL-INTERRUPT (interrupt.lisp) would exceed
;;                QUEUE-DEPTH -- :ERROR (signal INTERRUPT-QUEUE-FULL),
;;                :TRAP (signal LASM-TRAP), :DROP (discard the incoming
;;                signal), or :DROP-OLDEST (evict the queue's head first).
;;   MASK-WHEN    function designator (machine) -> generalized boolean,
;;                or NIL. At most one of MASK-WHEN/MASK-FLAG is non-NIL.
;;   MASK-FLAG    a flag name read the same way, or NIL.
;;   CYCLES       extra MACHINE-CYCLES cost of delivery itself; 0 by default.
;;   DROP-ON-ZERO-VECTOR  when true (the default), SIGNAL-INTERRUPT drops a
;;                signal outright, before it ever reaches the queue, while
;;                VECTOR's register currently reads 0 -- DCPU-16/ANIMA-16's
;;                "IA == 0 means interrupts are off" convention. A masked-
;;                but-nonzero-vector machine still queues normally; this
;;                knob exists because masking alone can't express "off"
;;                without risking a queue that fills and hits ON-OVERFLOW.
;;   STACK-KIND   :STACK (STACK-NAME names a lasm :STACK element, pushed/
;;                popped via STACK-PUSH/STACK-POP -- the original #109
;;                behavior) or :POINTER (STACK-NAME names a register bound by
;;                a (stack-pointer ...) clause, pushed/popped via SP-PUSH/
;;                SP-POP against that clause's own memory/direction; #166).
(defstruct interrupt-descriptor
  ;; A place is a scalar register/flag name, or (NAME INDEX) for one cell of
  ;; a banked register (#163).
  (vector nil :type (or symbol list))
  (message nil :type (or symbol list))
  (save nil :type list)
  (stack-name nil :type (or null symbol))
  (stack-kind :stack :type (member :stack :pointer))
  (queue-depth 256 :type (integer 1))
  (on-overflow :error :type (member :error :trap :drop :drop-oldest))
  (mask-when nil :type (or null symbol function))
  (mask-flag nil :type (or null symbol))
  (cycles 0 :type (integer 0))
  (drop-on-zero-vector t :type boolean)
  (mask-on-deliver nil :type boolean))

;; #166: a (stack-pointer REG [:memory NAME] [:grows :down/:up]) clause --
;; binds an existing scalar :register element as an address pointer into a
;; :memory element, for machines (DCPU-16, ANIMA-16) whose "stack" is a plain
;; register indexed by push/pop convention rather than a lasm :stack element.
;; Declares no new namespace name, same as INTERRUPT-DESCRIPTOR -- it only
;; references existing REGISTER/MEMORY elements.
;;   REGISTER   the bound scalar register's name.
;;   MEMORY     the memory element pushed/popped into; resolved by
;;              %FINISH-STACK-POINTERS (machine.lisp) to the sole declared
;;              :memory element when the clause omits :memory.
;;   GROWS      :DOWN (default) -- REGISTER points AT the top item: push
;;              pre-decrements then stores, pop loads then post-increments.
;;              :UP -- REGISTER points one PAST the top item: push stores
;;              then post-increments, pop pre-decrements then loads.
(defstruct stack-pointer-descriptor
  (register nil :type symbol)
  (memory nil :type (or null symbol))
  (grows :down :type (member :down :up)))

(defun %region-at (element address)
  "The MEMORY-REGION in ELEMENT containing ADDRESS, or NIL when ELEMENT
declares no regions or none of them cover ADDRESS. NIL up front on the
common case (no REGIONS at all) so an unregioned memory element's MREF/
(SETF MREF) does no scanning whatsoever.
TODO: linear region scan; bucket/page table if region counts grow (#107
follow-up)."
  (let ((regions (storage-element-regions element)))
    (and regions
         (find-if (lambda (r) (<= (memory-region-start r) address (memory-region-end r)))
                   regions))))

;; A machine-level fixed instruction-word bit layout (#20, M4): declared via
;; DEFMACHINE's (instruction-word :width n (field name width) ...) clause
;; (machine.lisp) for a DCPU-16-shaped machine whose whole instruction is one
;; WIDTH-bit word split into named bit fields rather than a cell-per-operand
;; stream. FIELDS is a list of (name width shift) in *declared* (most-
;; significant-first) order -- SHIFT is each field's bit offset from the
;; word's LSB, derived once here so encode/decode never recompute it.
;; WIDTH-CELLS is WIDTH/CELL-WIDTH, checked to be a whole number at parse
;; time (machine.lisp) since the word is emitted as CELL-WIDTH-wide cells
;; (#53 -- the assembler pipeline is typed to the target machine's own
;; memory cell width, not fixed at 8 bits), in the machine's own ENDIAN
;; order (#66).
;; #64: NAME is NIL on the default (machine-wide) layout, and a symbol on an
;; alternate declared by a (layout NAME (field ...)...) form. ALTERNATES holds
;; the machine's other layouts (each its own INSTRUCTION-WORD-LAYOUT, NAME
;; non-NIL) and is non-NIL only on the default -- an alternate's own
;; ALTERNATES is always NIL, so there is exactly one place to look up a
;; sibling from either side. Every alternate shares WIDTH/WIDTH-CELLS/
;; CELL-WIDTH and an OPCODE field identical in width and shift to the
;; default's (machine.lisp validates this at DEFMACHINE time) -- only the
;; fields below OPCODE vary per layout.
(defstruct instruction-word-layout
  (name nil :type symbol)
  (width nil :type (integer 1))
  (width-cells nil :type (integer 1))
  (cell-width nil :type (integer 1))
  (endian nil :type (or null keyword cons)) ; :little/:big or (outer inner group), #66
  (fields nil :type list)           ; (name width shift), MSB-first as declared
  ;; #191: field names, in the order their trailing words follow the
  ;; instruction word. NIL (the default) is operand-hole order. Default
  ;; layout only -- it applies machine-wide, resolved per descriptor by
  ;; field name (%WORD-EMIT-ORDER, instruction.lisp).
  (extra-word-order nil :type list)
  (alternates nil :type list))      ; list of INSTRUCTION-WORD-LAYOUT, default only

(defun instruction-word-field (layout name)
  "The (name width shift) entry in LAYOUT's FIELDS named NAME, or NIL."
  (find name (instruction-word-layout-fields layout) :key #'first))

(defun instruction-word-layout-named (layout name)
  "LAYOUT itself when NAME is NIL, else the alternate in LAYOUT's ALTERNATES
named NAME, or NIL if no such alternate exists. LAYOUT is always the
machine's default layout -- callers hold no other kind (#64)."
  (if (null name)
      layout
      (find name (instruction-word-layout-alternates layout) :key #'instruction-word-layout-name)))

(defstruct machine-descriptor
  (name nil :type symbol)
  (elements nil :type list)               ; ordered list of storage-element
  (table (make-hash-table :test 'eq))     ; name -> storage-element
  ;; Instruction registration (instruction.lisp). Keyed by upcased mnemonic
  ;; string and by opcode, so both the assembler (assembler.lisp, mnemonic ->
  ;; encoding) and the emulator (emulator.lisp, opcode -> decode) share one
  ;; table pair rather than each keeping its own index.
  ;; mnemonic string -> list of instruction-descriptor, one per addressing
  ;; mode the mnemonic accepts (mode.lisp/M2); a no-operand or single-mode
  ;; mnemonic's list has exactly one element.
  (instructions (make-hash-table :test 'equal))
  ;; opcode -> list of instruction-descriptor, one per co-tenant decode-
  ;; distinguishable descriptor sharing that opcode (#105) -- more than one
  ;; entry only on a word-encoded machine, where %CHECK-OPCODE-DECODABLE!
  ;; (instruction.lisp) requires every pair sharing a list to disagree on
  ;; some operand field's accepted raw bits so DECODE-INSTRUCTION-AT
  ;; (decoder.lisp) can tell them apart by the bits actually fetched. A
  ;; byte-encoded machine's opcode table has exactly one entry per key --
  ;; there is no per-field discriminator to decode by, so
  ;; REGISTER-INSTRUCTION-VARIANTS! rejects any second descriptor at an
  ;; opcode outright there, regardless of mnemonic or mode.
  (opcodes (make-hash-table :test 'eql))
  (word-decode-table nil :type (or null simple-vector))
  ;; NIL for an ordinary byte-encoded machine (every machine before #20) --
  ;; DEFINSTRUCTION/the assembler/the emulator all branch on this being NIL
  ;; vs. an INSTRUCTION-WORD-LAYOUT to pick between the two encoding schemes.
  (instruction-word nil :type (or null instruction-word-layout))
  ;; #75: NIL unless DEFMACHINE declares a (clock-speed n) clause -- the
  ;; machine's nominal rate in Hz. NIL is what keeps cycle-accurate execution
  ;; a zero-cost opt-in subsystem (LASM-plan.md sec. 1, pillar 4):
  ;; RUN-FOR-DURATION requires this to be set (it has no other way to convert
  ;; cycles to seconds), while RUN-FOR-CYCLES and the plain cycle count on
  ;; MACHINE-CYCLES below need no clock speed at all.
  (clock-speed nil :type (or null (integer 1)))
  ;; #108: DEVICE-DESCRIPTORs from every (device ...) clause, in declaration
  ;; order -- that order is a runtime MACHINE's initial bus index order (see
  ;; %ATTACH-DEVICE-DESCRIPTOR below and MAKE-MACHINE). NIL on a machine
  ;; declaring none.
  (devices nil :type list)
  ;; #109: NIL unless DEFMACHINE declares an (interrupts ...) clause -- the
  ;; machine's whole interrupt model (vector/message/save registers, queue
  ;; depth/overflow policy, masking, delivery cost). NIL is what keeps
  ;; DELIVER-PENDING-INTERRUPT (interrupt.lisp, called from STEP-MACHINE) a
  ;; single NULL test on a machine declaring no interrupt model, the same
  ;; way DEVICES being NIL keeps TICK-DEVICES a no-op loop.
  (interrupts nil :type (or null interrupt-descriptor))
  ;; #166: register name -> STACK-POINTER-DESCRIPTOR, one entry per declared
  ;; (stack-pointer ...) clause. Consulted by %RESOLVE-INTERRUPT-STACK
  ;; (machine.lisp) when (interrupts ...)'s :stack names a register rather
  ;; than a :stack element, and by WITH-MACHINE-BINDINGS's PUSH/POP
  ;; (semantics.lisp) so a stack-pointer works with or without an
  ;; (interrupts ...) clause. Empty (never NIL) on a machine declaring none.
  (stack-pointers (make-hash-table :test 'eq))
  ;; #72: alias name (upcased string) -> bank index, flattened across every
  ;; banked register's :names -- one machine-wide table, since an alias is
  ;; unique across the whole machine (BUILD-MACHINE-DESCRIPTOR's SEEN check),
  ;; so the index alone is enough for EVAL-EXPR (instruction.lisp) to resolve
  ;; "a" without also knowing which register it names. EQUALP so lookup is
  ;; case-insensitive, matching how mnemonics and mode literals already
  ;; compare (STRING-EQUAL). Empty (never NIL) on a machine with no aliased
  ;; register.
   (register-aliases (make-hash-table :test 'equalp))
   ;; Alias name -> owning register storage element, for alias-qualified mode
   ;; holes and disassembly of per-alternative register forms.
   (register-alias-elements (make-hash-table :test 'equalp))
  ;; #63: lazy memo for %DESCRIPTOR-CELL-WIDTH's no-MEMORY-NAME case
  ;; (machine.lisp) -- that path rebuilds ELEMENTS' memory sublist and calls
  ;; REMOVE-DUPLICATES on every call otherwise, and it's read once per
  ;; ENCODE-INSTRUCTION and several times per assembler relaxation pass. Two
  ;; states live here: :UNSET (never computed) and any other value (the
  ;; resolved width, cached). Safe to cache on this struct, unlike
  ;; INSTRUCTION-DESCRIPTOR-WORD-LAYOUT's deliberate non-caching
  ;; (instruction.lisp) -- DEFMACHINE rebuilds this whole struct from scratch
  ;; on redefinition (%BUILD-MACHINE-DESCRIPTOR), so there is no stale
  ;; instance for this slot to drift against.
  (cell-width-cache :unset)
  ;; #66: same memoization rationale as CELL-WIDTH-CACHE above, for
  ;; %DESCRIPTOR-ENDIAN's no-MEMORY-NAME case.
  (endian-cache :unset)
  ;; Machine families: PARENT is the name of the machine this one extends
  ;; (NIL for a standalone machine) and SOURCE-CLAUSES the fully merged clause
  ;; list a child merges onto in turn.
  (parent nil :type (or null symbol))
  (source-clauses nil :type list)
  ;; Upcased mnemonics defined directly on this machine, which parent
  ;; propagation never overwrites.
  (own-instructions (make-hash-table :test 'equal))
  ;; Upcased mnemonics removed from this machine, including inherited removals.
  (removed-instructions nil :type list)
  ;; Alist of upcased mnemonic -> cycle cost overriding inherited variants.
  (instruction-cycles nil :type list)
  ;; opcode -> descriptors of removed mnemonics, kept only so an undefined-
  ;; opcode :NOP can size them and word decode can rank them.
  (disabled-opcodes (make-hash-table :test 'eql))
  ;; What a step does on an opcode with no descriptor: :FAULT, :NOP or :TRAP.
  (undefined-opcode :fault :type (member :fault :nop :trap))
  (properties nil :type list))

(defun descriptor-element (descriptor name)
  (or (gethash name (machine-descriptor-table descriptor))
      (error 'unknown-storage :machine (machine-descriptor-name descriptor)
                               :name name)))

;; Registry of defined machine descriptors, keyed by machine name. Populated
;; by DEFMACHINE (see machine.lisp) inside an EVAL-WHEN so descriptors are
;; available at macroexpansion time -- this is what lets a later DEFINSTRUCTION
;; (M1) resolve storage names/widths for a machine defined earlier in the same
;; file, at compile time rather than only after loading.
(defvar *machines* (make-hash-table :test 'eq))

(defun find-machine-descriptor (name)
  (or (gethash name *machines*)
      (error "No machine named ~S has been defined with DEFMACHINE" name)))

(defun %machine-children (name)
  "Descriptors of every machine registered with parent NAME."
  (loop for descriptor being the hash-values of *machines*
        when (eq (machine-descriptor-parent descriptor) name)
          collect descriptor))

(defun %machine-ancestors (name)
  "Names of NAME's parent, grandparent and so on, nearest first."
  (loop for parent = (let ((d (gethash name *machines*))) (and d (machine-descriptor-parent d)))
          then (let ((d (gethash parent *machines*))) (and d (machine-descriptor-parent d)))
        while parent
        collect parent))

(defun machine-descriptor-property (descriptor key &optional default)
  "The value of KEY in DESCRIPTOR's (properties ...), or DEFAULT."
  (getf (machine-descriptor-properties descriptor) key default))

;; #108: a live device on a MACHINE's bus -- DESCRIPTOR is the DEVICE-
;; DESCRIPTOR it was attached from (a declared one, or one built inline by
;; ATTACH-DEVICE, device.lisp); INDEX is its bus position, fixed for the
;; device's lifetime (DETACH-DEVICE leaves a hole rather than renumbering
;; later devices, so a running program's cached index never goes stale).
;; STATE is whatever the descriptor's INIT hook returned -- opaque to
;; everything here, read back only by the device's own TICK/RECEIVE/DETACH.
(defstruct (device (:constructor %make-device (descriptor index)))
  (descriptor nil :type device-descriptor)
  (index nil :type (integer 0))
  (state nil))

;;; Runtime machine state

(defstruct (machine (:constructor %make-machine (descriptor)))
  (descriptor nil :type machine-descriptor)
  (slots (make-hash-table :test 'eq))     ; name -> slot representation
  ;; #75: total cycles consumed by every instruction STEP-MACHINE has
  ;; executed on this machine since the last RESET. Accumulated regardless of
  ;; whether the machine's descriptor declares a CLOCK-SPEED -- the count
  ;; itself is always meaningful, only the cycles-to-seconds conversion needs
  ;; one.
  (cycles 0 :type unsigned-byte)
  ;; #90: cycles the running instruction's semantics added with EXTRA-CYCLES
  ;; (semantics.lisp) beyond its declared cost. STEP-MACHINE zeroes it before
  ;; each execute and folds it into CYCLES afterwards.
  (extra-cycles 0 :type unsigned-byte)
  ;; #108: the device bus -- an adjustable, fill-pointered vector of DEVICE
  ;; or NIL (a detached hole, see DEVICE struct above). Seeded from the
  ;; descriptor's own DEVICES by MAKE-MACHINE/RESET below; ATTACH-DEVICE
  ;; (device.lisp) extends it, DETACH-DEVICE clears a slot in place rather
  ;; than shrinking it.
  (devices (make-array 0 :adjustable t :fill-pointer 0))
  ;; #108/#109: NIL, or a function (machine device data) called by
  ;; DEVICE-SIGNAL (device.lisp). MAKE-MACHINE below auto-installs
  ;; #'%DEFAULT-INTERRUPT-HOOK (interrupt.lisp) when the descriptor declares
  ;; an (interrupts ...) clause; a signal with no hook installed at all is
  ;; simply dropped. Host wiring, not machine state -- RESET below leaves
  ;; whatever is currently installed here alone, unconditionally, the same
  ;; way a DEBUG-SESSION's breakpoints (not the MACHINE) survive a RESET --
  ;; this holds for the auto-installed default exactly as for anything a
  ;; host replaced it with.
  (interrupt-hook nil :type (or null function))
  ;; #109: pending signals raised by SIGNAL-INTERRUPT (interrupt.lisp) but
  ;; not yet delivered -- a list of (DEVICE . DATA) conses, oldest first,
  ;; DEVICE possibly NIL for a software-raised (INT-style) signal. Capped at
  ;; the descriptor's INTERRUPT-DESCRIPTOR-QUEUE-DEPTH by SIGNAL-INTERRUPT
  ;; itself; this slot has no depth of its own. Machine state, unlike
  ;; INTERRUPT-HOOK above -- RESET clears it.
  (interrupt-queue nil :type list)
  ;; #110: set by the IDLE semantics primitive (semantics.lisp) -- STEP-
  ;; MACHINE (emulator.lisp) skips fetch/decode/execute while this is true,
  ;; but still ticks devices and accounts cycles. Cleared by DELIVER-
  ;; PENDING-INTERRUPT (interrupt.lisp) on delivery, or by WAKE-MACHINE
  ;; (emulator.lisp) directly. Machine state, like INTERRUPT-QUEUE above --
  ;; RESET clears it.
  (idle nil :type boolean)
  ;; Banked regions' runtime state: region name -> (CURRENT . ARRAYS), ARRAYS
  ;; a simple-vector of one cell array per bank. Empty when no region is banked.
  (banks (make-hash-table :test 'eq)))

;; Slot representations:
;;   :register / :flag -> a one-element (simple-vector 1) box holding an
;;                         unsigned integer
;;   :stack             -> a cons (vector . sp), vector is a fixed-size
;;                         simple-vector sized to :depth, not adjustable
;;   :memory            -> a (simple-array (unsigned-byte cell-width) (*))

(defun wrap-value (value width)
  "Mask VALUE to an unsigned WIDTH-bit integer."
  (logand value (1- (ash 1 width))))

(defun signed-value (value width)
  "Reinterpret unsigned WIDTH-bit VALUE as two's-complement signed."
  (if (logbitp (1- width) value)
      (- value (ash 1 width))
      value))

(defun make-storage-slot (element)
  (ecase (storage-element-kind element)
    (:register
     ;; :count cells -- 1 for an ordinary scalar register, more for a
     ;; banked register (#13, e.g. CHIP8's 16 V registers).
     (make-array (storage-element-count element) :initial-element 0))
    (:flag
     (make-array 1 :initial-element 0))
    (:stack
     (cons (make-array (storage-element-depth element) :initial-element 0)
           0))
    (:memory
     ;; Eager allocation of 2^addr-width cells, unconditionally -- #107's
     ;; region overlays (STORAGE-ELEMENT-REGIONS) change what MREF/(SETF
     ;; MREF) do with a range of this array, not how the array itself is
     ;; allocated, so a ROM or device region still occupies backing cells
     ;; here even though ordinary reads/writes route around them.
     (make-array (ash 1 (storage-element-addr-width element))
                 :element-type `(unsigned-byte ,(storage-element-cell-width element))
                 :initial-element 0))))

;; #109: append ENTRY (a (DEVICE . DATA) cons, DEVICE possibly NIL for a
;; software-raised signal) to MACHINE's pending interrupt queue, honoring
;; its descriptor's :DROP-ON-ZERO-VECTOR/:QUEUE/:ON-OVERFLOW policy. Lives
;; here, not interrupt.lisp, because %DEFAULT-INTERRUPT-HOOK below (which
;; MAKE-MACHINE sharp-quotes) needs it and LASM.ASD is :SERIAL T with
;; interrupt.lisp loading after this file -- the same reason %INSTANTIATE-
;; DEVICE lives here rather than in device.lisp. SIGNAL-INTERRUPT
;; (interrupt.lisp), the public device-optional entry point, is a second,
;; equally thin wrapper around this -- it can't be the other way around
;; (this calling out to interrupt.lisp) without creating the forward
;; reference this split avoids.
(defun %enqueue-interrupt (machine entry)
  (let ((interrupts (machine-descriptor-interrupts (machine-descriptor machine))))
    (unless interrupts
      (error "signal-interrupt on machine ~S: no (interrupts ...) clause declared"
             (machine-descriptor-name (machine-descriptor machine))))
    (when (and (interrupt-descriptor-drop-on-zero-vector interrupts)
               (zerop (%interrupt-place machine (interrupt-descriptor-vector interrupts))))
      (return-from %enqueue-interrupt (values)))
    ;; TODO: a plain list with LENGTH/NCONC here is O(depth) per signal --
    ;; fine at :QUEUE's modest default (256) but a real cost at a much
    ;; deeper declared queue. A ring buffer (sized to :QUEUE, tracking its
    ;; own count) would make depth checks and both ends O(1) if that ever
    ;; matters.
    (when (>= (length (machine-interrupt-queue machine)) (interrupt-descriptor-queue-depth interrupts))
      (ecase (interrupt-descriptor-on-overflow interrupts)
        (:error (error 'interrupt-queue-full
                        :machine (machine-descriptor-name (machine-descriptor machine))))
        (:trap (error 'lasm-trap :tag :interrupt-queue-overflow :data entry))
        (:drop (return-from %enqueue-interrupt (values)))
        (:drop-oldest (cl:pop (machine-interrupt-queue machine)))))
    (setf (machine-interrupt-queue machine)
          (nconc (machine-interrupt-queue machine) (list entry))))
  (values))

;; #109: the hook MAKE-MACHINE below auto-installs onto MACHINE-INTERRUPT-
;; HOOK when the descriptor declares (interrupts ...) -- DEVICE-SIGNAL
;; (device.lisp) reaches this indirectly through the hook; a software
;; INT-style instruction's semantics call SIGNAL-INTERRUPT (interrupt.lisp)
;; instead, DEVICE-optional. Both funnel into %ENQUEUE-INTERRUPT above. A
;; named top-level function, not an anonymous lambda, purely so a caller
;; (a test, or introspecting host code) can EQ-compare MACHINE-INTERRUPT-
;; HOOK against #'%DEFAULT-INTERRUPT-HOOK to tell "still the auto-installed
;; default" from "something else was installed" -- RESET itself does not
;; make this distinction (see its own docstring below): it leaves whatever
;; is currently installed alone either way.
(defun %default-interrupt-hook (machine device data)
  (%enqueue-interrupt machine (cons device data)))

;; #108: instantiate one live DEVICE from DESCRIPTOR at bus INDEX, running
;; its INIT hook (if any). Shared by MAKE-MACHINE/RESET below (seeding the
;; declared bus) and ATTACH-DEVICE (device.lisp, appending a runtime one) --
;; the one place that "run INIT, wrap the result in a DEVICE" happens, so
;; the two callers can't drift on it. Lives here rather than in device.lisp
;; since LASM.ASD is :SERIAL T with device.lisp loading after this file, and
;; MAKE-MACHINE/RESET both need it.
(defun %instantiate-device (machine descriptor index)
  (let ((device (%make-device descriptor index))
        (init (device-descriptor-init descriptor)))
    (when init
      (setf (device-state device) (funcall init machine device)))
    device))

(defun machine-property (thing key &optional default)
  "The value of KEY in the (properties ...) of THING -- a runtime machine, a
machine descriptor or a machine name -- or DEFAULT."
  (machine-descriptor-property
   (etypecase thing
     (machine (machine-descriptor thing))
     (machine-descriptor thing)
     (symbol (find-machine-descriptor thing)))
   key default))

;; Bank storage: one array per bank, sized to the region's window.
(defun %banked-regions (descriptor)
  (loop for element in (machine-descriptor-elements descriptor)
        when (eq (storage-element-kind element) :memory)
          append (loop for region in (storage-element-regions element)
                       when (memory-region-banks region)
                         collect (cons element region))))

(defun %allocate-banks (machine)
  (loop for (element . region) in (%banked-regions (machine-descriptor machine))
        do (let ((size (1+ (- (memory-region-end region) (memory-region-start region))))
                 (type `(unsigned-byte ,(storage-element-cell-width element))))
             (setf (gethash (memory-region-name region) (machine-banks machine))
                   (cons 0 (coerce (loop repeat (memory-region-banks region)
                                         collect (make-array size :element-type type
                                                                  :initial-element 0))
                                   'simple-vector))))))

(defun make-machine (name)
  "Instantiate runtime state for the machine descriptor registered under NAME."
  (let ((descriptor (find-machine-descriptor name)))
    (let ((m (%make-machine descriptor)))
      (dolist (element (machine-descriptor-elements descriptor))
        (setf (gethash (storage-element-name element) (machine-slots m))
              (make-storage-slot element)))
      (%allocate-banks m)
      ;; #108: seed the bus from every declared (device ...) clause, in
      ;; declaration order -- that order becomes each device's fixed index.
      (dolist (device-descriptor (machine-descriptor-devices descriptor))
        (vector-push-extend
         (%instantiate-device m device-descriptor (fill-pointer (machine-devices m)))
         (machine-devices m)))
      ;; #109: auto-wire the real delivery hook when the descriptor declares
      ;; an (interrupts ...) clause -- no host boilerplate needed. A machine
      ;; declaring no such clause gets no hook, exactly as #108 left it.
      (when (machine-descriptor-interrupts descriptor)
        (setf (machine-interrupt-hook m) #'%default-interrupt-hook))
      m)))

(defun reset (machine)
  "Zero all storage on MACHINE, including the #75 cycle counter -- which
lives on the MACHINE struct itself rather than as a storage element, so the
loop below (driven off MACHINE-DESCRIPTOR-ELEMENTS) never sees it and must
be told separately.

#108: also restores the device bus to its *declared* shape -- any runtime-
attached device (ATTACH-DEVICE, device.lisp) is dropped, every hole is
refilled, and every declared device's INIT hook runs again, exactly as if a
fresh MAKE-MACHINE had built the bus. MACHINE-INTERRUPT-HOOK is untouched --
it's host wiring (who the bus signals), not machine state, so it survives a
RESET the same way a DEBUG-SESSION's breakpoints do; this holds for #109's
auto-installed #'%DEFAULT-INTERRUPT-HOOK exactly as for a host's own hook,
so a host that replaced it (including with NIL, to disable delivery) keeps
that choice across a RESET. #109's pending INTERRUPT-QUEUE, unlike the
hook, *is* machine state and is cleared unconditionally below -- and so is
#110's IDLE flag."
  (dolist (element (machine-descriptor-elements (machine-descriptor machine)))
    (let ((slot (gethash (storage-element-name element) (machine-slots machine))))
      (ecase (storage-element-kind element)
        ((:register :flag) (fill slot 0))
        (:stack (fill (car slot) 0) (setf (cdr slot) 0))
        (:memory (fill slot 0)))))
  (loop for entry being the hash-values of (machine-banks machine)
        do (setf (car entry) 0)
           (map nil (lambda (bank) (fill bank 0)) (cdr entry)))
  (setf (machine-cycles machine) 0)
  (setf (machine-extra-cycles machine) 0)
  (let ((devices (machine-devices machine)))
    (setf (fill-pointer devices) 0)
    (dolist (device-descriptor (machine-descriptor-devices (machine-descriptor machine)))
      (vector-push-extend
       (%instantiate-device machine device-descriptor (fill-pointer devices))
       devices)))
  (setf (machine-interrupt-queue machine) nil)
  (setf (machine-idle machine) nil)
  machine)

;;; Accessors

(defun %slot (machine name kind)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (eq (storage-element-kind element) kind)
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun sref (machine name)
  "Read a scalar register or flag by NAME as an unsigned integer. Signals
UNKNOWN-STORAGE on a banked (:count > 1) register -- use REGREF instead."
  (multiple-value-bind (slot element) (%slot-any machine name)
    (declare (ignore element))
    (aref slot 0)))

(defun %slot-any (machine name)
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (unless (member (storage-element-kind element) '(:register :flag))
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    ;; SREF/(SETF SREF) are the scalar accessor -- a banked register (#13)
    ;; has no single cell 0 answer, so treat it as unaddressable by this
    ;; path rather than silently aliasing every index to one cell.
    (when (> (storage-element-count element) 1)
      (error 'unknown-storage :machine (machine-descriptor-name (machine-descriptor machine))
                               :name name))
    (values (gethash name (machine-slots machine)) element)))

(defun (setf sref) (value machine name)
  (multiple-value-bind (slot element) (%slot-any machine name)
    (setf (aref slot 0) (wrap-value value (storage-element-width element)))))

;; #13: indexed access into a banked (:count > 1) register, e.g. CHIP8's
;; V0-VF or DCPU-16's A/B/C/X/Y/Z/I/J. INDEX is evaluated at run time --
;; unlike STACK-REF's top-relative OFFSET, this is a plain 0-based bank
;; index (0 = the register's first element) since a banked register has no
;; notion of "top". Mirrors SREF/MREF's shape.
(defun regref (machine name index)
  "Read banked register NAME on MACHINE at bank INDEX as an unsigned integer."
  (multiple-value-bind (slot element) (%slot machine name :register)
    (unless (and (>= index 0) (< index (storage-element-count element)))
      (error 'register-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                           :name name :index index))
    (aref slot index)))

(defun (setf regref) (value machine name index)
  (multiple-value-bind (slot element) (%slot machine name :register)
    (unless (and (>= index 0) (< index (storage-element-count element)))
      (error 'register-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                           :name name :index index))
    (setf (aref slot index) (wrap-value value (storage-element-width element)))))

;; #163: an interrupt :VECTOR/:MESSAGE/:SAVE place -- a scalar name read
;; through SREF, or (NAME INDEX) naming one bank cell read through REGREF.
(defun %interrupt-place (machine place)
  (if (consp place)
      (regref machine (first place) (second place))
      (sref machine place)))

(defun (setf %interrupt-place) (value machine place)
  (if (consp place)
      (setf (regref machine (first place) (second place)) value)
      (setf (sref machine place) value)))

;; #143: the read direction of #72's NAMES -- resolving a decoded bank INDEX
;; back to its alias, for the disassembler (and #144's debugger) to render
;; symbolically instead of as a bare integer. Takes the STORAGE-ELEMENT
;; itself, not a machine/name pair, since a caller here typically already has
;; it (e.g. off an INSTRUCTION-DESCRIPTOR's own machine) and REGREF's
;; MACHINE/NAME indirection would be pure overhead.
(defun register-alias-at (element index)
  "ELEMENT's #72 :NAMES alias at bank INDEX, downcased, or NIL when ELEMENT
declares no NAMES or INDEX is outside them."
  (let ((names (storage-element-names element)))
    (and names (>= index 0) (< index (length names))
         (string-downcase (symbol-name (nth index names))))))

(defun flag (machine name)
  "Read a flag by NAME as 0 or 1."
  (multiple-value-bind (slot element) (%slot machine name :flag)
    (declare (ignore element))
    (aref slot 0)))

(defun (setf flag) (value machine name)
  (multiple-value-bind (slot element) (%slot machine name :flag)
    (declare (ignore element))
    (setf (aref slot 0) (if (or (null value) (and (integerp value) (zerop value))) 0 1))))

;; #107: shared bounds-checked lookup for MREF/(SETF MREF)/MPEEK/%POKE --
;; keeps the ADDRESS-OUT-OF-RANGE check and %SLOT call in one place so the
;; four memory accessors below can't drift on it. Returns (VALUES SLOT
;; ELEMENT REGION), REGION being the #107 MEMORY-REGION covering ADDRESS (or
;; NIL).
(defun %memory-slot-checked (machine name address)
  (multiple-value-bind (slot element) (%slot machine name :memory)
    (unless (and (>= address 0) (< address (length slot)))
      (error 'address-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                    :name name :address address))
    (values slot element (%region-at element address))))

(defun %live-bank (machine region)
  "The cell array of REGION's currently selected bank."
  (let ((entry (gethash (memory-region-name region) (machine-banks machine))))
    (svref (cdr entry) (car entry))))

(defun mref (machine name address)
  "Read memory element NAME on MACHINE at ADDRESS. #107: an address falling
in a :DEVICE region calls that region's READ instead of touching backing
storage (0 when the region declares no READ); an address in a banked region
reads the live bank; every other address -- including one in an unbanked
:RAM or :ROM region -- reads backing storage directly. A device read is
masked to the cell width, as writes are."
  (multiple-value-bind (slot element region) (%memory-slot-checked machine name address)
    (cond
      ((null region) (aref slot address))
      ((eq (memory-region-kind region) :device)
       (let ((read (memory-region-read region)))
         (wrap-value (if read (funcall read machine address) 0)
                     (storage-element-cell-width element))))
      ((memory-region-banks region)
       (aref (%live-bank machine region) (- address (memory-region-start region))))
      (t (aref slot address)))))

(defun (setf mref) (value machine name address)
  "Write memory element NAME on MACHINE at ADDRESS. #107: a store into a
:ROM region is dropped (:ON-WRITE :IGNORE, the default) or signals
MEMORY-WRITE-PROTECTED (:ON-WRITE :ERROR); a store into a :DEVICE region
calls that region's WRITE instead of touching backing storage (discarded
when the region declares no WRITE), passed the same cell-width-wrapped
value every other memory write receives; a store into a banked :RAM region
writes the live bank. Use %POKE to bypass region write policy entirely --
LOAD-PROGRAM and the debugger burn a ROM image in that way. Returns the
wrapped value in every case, matching plain (SETF MREF)'s existing return
contract."
  (multiple-value-bind (slot element region) (%memory-slot-checked machine name address)
    (let ((wrapped (wrap-value value (storage-element-cell-width element))))
      (cond
        ((and region (eq (memory-region-kind region) :rom))
         (when (eq (memory-region-on-write region) :error)
           (error 'memory-write-protected :machine (machine-descriptor-name (machine-descriptor machine))
                                           :name name :address address)))
        ((and region (eq (memory-region-kind region) :device))
         (let ((write (memory-region-write region)))
           (when write (funcall write machine address wrapped))))
        ((and region (memory-region-banks region))
         (setf (aref (%live-bank machine region) (- address (memory-region-start region))) wrapped))
        (t (setf (aref slot address) wrapped)))
      wrapped)))

(defun mpeek (machine name address)
  "Read memory element NAME on MACHINE at ADDRESS directly from storage,
bypassing any #107 region policy -- a :DEVICE region's READ is never called
(returning 0, since a device region has no backing cell of its own), and a
:ROM region's read-only status is irrelevant since this never writes. An
address in a banked region reads the live bank. For inspection paths (the
debugger's hex dump, disassembly) that must not trigger a device's read
side effects merely by displaying memory."
  (multiple-value-bind (slot element region) (%memory-slot-checked machine name address)
    (declare (ignore element))
    (cond
      ((null region) (aref slot address))
      ((eq (memory-region-kind region) :device) 0)
      ((memory-region-banks region)
       (aref (%live-bank machine region) (- address (memory-region-start region))))
      (t (aref slot address)))))

(defun %poke (machine name address value)
  "Write memory element NAME on MACHINE at ADDRESS directly into storage,
bypassing any #107 region's write policy -- a :ROM region accepts this
store and a :DEVICE region's WRITE is never called. An address in a banked
region writes the live bank. For LOAD-PROGRAM and the debugger: a ROM image
is burned in, not stored by the CPU."
  (multiple-value-bind (slot element region) (%memory-slot-checked machine name address)
    (let ((wrapped (wrap-value value (storage-element-cell-width element))))
      (if (and region (memory-region-banks region))
          (setf (aref (%live-bank machine region) (- address (memory-region-start region))) wrapped)
          (setf (aref slot address) wrapped)))))

;;; Bank switching

(defun %banked-region-entry (machine region)
  "The (REGION-STRUCT . BANK-STATE) for banked region name REGION on MACHINE."
  (let ((state (gethash region (machine-banks machine))))
    (unless state
      (error "~S is not a banked region on machine ~S"
             region (machine-descriptor-name (machine-descriptor machine))))
    (cons (cdr (find region (%banked-regions (machine-descriptor machine))
                     :key (lambda (entry) (memory-region-name (cdr entry)))))
          state)))

(defun %check-bank (machine region bank count)
  (unless (and (integerp bank) (< -1 bank count))
    (error 'bank-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                              :name region :bank bank)))

(defun current-bank (machine region)
  "The index of the bank currently mapped into banked region REGION."
  (car (cdr (%banked-region-entry machine region))))

(defun (setf current-bank) (bank machine region)
  "Map BANK into banked region REGION. Signals BANK-OUT-OF-RANGE unless
0 <= BANK < the region's :BANKS."
  (let ((state (cdr (%banked-region-entry machine region))))
    (%check-bank machine region bank (length (cdr state)))
    (setf (car state) bank)))

(defun %bank-cell-index (machine region bank address)
  "The bank array and offset for absolute ADDRESS in bank BANK of REGION."
  (destructuring-bind (struct . state) (%banked-region-entry machine region)
    (%check-bank machine region bank (length (cdr state)))
    (unless (<= (memory-region-start struct) address (memory-region-end struct))
      (error 'address-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                   :name region :address address))
    (values (svref (cdr state) bank) (- address (memory-region-start struct)))))

(defun bank-peek (machine region bank address)
  "The cell at absolute ADDRESS in bank BANK of banked region REGION, live
or not. Never touches the live mapping."
  (multiple-value-bind (cells index) (%bank-cell-index machine region bank address)
    (aref cells index)))

(defun (setf bank-peek) (value machine region bank address)
  "Store VALUE, wrapped to the cell width, at absolute ADDRESS in bank BANK
of banked region REGION, bypassing the region's write policy."
  (multiple-value-bind (cells index) (%bank-cell-index machine region bank address)
    (setf (aref cells index) (wrap-value value (second (array-element-type cells))))))

(defun stack-push (machine name value)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (let ((vec (car slot)) (sp (cdr slot)))
      (when (>= sp (storage-element-depth element))
        (error 'stack-overflow :machine (machine-descriptor-name (machine-descriptor machine)) :name name))
      (setf (aref vec sp) (wrap-value value (storage-element-width element)))
      (setf (cdr slot) (1+ sp)))))

;; #166: register-indexed push/pop for a (stack-pointer ...) clause -- REG
;; holds an address into MEMORY rather than indexing a lasm :stack element.
;; No overflow/underflow condition: a wrapping REG is the machine's own
;; business, same as the hardware it models. The address is masked to
;; MEMORY's :addr-width before indexing, so REG may be wider than the
;; address space.
(defun %sp-address (machine reg memory)
  (let ((element (descriptor-element (machine-descriptor machine) memory)))
    (wrap-value (sref machine reg) (storage-element-addr-width element))))

(defun sp-push (machine reg memory grows value)
  "Push VALUE onto MACHINE's REG/MEMORY-backed stack-pointer stack, per
GROWS (:DOWN: pre-decrement REG then store; :UP: store then post-increment
REG)."
  (ecase grows
    (:down (setf (sref machine reg) (1- (sref machine reg)))
           (setf (mref machine memory (%sp-address machine reg memory)) value))
    (:up (setf (mref machine memory (%sp-address machine reg memory)) value)
         (setf (sref machine reg) (1+ (sref machine reg))))))

(defun sp-pop (machine reg memory grows)
  "Pop and return a value from MACHINE's REG/MEMORY-backed stack-pointer
stack, per GROWS (:DOWN: load then post-increment REG; :UP: pre-decrement
REG then load) -- the exact mirror of SP-PUSH's own GROWS case."
  (ecase grows
    (:down (prog1 (mref machine memory (%sp-address machine reg memory))
             (setf (sref machine reg) (1+ (sref machine reg)))))
    (:up (setf (sref machine reg) (1- (sref machine reg)))
         (mref machine memory (%sp-address machine reg memory)))))

(defun stack-pop (machine name)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (let ((vec (car slot)) (sp (cdr slot)))
      (when (<= sp 0)
        (error 'stack-underflow :machine (machine-descriptor-name (machine-descriptor machine)) :name name))
      (let ((new-sp (1- sp)))
        (setf (cdr slot) new-sp)
        (aref vec new-sp)))))

(defun %stack-pointer (machine name)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (cdr slot)))

(defun (setf %stack-pointer) (value machine name)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (unless (and (integerp value) (<= 0 value (storage-element-depth element)))
      (error 'stack-pointer-out-of-range
             :machine (machine-descriptor-name (machine-descriptor machine))
             :name name :value value))
    (setf (cdr slot) value)))

(defun stack-pointer (machine name)
  (%stack-pointer machine name))

(defun (setf stack-pointer) (value machine name)
  (setf (%stack-pointer machine name) value))

(defun stack-depth (machine name)
  (%stack-pointer machine name))

;; #50: indexed access into a stack, for a stack-relative addressing mode
;; (mode.lisp's STACK-RELATIVE) or any semantics body that needs to look past
;; the top without popping. OFFSET is top-relative and unsigned: 0 is the
;; top (the most recently pushed value, same as STACK-POP would return), 1 is
;; one below that, and so on -- Forth PICK / 65816 "n,S" convention. This
;; means the internal vector index is (- sp 1 offset), the mirror image of
;; how STACK-PUSH/STACK-POP already use SP. Bottom-relative indexing (index 0
;; = oldest entry) is still reachable by callers via STACK-DEPTH when wanted;
;; it just isn't OFFSET's own convention.
(defun %stack-ref (machine name offset)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (declare (ignore element))
    (let ((sp (cdr slot)))
      (unless (and (>= offset 0) (< offset sp))
        (error 'stack-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                          :name name :index offset))
      (aref (car slot) (- sp 1 offset)))))

(defun (setf %stack-ref) (value machine name offset)
  (multiple-value-bind (slot element) (%slot machine name :stack)
    (let ((sp (cdr slot)))
      (unless (and (>= offset 0) (< offset sp))
        (error 'stack-index-out-of-range :machine (machine-descriptor-name (machine-descriptor machine))
                                          :name name :index offset))
      (setf (aref (car slot) (- sp 1 offset))
            (wrap-value value (storage-element-width element))))))

(defun stack-ref (machine name offset)
  (%stack-ref machine name offset))

(defun (setf stack-ref) (value machine name offset)
  (setf (%stack-ref machine name offset) value))
