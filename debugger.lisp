;;;; debugger.lisp
;;;; #76 (M7): a gdb-like interactive debugger built on the emulator's
;;;; existing STEP-MACHINE/%RUN-LOOP primitives -- not a second execution
;;;; engine. A DEBUG-SESSION wraps a live MACHINE (and, optionally, the
;;;; ASSEMBLY that produced its program) with breakpoints, step/continue
;;;; commands, and read-only inspection of registers/flags/stacks/memory
;;;; driven off the machine's own declared storage elements
;;;; (MACHINE-DESCRIPTOR-ELEMENTS, storage.lisp) -- consistent with LASM's
;;;; storage-abstraction pillar (LASM-plan.md sec. 1, pillar 1).
;;;;
;;;; TWO LAYERS -- a machine-agnostic API (DEBUG-BREAK, DEBUG-STEP,
;;;; DEBUG-CONTINUE, DEBUG-STATE-TEXT, ...) is the real deliverable; a
;;;; command dispatcher (DEBUG-COMMAND, string in -> text out, mirroring
;;;; LISTING-TEXT/SYMBOLS-TEXT/DISASSEMBLY-TEXT's own :STREAM NIL -> string
;;;; convention) sits on top of it, and DEBUGGER-REPL is a thin read/
;;;; dispatch/print loop over that dispatcher -- the reference front end the
;;;; ticket asks for, not the primary interface.
;;;;
;;;; CONTINUE REUSES %RUN-LOOP (emulator.lisp) RATHER THAN WRITING A SECOND
;;;; LOOP -- it already takes a no-argument STOP-P checked after each step
;;;; and already handles LASM-TRAP/:DECODE-FAILURE. A breakpoint continue is
;;;; just %RUN-LOOP with a STOP-P that checks the current PC against the
;;;; breakpoint table. Because STOP-P fires *after* the step executes,
;;;; continuing from a PC that is itself a breakpoint proceeds rather than
;;;; re-triggering immediately -- this is deliberate, the same behaviour
;;;; gdb's `continue` has when already stopped on a breakpoint.
;;;;
;;;; WATCHPOINTS hook the machine's ACCESS-HOOK (storage.lisp), armed only
;;;; while instructions execute. CONDITIONAL BREAKPOINTS evaluate a parsed
;;;; expression with EVAL-EXPR against live register/flag values and the
;;;; attached assembly's symbols.
;;;;
;;;; STEP-BACK replays from checkpoints: a session created with :HISTORY
;;;; checkpoints the machine at the start of every execution command and every
;;;; *DEBUG-CHECKPOINT-INTERVAL* steps, and DEBUG-STEP-BACK restores the
;;;; nearest earlier checkpoint and replays forward to the target. A checkpoint
;;;; is a full snapshot (an anchor, every *DEBUG-ANCHOR-INTERVAL*th) or a delta
;;;; of the memory cells changed since the previous one. DEBUG-REVERSE-CONTINUE
;;;; jumps to a stop recorded during a forward run, else replays segment by
;;;; segment, newest first, skipping any that never reached a stop address or
;;;; accessed a watched target.
;;;; Cycle budgets reuse the same step loop and %RUN-LOOP's stop predicate.
;;;;
;;;; WRITES -- DEBUG-SET stores into registers, flags, stack slots and memory
;;;; (poking past region write policy); DEBUG-SET-BANK remaps a banked region.

(in-package #:lasm)

;;; Session

(defstruct breakpoint
  (id 0 :type (integer 1))
  (address 0 :type unsigned-byte)
  (label nil :type (or null string))  ; the name it was set by, if any
  (region nil :type (or null symbol)) ; banked region and bank it stops in;
  (bank nil :type (or null (integer 0))) ; NIL stops whichever bank is mapped
  (condition nil :type (or null string)) ; source text of the stop condition, if any
  (test nil)                             ; parsed condition AST
  (values nil)                           ; condition name -> value, labels and .equs
  (readers nil))                         ; condition (name . thunk) for live registers/flags

(defstruct watchpoint
  (id 0 :type (integer 1))
  (name nil :type (or null symbol))     ; register or flag watched; NIL for memory
  (index nil)                           ; bank index of a banked register
  (address nil)                         ; memory address watched
  (bank nil :type (or null (integer 0)))
  (access :write :type (member :read :write :read-write))
  (label "" :type string))

(defstruct watch-hit watchpoint access old new)

(defstruct checkpoint
  (step 0 :type (integer 0))
  (snapshot nil)                        ; full for an anchor, without memory and bank cells for a delta
  (anchor-p nil)
  (pcs (make-hash-table))               ; PCs reached in the steps after this checkpoint, up to the next
  (accessed (make-hash-table :test 'eq)) ; storage name -> read/write mask of that segment's steps; the memory's is a page -> mask table
  (diff nil))                           ; delta only: (array-key . ((start . new-cells) ...)) since the previous checkpoint

(defstruct (debug-session (:constructor %make-debug-session))
  (machine nil :type machine)
  (assembly nil)                ; optional ASSEMBLY -- symbols + source context
  (pc nil :type symbol)         ; resolved once, not re-resolved per command (#70's shape)
  (memory nil :type symbol)
  (cell-width nil :type (integer 1))
  (hex-digits nil :type (integer 1))   ; digits for one MEMORY cell's value (%listing-hex-digits)
  (addr-digits 4 :type (integer 1))    ; digits for an ADDRESS -- fixed at 4, matching
                                        ; LISTING-TEXT/PRINT-DISASSEMBLY's own address column

  (breakpoints (make-hash-table :test 'equal) :type hash-table) ; (address . bank) -> breakpoint
  (watchpoints nil :type list)  ; ascending id
  (watch-hit nil)               ; first WATCH-HIT of the running instruction
  (condition-error nil)         ; error from a breakpoint condition, stops the run
  (lexer 'default)
  (history nil :type (or null (integer 1))) ; steps of step-back history kept; NIL is off
  (step-count 0 :type (integer 0))          ; steps executed on this session's timeline
  (hits nil :type list)                     ; (step reason condition) of each step a reverse continue stops at, newest first
  (step-verdict nil)                        ; (step verdict . condition-error) of the breakpoint test run at STEP, taken once
  (hits-from nil)                           ; first step HITS is complete from; NIL when none is recorded
  (dirty nil)                               ; DIRTY-PAGES this session installed on the machine
  (hook nil)                                ; the access hook %ARM installs, made on first use
  (replaying nil)                           ; true inside %REPLAY, whose accesses are not recorded
  (checkpoints nil :type list)              ; CHECKPOINTs, newest first
  (shadow nil :type list)                   ; (array-key . copy) as of the newest checkpoint; NIL forces an anchor
  (next-id 1 :type (integer 1))
  (last-x-address nil)          ; so a bare `x` without an address continues from the last one
  (qualified-names nil))        ; readable name -> SYMBOL-INFO, built on first use

(defun make-debug-session (machine &key assembly pc memory (lexer 'default) history)
  "Create a DEBUG-SESSION wrapping MACHINE (a live MACHINE instance,
storage.lisp), optionally with the ASSEMBLY that produced its program for
label breakpoints, symbol listing, and source-line context. PC/MEMORY
override the usual by-convention resolution (%RESOLVE-PC/%RESOLVE-MEMORY,
emulator.lisp), same as STEP-MACHINE's own keywords -- resolved once here,
not on every command. LEXER tokenizes breakpoint conditions. HISTORY, a step
count, enables DEBUG-STEP-BACK over at least that many steps; NIL (the
default) records nothing."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (cell-width (%machine-cell-width machine-name memory)))
    (%make-debug-session
     :dirty (and history (setf (machine-dirty machine) (make-dirty-pages)))
     :machine machine :assembly (or assembly (machine-program machine)) :pc pc :memory memory :lexer lexer :history history
     :cell-width cell-width :hex-digits (%listing-hex-digits cell-width))))

;;; Breakpoints

(defun %qualified-names (session)
  "SESSION's table from readable qualified name to SYMBOL-INFO, the first in
line order when spellings collide."
  (or (debug-session-qualified-names session)
      (let ((table (make-hash-table :test 'equal)))
        (dolist (info (assembly-symbols-list (debug-session-assembly session)))
          (let ((name (symbol-info-qualified-name info)))
            (unless (nth-value 1 (gethash name table))
              (setf (gethash name table) info))))
        (setf (debug-session-qualified-names session) table))))

(defun %session-symbol (session name scope)
  "The SYMBOL-INFO for NAME in SESSION's assembly: a local under SCOPE, a
global, or a local by its qualified spelling (\"count.loop\"). NIL if none."
  (let ((assembly (debug-session-assembly session)))
    (or (and scope (assembly-symbol assembly name :scope scope))
        (assembly-symbol assembly name)
        (gethash name (%qualified-names session)))))

(defun %resolve-breakpoint-address (session where &key scope bank)
  "WHERE as (VALUES ADDRESS REGION BANK): an integer as-is, or a label string
resolved through %SESSION-SYMBOL against SESSION's attached ASSEMBLY. A label defined under .BANK carries its region and bank; BANK
qualifies an integer address instead, and must agree with a label's own bank.
Signals a plain error when SESSION has no ASSEMBLY, when the name is
unbound, or when it names an assignment rather than a :LABEL -- its value is
not an address (the #81 ambiguity SYMBOL-INFO's KIND already resolves; a
bare ASSEMBLY-SYMBOLS lookup would reintroduce it here)."
  (etypecase where
    (integer
     (if bank
         (let ((region (or (%session-banked-region session where)
                           (%debugger-usage-error "address ~D is not in a banked region" where))))
           (unless (< bank (memory-region-banks region))
             (%debugger-usage-error "bank ~D is out of range for region ~(~A~) (~D bank~:P)"
                    bank (memory-region-name region) (memory-region-banks region)))
           (values where (memory-region-name region) bank))
         (values where nil nil)))
    (string
     (let ((assembly (debug-session-assembly session)))
       (unless assembly
         (%debugger-usage-error "no assembly attached to this session -- cannot resolve label ~S" where))
       (let ((info (%session-symbol session where scope)))
         (unless info
           (%debugger-usage-error "no symbol named ~S~@[ in scope ~S~]" where scope))
         (unless (eq (symbol-info-kind info) :label)
           (%debugger-usage-error "~S is a ~(~A~), not a label -- its value is not an address"
                  where (symbol-info-kind info)))
         (when (and bank (not (eql bank (symbol-info-bank info))))
           (%debugger-usage-error "~S is not in bank ~D" where bank))
         (values (symbol-info-value info) (symbol-info-region info) (symbol-info-bank info)))))))

;;; Conditions

(defmacro %without-hook ((machine) &body body)
  "Run BODY with MACHINE's access hook removed, so the debugger's own reads
never notify it."
  (let ((m (gensym "MACHINE")) (hook (gensym "HOOK")))
    `(let* ((,m ,machine) (,hook (machine-access-hook ,m)))
       (setf (machine-access-hook ,m) nil)
       (unwind-protect (progn ,@body)
         (setf (machine-access-hook ,m) ,hook)))))

(defun %resolve-depth (session name)
  "The fixed stack NAME as (VALUES STACK :POINTER). Signals when NAME is not one."
  (let ((stack (%resolve-storage session name nil t)))
    (unless (and stack (%stack-name-p (debug-session-machine session) stack))
      (%debugger-usage-error "~A is not a fixed stack" name))
    (values stack :pointer)))

(defun %resolve-storage (session name &optional index stacks)
  "NAME as a scalar register, flag or register alias on SESSION's machine,
as (VALUES ELEMENT-NAME INDEX), or NIL when NAME is not a storage name.
INDEX selects a cell of a banked register, or with STACKS a slot of a fixed
stack (a bare stack name gives INDEX :ANY). NAME.depth, or a stack NAME with
INDEX :DEPTH, gives INDEX :POINTER. Signals when NAME is storage that cannot
be read as a single value."
  (let ((dot (position #\. name :from-end t)))
    (cond
      ((eq index :depth)
       (return-from %resolve-storage (%resolve-depth session name)))
      ((and dot (string-equal (subseq name (1+ dot)) "depth"))
       (when index (%debugger-usage-error "~A takes no index" name))
       (let ((base (subseq name 0 dot)))
         (return-from %resolve-storage
           (and (%resolve-storage session base nil t) (%resolve-depth session base)))))))
  (let* ((descriptor (machine-descriptor (debug-session-machine session)))
         (alias-element (gethash name (machine-descriptor-register-alias-elements descriptor)))
         (symbol (find-symbol (string-upcase name) :lasm))
         (element (and symbol (gethash symbol (machine-descriptor-table descriptor)))))
    (cond
      (alias-element
       (when index (%debugger-usage-error "~A is a register alias and takes no index" name))
       (values (storage-element-name alias-element)
               (gethash name (machine-descriptor-register-aliases descriptor))))
      ((null element) nil)
      ((and stacks (eq (storage-element-kind element) :stack))
       (cond ((null index) (values symbol :any))
             ((< -1 index (storage-element-depth element)) (values symbol index))
             (t (%debugger-usage-error "slot ~D is out of range for stack ~A" index name))))
      ((eq (storage-element-kind element) :flag)
       (when index (%debugger-usage-error "~A is a flag and takes no index" name))
       (values symbol nil))
      ((not (eq (storage-element-kind element) :register))
       (%debugger-usage-error "~A is not a register or flag" name))
      ((= (storage-element-count element) 1)
       (when index (%debugger-usage-error "~A is not a banked register" name))
       (values symbol nil))
      ((null index)
       (%debugger-usage-error "~A is a banked register -- name a cell as ~A[N] or use an alias" name name))
      ((not (< -1 index (storage-element-count element)))
       (%debugger-usage-error "index ~D is out of range for register ~A" index name))
      (t (values symbol index)))))

(defun %read-storage (machine name index)
  (%without-hook (machine)
    (cond
      ((eq index :pointer) (stack-depth machine name))
      (index (regref machine name index))
      ((eq (storage-element-kind (descriptor-element (machine-descriptor machine) name)) :flag)
       (flag machine name))
      (t (sref machine name)))))

(defun %condition-names (ast)
  "As (VALUES NAMES INDEXED): the distinct EXPR-LABEL names in AST, and its
EXPR-INDEX nodes. Signals on an assembler-only operator (bank, defined,
lowcell, highcell)."
  (let (names indexed)
    (labels ((walk (node)
               (etypecase node
                 ((or expr-number expr-location) nil)
                 (expr-string (%debugger-usage-error "A string literal is not available in a condition"))
                 (expr-label (pushnew (expr-label-name node) names :test #'string=))
                 (expr-index (cl:push node indexed)
                             (walk (expr-index-operand node)))
                 (expr-unary
                  (when (member (expr-unary-op node) '(:bank :defined :lowcell :highcell))
                    (%debugger-usage-error "~(~A~)() is not available in a condition" (expr-unary-op node)))
                  (walk (expr-unary-operand node)))
                 (expr-binary (walk (expr-binary-left node))
                              (walk (expr-binary-right node))))))
      (walk ast))
    (values (nreverse names) (nreverse indexed))))

(defun %check-indexed (session node)
  "Signal when NODE's storage is unknown or not indexable, or its literal index is out of range."
  (let ((operand (expr-index-operand node))
        (name (expr-index-name node)))
    (unless (%resolve-storage session name (if (expr-number-p operand) (expr-number-value operand) 0) t)
      (%debugger-usage-error "~A is not a register or stack" name))))

(defun %read-indexed (session name index)
  "The cell of banked register or live slot of the fixed stack NAME at INDEX."
  (multiple-value-bind (storage slot) (%resolve-storage session name index t)
    (let ((machine (debug-session-machine session)))
      (unless storage (%debugger-usage-error "~A is not a register or stack" name))
      (if (%stack-name-p machine storage)
          (or (%stack-slot-value machine storage slot)
              (%debugger-usage-error "stack ~A has no live slot ~D" name slot))
          (%read-storage machine storage slot)))))

(defun %compile-condition (session text scope)
  "Parse TEXT and bind every name in it, as (VALUES AST VALUES READERS): a
register, flag or alias becomes a live reader, anything else a label or .equ
of SESSION's assembly, looked up under SCOPE first, then globally. Signals
on a syntax error or an unknown name."
  (let* ((tokens (coerce (tokenize text :lexer (debug-session-lexer session)) 'simple-vector))
         (end (or (position :eof tokens :key #'token-type) (length tokens)))
         (values (make-hash-table :test 'equal))
         readers)
    (multiple-value-bind (ast next) (let ((*indexed-names* t)) (parse-expression tokens :end end))
      (when (< next end)
        (%debugger-usage-error "unexpected ~S in condition" (token-text (aref tokens next))))
      (multiple-value-bind (names indexed) (%condition-names ast)
        (dolist (node indexed) (%check-indexed session node))
        (dolist (name names)
          (multiple-value-bind (storage index) (%resolve-storage session name)
            (let* ((assembly (and (null storage) (debug-session-assembly session)))
                   (info (and assembly (%session-symbol session name scope))))
              (cond
                (storage
                 (let ((machine (debug-session-machine session)))
                   (cl:push (cons name (lambda () (%read-storage machine storage index))) readers)))
                (info (setf (gethash name values) (symbol-info-value info)))
                (t (%debugger-usage-error "unknown name ~S in condition" name)))))))
      (values ast values readers))))

(defun %memory-reader (session)
  "A function from address to the cell at it in SESSION's memory, for mem()."
  (let ((machine (debug-session-machine session))
        (memory (debug-session-memory session)))
    (lambda (address) (%without-hook (machine) (mpeek machine memory address)))))

(defun %eval-condition (session test values readers)
  "TEST evaluated over freshly read READERS and VALUES, with mem() bound."
  (loop for (name . reader) in readers
        do (setf (gethash name values) (funcall reader)))
  (let ((*memory-reader* (%memory-reader session))
        (*index-reader* (lambda (name index) (%read-indexed session name index))))
    (eval-expr test :symbols values :pc (%pc session))))

(defun %breakpoint-triggered-p (session bp)
  "True when BP has no condition or its condition is nonzero. An error in the
condition counts as true and is left in SESSION's CONDITION-ERROR."
  (or (null (breakpoint-test bp))
      (handler-case
          (/= 0 (%eval-condition session (breakpoint-test bp) (breakpoint-values bp)
                                 (breakpoint-readers bp)))
        (error (c) (setf (debug-session-condition-error session) c) t))))

(defun debug-break (session where &key scope bank condition)
  "Set a breakpoint at WHERE (an address, or a label name resolved via
%RESOLVE-BREAKPOINT-ADDRESS) on SESSION. A label in a banked region, or an
address given with BANK, only stops while that bank is mapped. CONDITION, a
string, is an expression over registers, flags, register aliases, REG[N] cells,
STACK[N] slots, STACK.depth, labels and .equs of the attached assembly (SCOPE qualifies local labels), with * as the
PC and mem(ADDR) as the cell at ADDR; the breakpoint only stops while it is nonzero. A bad condition signals
here. Returns the new BREAKPOINT. Setting a second breakpoint at the same
address and bank replaces it (same id space, but the old entry is gone)
rather than stacking duplicates."
  (multiple-value-bind (address region bank)
      (%resolve-breakpoint-address session where :scope scope :bank bank)
    (multiple-value-bind (test values readers)
        (and condition (%compile-condition session condition scope))
      (let ((bp (make-breakpoint :id (debug-session-next-id session)
                                 :address address :region region :bank bank
                                 :label (and (stringp where) where)
                                 :condition condition :test test
                                 :values values :readers readers)))
        (incf (debug-session-next-id session))
        (setf (gethash (cons address bank) (debug-session-breakpoints session)) bp)
        (%forget-hits session)
        bp))))

(defun debug-unbreak (session id-or-address &key bank)
  "Remove a breakpoint from SESSION by its BREAKPOINT-ID or by address.
Returns T if one was removed, NIL if ID-OR-ADDRESS named none.

ID-OR-ADDRESS is tried as a BREAKPOINT-ID first (gdb's own `delete N`
convention -- ids are the number `info break`/`break` itself prints, and are
what a caller almost always means), falling back to an address match only
when no breakpoint has that id. Ids and addresses are both plain integers
with no reserved ranges of their own, so the two CAN collide (breakpoint 1
happens to sit at address 1) -- id takes priority in that case, matching
what a human typing a small integer after `delete` almost certainly meant.
An address match removes every breakpoint at that address, or only the one
in BANK when given."
  (let ((table (debug-session-breakpoints session))
        hits)
    (if bank
        (when (gethash (cons id-or-address bank) table)
          (setf hits (list (cons id-or-address bank))))
        (progn
          (maphash (lambda (key bp)
                     (when (eql (breakpoint-id bp) id-or-address)
                       (setf hits (list key))))
                   table)
          (unless hits
            (maphash (lambda (key bp)
                       (declare (ignore bp))
                       (when (eql (car key) id-or-address) (cl:push key hits)))
                     table))))
    (dolist (key hits) (remhash key table))
    (%forget-hits session)
    (and hits t)))

(defun debug-breakpoints (session)
  "SESSION's live breakpoints, as a list of BREAKPOINT, ascending by address
then bank."
  (let (result)
    (maphash (lambda (key bp) (declare (ignore key)) (cl:push bp result)) (debug-session-breakpoints session))
    (sort result (lambda (a b)
                   (or (< (breakpoint-address a) (breakpoint-address b))
                       (and (= (breakpoint-address a) (breakpoint-address b))
                            (< (or (breakpoint-bank a) -1) (or (breakpoint-bank b) -1))))))))

(defun %mapped-bank (session address)
  "The bank currently mapped at ADDRESS, or NIL outside a banked region."
  (let ((region (%session-banked-region session address)))
    (and region (current-bank (debug-session-machine session) (memory-region-name region)))))

;;; Watchpoints

(defun debug-watch (session target &key (access :write) index scope bank)
  "Watch TARGET on SESSION, stopping after the instruction that accesses it.
TARGET is a scalar register, flag or register alias name (a banked register
takes INDEX), a fixed stack name (INDEX picks a bottom-relative slot, :DEPTH
its depth, which needs :WRITE or :READ-WRITE; without one any access to the
stack), a label, or a memory address; a name that is
both storage and a label is storage. ACCESS is :READ, :WRITE or :READ-WRITE. SCOPE and BANK
qualify a memory target as in DEBUG-BREAK. Returns the new WATCHPOINT."
  (unless (member access '(:read :write :read-write))
    (%debugger-usage-error "bad watch access ~S -- expected :READ, :WRITE or :READ-WRITE" access))
  (when (and (eq index :depth) (not (stringp target)))
    (%debugger-usage-error "depth watches need a fixed stack name, not ~S" target))
  (multiple-value-bind (name register-index)
      (and (stringp target) (%resolve-storage session target index t))
    (when (and (eq register-index :pointer) (eq access :read))
      (%debugger-usage-error "a depth changes only by writes -- watch ~A with w or rw" target))
    (let ((wp (if name
                  (make-watchpoint :id (debug-session-next-id session) :name name :index register-index :access access
                                   :label (cond ((eq register-index :pointer) (format nil "~(~A~).depth" name))
                                                ((null register-index) (string-downcase target))
                                                (index (format nil "~(~A~)[~D]" target index))
                                                (t (string-downcase target))))
                  (multiple-value-bind (address region bank)
                      (%resolve-breakpoint-address session target :scope scope :bank bank)
                    (declare (ignore region))
                    (make-watchpoint :id (debug-session-next-id session) :address address :bank bank :access access
                                     :label (if (stringp target)
                                                target
                                                (%breakpoint-address-text session address bank)))))))
      (incf (debug-session-next-id session))
      (setf (debug-session-watchpoints session)
            (append (debug-session-watchpoints session) (list wp)))
      (%forget-hits session)
      wp)))

(defun %set-memory (session address bank value)
  "Poke VALUE at ADDRESS in SESSION's memory, or in BANK of its banked region."
  (let* ((machine (debug-session-machine session))
         (memory (debug-session-memory session))
         (region (%region-at (descriptor-element (machine-descriptor machine) memory) address)))
    (when (and region (eq (memory-region-kind region) :device))
      (%debugger-usage-error "address ~D is in device region ~(~A~) -- it has no cell to write"
             address (memory-region-name region)))
    (if bank
        (setf (bank-peek machine (memory-region-name region) bank address) value)
        (%poke machine memory address value))
    (if bank (bank-peek machine (memory-region-name region) bank address) (mpeek machine memory address))))

(defun %stack-target (session target)
  "The name of the fixed stack TARGET on SESSION, or signals."
  (multiple-value-bind (name index) (and (stringp target) (%resolve-storage session target nil t))
    (unless (and name (not (eq index :pointer)) (%stack-name-p (debug-session-machine session) name))
      (%debugger-usage-error "~A is not a fixed stack" target))
    name))

(defun %set-stack-contents (machine name values)
  "Replace stack NAME's live entries with VALUES, bottom first. Signals, leaving
the stack untouched, unless VALUES are integers that fit its depth."
  (let ((depth (storage-element-depth (descriptor-element (machine-descriptor machine) name)))
        (count (length values)))
    (unless (every #'integerp values)
      (%debugger-usage-error "cannot store ~S -- expected integers" values))
    (when (> count depth)
      (%debugger-usage-error "~D values do not fit stack ~(~A~) (depth ~D)" count name depth))
    (setf (stack-pointer machine name) count)
    (loop for value in values
          for offset downfrom (1- count)
          do (setf (stack-ref machine name offset) value))
    (loop for offset downfrom (1- count) to 0 collect (stack-ref machine name offset))))

(defun debug-set (session target value &key index scope bank)
  "Store integer VALUE, wrapped to the cell width, in TARGET on SESSION's
machine and return the stored value. TARGET is a register, flag or register
alias name (a banked register takes INDEX), a fixed stack name with INDEX
picking a live bottom-relative slot, a label, or a memory address; SCOPE and
BANK qualify a memory target as in DEBUG-BREAK. A fixed stack name with INDEX
:DEPTH sets its depth to VALUE (cells a grown stack uncovers keep their old
values); with a list VALUE it replaces the live entries, bottom first, and
returns the stored list. Memory is poked, so a :ROM region is writable and a
:DEVICE region signals -- see DEBUG-WRITE for a CPU-faithful store. Never
notifies the access hook, so watchpoints do not fire."
  (%forget-hits session)
  (let ((machine (debug-session-machine session)))
    (cond
      ((eq index :depth)
       (unless (integerp value)
         (%debugger-usage-error "cannot set depth to ~S -- expected an integer" value))
       (let ((name (%stack-target session target)))
         (%without-hook (machine) (setf (stack-pointer machine name) value))))
      ((listp value)
       (when index (%debugger-usage-error "cannot store a list in ~A[~A]" target index))
       (let ((name (%stack-target session target)))
         (%without-hook (machine) (%set-stack-contents machine name value))))
      ((not (integerp value))
       (%debugger-usage-error "cannot store ~S -- expected an integer" value))
      (t
       (multiple-value-bind (name slot) (and (stringp target) (%resolve-storage session target index t))
         (%without-hook (machine)
           (cond
             ((eq slot :pointer) (setf (stack-pointer machine name) value))
             ((null name)
              (multiple-value-bind (address region bank)
                  (%resolve-breakpoint-address session target :scope scope :bank bank)
                (declare (ignore region))
                (%set-memory session address bank value)))
             ((%stack-name-p machine name)
              (let ((depth (stack-depth machine name)))
                (unless (and (integerp slot) (< slot depth))
                  (%debugger-usage-error "stack ~A has no live slot ~A" target (if (integerp slot) slot "-- name one as NAME[N]")))
                (setf (stack-ref machine name (- depth 1 slot)) value)))
             (slot (setf (regref machine name slot) value))
             ((eq (storage-element-kind (descriptor-element (machine-descriptor machine) name)) :flag)
              (setf (flag machine name) value))
             (t (setf (sref machine name) value)))))))))

(defun debug-write (session where value &key scope bank)
  "Store integer VALUE at memory address or label WHERE through the machine's
own write path, as the CPU would: a :ROM region drops it (or signals, with
:ON-WRITE :ERROR) and a :DEVICE region's WRITE runs. SCOPE and BANK qualify
WHERE as in DEBUG-BREAK; a BANK other than the mapped one signals. Returns
(VALUES CELL STORED-P): the cell now at WHERE (for a device region, the wrapped
value written) and whether it holds VALUE. Never notifies the access hook."
  (unless (integerp value)
    (%debugger-usage-error "cannot store ~S -- expected an integer" value))
  (%forget-hits session)
  (when (and (stringp where) (%resolve-storage session where nil t))
    (%debugger-usage-error "write only targets memory -- use set for ~A" where))
  (let ((machine (debug-session-machine session))
        (memory (debug-session-memory session)))
    (multiple-value-bind (address region-name bank)
        (%resolve-breakpoint-address session where :scope scope :bank bank)
      (declare (ignore region-name))
      (when (and bank (/= bank (%mapped-bank session address)))
        (%debugger-usage-error "bank ~D is not mapped at address ~D" bank address))
      (%without-hook (machine)
        (let ((*privilege-checks* nil))
          (setf (mref machine memory address) value))
        (let* ((element (descriptor-element (machine-descriptor machine) memory))
               (region (%region-at element address))
               (cell (if (and region (eq (memory-region-kind region) :device))
                         (wrap-value value (storage-element-cell-width element))
                         (mpeek machine memory address))))
          (values cell (= cell (wrap-value value (storage-element-cell-width element)))))))))

(defun debug-unwatch (session id)
  "Remove the watchpoint with ID from SESSION. Returns T if one was removed."
  (let ((wp (find id (debug-session-watchpoints session) :key #'watchpoint-id)))
    (when wp
      (setf (debug-session-watchpoints session) (remove wp (debug-session-watchpoints session)))
      (%forget-hits session)
      t)))

(defun debug-watchpoints (session)
  "SESSION's watchpoints, ascending by id."
  (copy-list (debug-session-watchpoints session)))

(defun %stack-name-p (machine name)
  (eq (storage-element-kind (descriptor-element (machine-descriptor machine) name)) :stack))

(defun %stack-slot-value (machine name slot)
  "The live value in bottom-relative SLOT of stack NAME, or NIL when it is not live."
  (let ((cell (%slot machine name :stack)))
    (and (< slot (cdr cell)) (aref (car cell) slot))))

(defun %watch-matches-p (session wp name index access)
  (and (or (eq (watchpoint-access wp) :read-write) (eq (watchpoint-access wp) access))
       (if (watchpoint-name wp)
           (and (eq name (watchpoint-name wp))
                (or (eql index (watchpoint-index wp)) (eq (watchpoint-index wp) :any)))
           (and (eq name (debug-session-memory session))
                (eql index (watchpoint-address wp))
                (or (null (watchpoint-bank wp))
                    (eql (watchpoint-bank wp) (%mapped-bank session index)))))))

(defun %watch-hook (session)
  "The access hook recording SESSION's first watchpoint hit."
  (lambda (machine name index access value)
    (unless (debug-session-watch-hit session)
      (let ((wp (find-if (lambda (wp) (%watch-matches-p session wp name index access))
                         (debug-session-watchpoints session))))
        (when wp
          (setf (debug-session-watch-hit session)
                (make-watch-hit
                 :watchpoint wp :access access :new value
                 :old (cond ((eq access :read) value)
                            ((eq index :pointer) (stack-depth machine name))
                            ((%stack-name-p machine name) (%stack-slot-value machine name index))
                            ((watchpoint-name wp) (%read-storage machine name index))
                            (t (mpeek machine name index))))))))))

(defun %record-access (session name index access)
  "Note in SESSION's newest checkpoint segment that NAME, at INDEX when it is
the memory, was ACCESSed."
  (let ((checkpoint (car (debug-session-checkpoints session))))
    (when (and checkpoint (not (debug-session-replaying session)))
      (let ((accessed (checkpoint-accessed checkpoint))
            (bit (if (eq access :read) 1 2)))
        (if (eq name (debug-session-memory session))
            (let ((pages (or (gethash name accessed)
                             (setf (gethash name accessed) (make-hash-table)))))
              (let ((page (ash index (- +dirty-page-bits+))))
                (setf (gethash page pages) (logior bit (gethash page pages 0)))))
            (setf (gethash name accessed) (logior bit (gethash name accessed 0))))))))

(defun %session-hook (session)
  "The access hook for SESSION's runs: it records the segment's accesses when
SESSION keeps history, then notes watchpoint hits."
  (or (debug-session-hook session)
      (let ((watch (%watch-hook session))
            (history (debug-session-history session)))
        (setf (debug-session-hook session)
              (lambda (machine name index access value)
                (when history (%record-access session name index access))
                (funcall watch machine name index access value))))))

(defun %hooked-p (session)
  (or (debug-session-watchpoints session) (debug-session-history session)))

(defun %arm (session)
  (when (%hooked-p session)
    (setf (machine-access-hook (debug-session-machine session)) (%session-hook session))))

(defun %disarm (session)
  (when (%hooked-p session)
    (setf (machine-access-hook (debug-session-machine session)) nil)))

;;; Execution
;;;
;;; DEBUG-STEP/DEBUG-STEP-CYCLES/DEBUG-CONTINUE/DEBUG-CONTINUE-TO return
;;; (VALUES REASON STEPS [CONDITION]). A trap or fault supplies the condition as
;;; the third value; a :WATCHPOINT stop supplies its WATCH-HIT, and a
;;; :BREAKPOINT stop the error from its condition, if that failed.
;;; DEBUG-STEP reports :STEP after N steps rather than :MAX-STEPS.
;;;
;;; An idle STEP-MACHINE result is not a stop condition here either --
;;; DEBUG-STEP counts it as one of its N steps (so single-stepping through a
;;; sleeping machine just burns steps one at a time, same as any other
;;; instruction), and DEBUG-CONTINUE/DEBUG-CONTINUE-TO forward %RUN-LOOP's
;;; own :IDLE straight through (see %RUN-UNTIL below) exactly like :TRAP/
;;; :DECODE-FAILURE.
;;;
;;; STEP-COUNT is the session's position on its step timeline: every step that
;;; ran counts (idle, trap and fault steps included), a decode failure does not.

(defvar *debug-checkpoint-interval* 256
  "Steps between the checkpoints a session with a HISTORY records mid-run.")

(defun %pc (session)
  (sref (debug-session-machine session) (debug-session-pc session)))

(defvar *debug-anchor-interval* 16
  "Checkpoints between the full-snapshot anchors of a session's history.")

(defun %memory-arrays (machine)
  "The memory and bank cell arrays of MACHINE, as (KEY . ARRAY)."
  (let ((descriptor (machine-descriptor machine))
        (result '()))
    (dolist (element (machine-descriptor-elements descriptor))
      (when (eq (storage-element-kind element) :memory)
        (cl:push (cons (list :memory (storage-element-name element))
                       (gethash (storage-element-name element) (machine-slots machine)))
                 result)))
    (loop for (nil . region) in (%banked-regions descriptor)
          for banks = (cdr (gethash (memory-region-name region) (machine-banks machine)))
          do (loop for array across banks
                   for i from 0
                   do (cl:push (cons (list :bank (memory-region-name region) i) array) result)))
    (nreverse result)))

(defun %diff-spans (old new &key (start 0) (end (length new)))
  "The runs of NEW between START and END that differ from OLD, as (START .
CELLS), copied into OLD."
  (let ((spans '())
        (i start))
    (loop
      (let ((start (mismatch old new :start1 i :start2 i :end1 end :end2 end)))
        (unless start (return))
        (setf i start)
        (loop while (and (< i end) (/= (aref old i) (aref new i))) do (incf i))
        (cl:push (cons start (subseq new start i)) spans)
        (replace old new :start1 start :end1 i :start2 start)))
    (nreverse spans)))

(defun %dirty-complete-p (session)
  "True when the machine's dirty pages are SESSION's own and no bulk write hid any."
  (let ((dirty (debug-session-dirty session)))
    (and dirty (eq dirty (machine-dirty (debug-session-machine session)))
         (not (dirty-pages-all dirty)))))

(defun %delta-diff (session arrays)
  "The memory changes since SESSION's shadow, as (KEY . SPANS) in ARRAYS
order. Only the pages the machine marked dirty are compared, unless the
machine tracks another session's pages or a bulk write hid some."
  (let ((dirty (debug-session-dirty session))
        (shadow (debug-session-shadow session)))
    (flet ((shadow-of (key) (cdr (assoc key shadow :test #'equal))))
      (if (%dirty-complete-p session)
          (let ((pages (make-hash-table :test 'eq)))
            (loop for (array . page) in (dirty-pages-queue dirty)
                  do (cl:push page (gethash array pages)))
            (loop for (key . array) in arrays
                  for spans = (loop for page in (sort (gethash array pages) #'<)
                                    for start = (ash page +dirty-page-bits+)
                                    append (%diff-spans (shadow-of key) array
                                                        :start start
                                                        :end (min (length array)
                                                                  (+ start (ash 1 +dirty-page-bits+)))))
                  when spans collect (cons key spans)))
          (loop for (key . array) in arrays
                for spans = (%diff-spans (shadow-of key) array)
                when spans collect (cons key spans))))))

(defun %clear-dirty (session)
  "Start SESSION's dirty tracking afresh, taking over the machine's."
  (let ((dirty (debug-session-dirty session)))
    (when dirty
      (setf (dirty-pages-queue dirty) nil
            (dirty-pages-all dirty) nil
            (machine-dirty (debug-session-machine session)) dirty)
      (clrhash (dirty-pages-bits dirty)))))

(defun %checkpoint (session)
  "Record a checkpoint at SESSION's current step count, discarding any recorded
beyond it and any older than its HISTORY. Does nothing when history is off."
  (let ((history (debug-session-history session))
        (now (debug-session-step-count session)))
    (when history
      (let* ((machine (debug-session-machine session))
             (old (debug-session-checkpoints session))
             (checkpoints (remove-if (lambda (c) (>= (checkpoint-step c) now)) old))
             (deltas (position-if #'checkpoint-anchor-p checkpoints)))
        (when (/= (length old) (length checkpoints))
          (setf (debug-session-shadow session) nil))
        (cl:push
         (if (or (null (debug-session-shadow session)) (null deltas)
                 (>= deltas (1- *debug-anchor-interval*)))
             (progn
               (setf (debug-session-shadow session)
                     (loop for (key . array) in (%memory-arrays machine)
                           collect (cons key (copy-seq array))))
               (%clear-dirty session)
               (make-checkpoint :step now :anchor-p t :snapshot (machine-snapshot machine)))
             (let ((diff (%delta-diff session (%memory-arrays machine))))
               (%clear-dirty session)
               (make-checkpoint :step now :snapshot (%machine-snapshot machine nil)
                                :diff diff)))
         checkpoints)
        (let ((tail (member-if (lambda (c) (and (checkpoint-anchor-p c)
                                                (<= (checkpoint-step c) (- now history))))
                               checkpoints)))
          (when tail
            (setf (cdr tail) nil)
            (let ((oldest (checkpoint-step (car (last checkpoints)))))
              (setf (debug-session-hits session)
                    (remove oldest (debug-session-hits session) :key #'car :test #'>)))))
        (setf (debug-session-checkpoints session) checkpoints)))))

(defun %step-hit (session)
  "(REASON CONDITION) when SESSION's current step is one a reverse continue
stops at: a watchpoint hit, or a breakpoint that holds. A failing condition is
returned, not left in SESSION's CONDITION-ERROR."
  (let ((saved (debug-session-condition-error session))
        (watch (debug-session-watch-hit session)))
    (setf (debug-session-condition-error session) nil)
    (let* ((stop (and (not watch) (%breakpoint-stop session)))
           (failure (debug-session-condition-error session))
           (reason (cond (watch :watchpoint) (stop :breakpoint))))
      (unless watch
        (setf (debug-session-step-verdict session)
              (list* (debug-session-step-count session) (and stop t) failure)))
      (setf (debug-session-condition-error session) saved)
      (and reason (list reason (or watch failure))))))

(defun %breakpoint-stop-once (session)
  "%BREAKPOINT-STOP, reusing the verdict %STEP-HIT just reached at this step."
  (let ((verdict (shiftf (debug-session-step-verdict session) nil)))
    (cond ((and verdict (= (car verdict) (debug-session-step-count session)))
           (when (cddr verdict)
             (setf (debug-session-condition-error session) (cddr verdict)))
           (cadr verdict))
          (t (%breakpoint-stop session)))))

(defun %record-step (session)
  "Note SESSION's current step, just executed: its PC in the open checkpoint
segment, and whether a reverse continue would stop at it."
  (let ((now (debug-session-step-count session))
        (checkpoint (car (debug-session-checkpoints session))))
    (%without-hook ((debug-session-machine session))
      (when checkpoint
        (setf (gethash (%pc session) (checkpoint-pcs checkpoint)) t))
      (loop while (and (debug-session-hits session) (>= (car (first (debug-session-hits session))) now))
            do (cl:pop (debug-session-hits session)))
      (when (debug-session-hits-from session)
        (let ((hit (%step-hit session)))
          (when hit (cl:push (cons now hit) (debug-session-hits session))))))))

(defun %forget-hits (session)
  "Drop the recorded hits, after the breakpoints or watchpoints changed."
  (setf (debug-session-hits session) nil
        (debug-session-hits-from session) nil
        (debug-session-step-verdict session) nil))

(defun %start-command (session)
  "Checkpoint the step an execution command starts at. Hits are recorded from
the next step, since a watchpoint hit at this one is only known if it was
recorded when it ran."
  (when (debug-session-history session)
    (unless (debug-session-hits-from session)
      (setf (debug-session-hits-from session) (1+ (debug-session-step-count session))))
    (%checkpoint session)))

(defun %count-step (session)
  "Count one executed step, checkpointing at every interval boundary."
  (let ((now (incf (debug-session-step-count session))))
    (when (debug-session-history session)
      (%record-step session)
      (when (zerop (mod now *debug-checkpoint-interval*))
        (%checkpoint session)))))

(defun %step-while (session more-p)
  "Step SESSION's machine while MORE-P, called with the steps taken and the
cycles spent so far, is true. Returns (VALUES REASON STEPS [CONDITION]) as
DEBUG-STEP does."
  (let* ((machine (debug-session-machine session))
         (pc (debug-session-pc session))
         (memory (debug-session-memory session))
         (start-cycles (machine-cycles machine))
         (steps 0))
    (setf (debug-session-watch-hit session) nil)
    (%start-command session)
    (unwind-protect
         (loop
           (unless (funcall more-p steps (- (machine-cycles machine) start-cycles))
             (return (values :step steps)))
           (handler-case
               (progn
                 (%arm session)
                 (let ((result (handler-bind ((storage-error
                                                (lambda (c)
                                                  (declare (ignore c))
                                                  (%count-step session))))
                                 (step-machine machine :pc pc :memory memory))))
                   (%disarm session)
                   (when (eq result :decode-failure)
                     (return (values :decode-failure steps)))
                   (incf steps)
                   (%count-step session)
                   (let ((hit (debug-session-watch-hit session)))
                     (when hit
                       (return (values :watchpoint steps hit))))))
             (lasm-trap (c)
               (%count-step session)
               (return (values :trap (1+ steps) c)))))
      (%disarm session))))

(defun debug-step (session &optional (n 1))
  "Execute up to N instructions on SESSION's machine one at a time, stopping
early on a trap, decode failure or watchpoint hit. Returns (VALUES REASON
STEPS [CONDITION]): REASON is :STEP (all N executed), :TRAP, :DECODE-FAILURE
or :WATCHPOINT; STEPS is the number of instructions actually executed."
  (%step-while session (lambda (steps cycles)
                         (declare (ignore cycles))
                         (< steps n))))

(defun %require-cycle-costs (session)
  "Signal unless SESSION's machine declares (cycles n) on some instruction."
  (let ((descriptor (machine-descriptor (debug-session-machine session))))
    (unless (loop for variants being the hash-values of (machine-descriptor-instructions descriptor)
                    thereis (some #'instruction-descriptor-cycles variants))
      (%debugger-usage-error "machine ~(~A~) declares no (cycles n) -- cycle budgets need per-instruction costs"
             (machine-descriptor-name descriptor)))))

(defun debug-step-cycles (session cycles &key (max-steps (max 10000 cycles)))
  "Execute instructions on SESSION's machine until CYCLES cycles have been
spent (overshooting by at most one instruction's cost), stopping early as
DEBUG-STEP does. Breakpoints are ignored. Returns (VALUES REASON STEPS
[CONDITION]): REASON is :STEP (budget spent), :MAX-STEPS when MAX-STEPS
instructions ran without spending it, or an early stop. Signals when the
machine declares no (cycles n)."
  (%require-cycle-costs session)
  (let ((spent 0))
    (multiple-value-bind (reason steps condition)
        (%step-while session (lambda (steps cycles-spent)
                               (setf spent cycles-spent)
                               (and (< steps max-steps) (< spent cycles))))
      (values (if (and (eq reason :step) (< spent cycles)) :max-steps reason)
              steps condition))))

(defun %require-replayable (session)
  "Signal unless SESSION keeps history and every device on its bus can be saved."
  (unless (debug-session-history session)
    (%debugger-usage-error "step-back history is off -- create the session with :history N"))
  (loop for device across (machine-devices (debug-session-machine session))
        when (and device (null (device-descriptor-save (device-descriptor device))))
          do (%debugger-usage-error "device ~(~A~) has no :save hook, so it cannot be stepped back over"
                    (device-descriptor-name (device-descriptor device)))))

(defun %restore-checkpoint (session checkpoint)
  "Put SESSION's machine in the state CHECKPOINT recorded: its anchor, then the
memory deltas up to it."
  (let* ((machine (debug-session-machine session))
         (chain (member checkpoint (debug-session-checkpoints session)))
         (anchor (find-if #'checkpoint-anchor-p chain))
         (deltas (reverse (ldiff chain (member anchor chain)))))
    (restore-snapshot machine (checkpoint-snapshot anchor))
    (when deltas
      (let ((arrays (%memory-arrays machine)))
        (dolist (delta deltas)
          (loop for (key . spans) in (checkpoint-diff delta)
                for array = (cdr (assoc key arrays :test #'equal))
                do (dolist (span spans)
                     (replace array (cdr span) :start1 (car span))))))
      (%restore-snapshot machine (checkpoint-snapshot checkpoint) nil))
    (setf (debug-session-shadow session) nil)))

(defun %replay (session checkpoint target &key watch on-step)
  "Restore CHECKPOINT and step forward to step TARGET, ignoring traps and
faults. ON-STEP is called with each step number after it runs; WATCH arms the
watchpoints around each step, leaving any hit in SESSION's WATCH-HIT."
  (%restore-checkpoint session checkpoint)
  (let ((machine (debug-session-machine session))
        (pc (debug-session-pc session))
        (memory (debug-session-memory session)))
    (setf (debug-session-replaying session) t)
    (unwind-protect
         (loop for step from (1+ (checkpoint-step checkpoint)) to target
               do (when watch
                    (setf (debug-session-watch-hit session) nil)
                    (%arm session))
                  (handler-case (step-machine machine :pc pc :memory memory)
                    ((or lasm-trap storage-error) ()))
                  (when watch (%disarm session))
                  (when on-step (funcall on-step step)))
      (setf (debug-session-replaying session) nil)
      (when watch (%disarm session)))))

(defun %travel (session target)
  "Move SESSION to step TARGET, no later than its current step, dropping the
checkpoints beyond it."
  (let ((base (find-if (lambda (c) (<= (checkpoint-step c) target))
                       (debug-session-checkpoints session))))
    (%replay session base target)
    (setf (debug-session-step-count session) target
          (debug-session-checkpoints session)
          (remove-if (lambda (c) (> (checkpoint-step c) target)) (debug-session-checkpoints session))
          (debug-session-hits session)
          (member target (debug-session-hits session) :key #'car :test #'>=))
    (when (and (debug-session-hits-from session) (> (debug-session-hits-from session) target))
      (setf (debug-session-hits-from session) nil))))

(defun debug-step-back (session &optional (n 1))
  "Undo the last N executed steps by restoring the nearest earlier checkpoint
and replaying forward. Returns (VALUES REASON UNDONE): REASON is :BACK, or
:HISTORY-START when fewer than N steps were recorded. Signals when SESSION
keeps no history, or a device on the bus has no :SAVE hook -- restoring would
reset it and replay could not reproduce its state."
  (%require-replayable session)
  (unless (typep n '(integer 1))
    (%debugger-usage-error "bad step count ~S" n))
  (let ((checkpoints (debug-session-checkpoints session))
        (now (debug-session-step-count session)))
    (when (null checkpoints)
      (return-from debug-step-back (values :history-start 0)))
    (let ((target (max (- now n) (checkpoint-step (car (last checkpoints))))))
      (%travel session target)
      (values (if (= (- now target) n) :back :history-start)
              (- now target)))))

(defun %segment-may-watch-p (session checkpoint)
  "True when a watchpoint's target was accessed the way it watches for in
CHECKPOINT's segment."
  (let ((accessed (checkpoint-accessed checkpoint)))
    (loop for wp in (debug-session-watchpoints session)
          for needed = (ecase (watchpoint-access wp) (:read 1) (:write 2) (:read-write 3))
          thereis (if (watchpoint-name wp)
                      (logtest needed (gethash (watchpoint-name wp) accessed 0))
                      (let ((pages (gethash (debug-session-memory session) accessed)))
                        (and pages
                             (logtest needed (gethash (ash (watchpoint-address wp) (- +dirty-page-bits+))
                                                      pages 0))))))))

(defun %recorded-hit (session now oldest)
  "The latest recorded hit before step NOW and no earlier than OLDEST, or NIL."
  (let ((from (debug-session-hits-from session))
        (hit (find-if (lambda (hit) (< (car hit) now)) (debug-session-hits session))))
    (and hit from (>= (car hit) (max from oldest)) hit)))

(defun %reverse-until (session stop-p &key (addresses :any) cached)
  "Move SESSION back to the latest earlier step where STOP-P, called with the
machine at that step, returns a reason, or where a watchpoint fired running
into it. Returns (VALUES REASON UNDONE [HIT]) as DEBUG-REVERSE-CONTINUE does.
ADDRESSES lists the PCs STOP-P can hold at, so a segment that reached none of
them is not replayed unless it accessed a watched target; CACHED says STOP-P is the
breakpoint test the recorded hits were made with."
  (%require-replayable session)
  (let* ((now (debug-session-step-count session))
         (checkpoints (debug-session-checkpoints session))
         (oldest (car (last checkpoints)))
         (upper (1- now)))
    (flet ((match ()
             (setf (debug-session-condition-error session) nil)
             (let ((reason (if (debug-session-watch-hit session) :watchpoint (funcall stop-p))))
               (and reason (list reason (or (debug-session-watch-hit session)
                                            (debug-session-condition-error session))))))
           (land (step reason hit)
             (%travel session step)
             (return-from %reverse-until (values reason (- now step) hit))))
      (when (or (null oldest) (>= (checkpoint-step oldest) now))
        (return-from %reverse-until (values :history-start 0)))
      (when cached
        (let ((hit (%recorded-hit session now (checkpoint-step oldest))))
          (when hit
            (land (first hit) (second hit) (third hit)))
          (when (debug-session-hits-from session)
            (setf upper (min upper (1- (debug-session-hits-from session)))))))
      (dolist (checkpoint checkpoints)
        (let ((found nil))
          (when (and (< (checkpoint-step checkpoint) upper)
                     (or (%segment-may-watch-p session checkpoint)
                         (eq addresses :any)
                         (some (lambda (address) (gethash address (checkpoint-pcs checkpoint)))
                               addresses)))
            (%replay session checkpoint upper :watch t
                     :on-step (lambda (step)
                                (let ((m (match)))
                                  (when m (setf found (cons step m))))))
            (when found
              (land (car found) (second found) (third found))))
          (setf upper (min upper (checkpoint-step checkpoint)))))
      (%restore-checkpoint session oldest)
      (setf (debug-session-watch-hit session) nil)
      (let ((m (match)))
        (if m
            (land (checkpoint-step oldest) (first m) (second m))
            (land (checkpoint-step oldest) :history-start nil))))))

(defun %breakpoint-stop (session)
  "T when a breakpoint that holds is at SESSION's PC in the mapped bank."
  (let ((breakpoints (debug-session-breakpoints session))
        (pc (%pc session)))
    (flet ((hit-p (key)
             (let ((bp (gethash key breakpoints)))
               (and bp (%breakpoint-triggered-p session bp)))))
      (or (hit-p (cons pc nil))
          (let ((bank (%mapped-bank session pc)))
            (and bank (hit-p (cons pc bank))))))))

(defun debug-reverse-continue (session)
  "Run SESSION backwards to the latest earlier step where a breakpoint whose
condition holds is at the PC, or where a watchpoint fired. The step SESSION is
at now never counts. Returns (VALUES REASON UNDONE [CONDITION]): REASON is
:BREAKPOINT (CONDITION is the error from a condition that failed to
evaluate), :WATCHPOINT (CONDITION is the WATCH-HIT, the same state a forward
continue stops in) or :HISTORY-START, leaving SESSION at its oldest recorded
step. Signals as DEBUG-STEP-BACK does."
  (%reverse-until session (lambda () (and (%breakpoint-stop session) :breakpoint))
                  :addresses (loop for bp being the hash-values of (debug-session-breakpoints session)
                                   collect (breakpoint-address bp))
                  :cached t))

(defun debug-reverse-continue-to (session where &key scope bank)
  "Like DEBUG-REVERSE-CONTINUE, but stops at the latest earlier step with the
PC at WHERE (DEBUG-CONTINUE-TO's target), with REASON :UNTIL in place of
:BREAKPOINT."
  (multiple-value-bind (address region bank)
      (%resolve-breakpoint-address session where :scope scope :bank bank)
    (declare (ignore region))
    (%reverse-until session
                    (lambda ()
                      (and (= (%pc session) address)
                           (or (null bank) (eql bank (%mapped-bank session address)))
                           :until))
                    :addresses (list address))))

(defun %run-until (session stop-p &key (max-steps 10000) (idle-stop t))
  "Shared body of DEBUG-CONTINUE/DEBUG-CONTINUE-TO: %RUN-LOOP (emulator.lisp)
against SESSION's machine, remapped from %RUN-LOOP's own reason vocabulary
into the debugger's (see this section's header comment). STOP-P is called
after each step and returns the stop reason (a keyword) or NIL to keep going."
  (let ((machine (debug-session-machine session))
        (start (debug-session-step-count session))
        (stopped nil))
    (setf (debug-session-watch-hit session) nil
          (debug-session-condition-error session) nil)
    (%start-command session)
    (multiple-value-bind (reason steps condition)
        (unwind-protect
             (progn
               (%arm session)
               (%run-loop machine :pc (debug-session-pc session) :memory (debug-session-memory session)
                                  :max-steps max-steps :stop-reason :stop
                                  :stop-p (lambda ()
                                            (%disarm session)
                                            (cond ((debug-session-watch-hit session) t)
                                                  ((setf stopped (funcall stop-p)) t)
                                                  (t (%arm session) nil)))
                                  :on-step (and (debug-session-history session)
                                                (lambda (cost)
                                                  (declare (ignore cost))
                                                  (%count-step session)))
                                  :idle-stop idle-stop))
          (%disarm session))
      (let ((counted (debug-session-step-count session)))
        (setf (debug-session-step-count session) (+ start steps))
        (when (and (debug-session-history session) (> (debug-session-step-count session) counted))
          (%record-step session)))
      (when (eq reason :stop)
        (cond ((debug-session-watch-hit session)
               (return-from %run-until
                 (values :watchpoint steps (debug-session-watch-hit session))))
              ((debug-session-condition-error session)
               (return-from %run-until
                 (values stopped steps (debug-session-condition-error session))))))
      (values (if (eq reason :stop) stopped reason) steps condition))))

(defun debug-continue (session &key max-steps cycles)
  "Run SESSION's machine until it hits a breakpoint whose condition holds or a
watchpoint, traps, hits a decode failure, goes idle with nothing left to wake
it, or MAX-STEPS instructions have executed with none of those happening (a
runaway-program guard, mirroring RUN's own; default 10000, or CYCLES if
larger). CYCLES adds a budget: the run also stops once that many cycles have
been spent, overshooting by at most one instruction's cost, and an idle
machine keeps running to spend it. Returns (VALUES REASON STEPS [CONDITION])
-- REASON one of :BREAKPOINT, :WATCHPOINT, :TRAP, :FAULT, :DECODE-FAILURE,
:IDLE, :MAX-CYCLES, :MAX-STEPS. A condition that fails to evaluate stops the
run as :BREAKPOINT with its error as CONDITION. Signals when CYCLES is given
and the machine declares no (cycles n).

STOP-P is checked *after* each step executes (%RUN-LOOP's own contract), so
continuing from a PC that is itself a breakpoint runs past it rather than
re-triggering immediately -- the same behaviour gdb's `continue` has when
already stopped on a breakpoint."
  (when cycles (%require-cycle-costs session))
  (let* ((machine (debug-session-machine session))
         (start-cycles (machine-cycles machine)))
    (%run-until session
                (lambda ()
                  (cond ((%breakpoint-stop-once session) :breakpoint)
                        ((and cycles (>= (- (machine-cycles machine) start-cycles) cycles))
                         :max-cycles)))
                :max-steps (or max-steps (if cycles (max 10000 cycles) 10000))
                :idle-stop (null cycles))))

(defun debug-continue-to (session where &key scope bank (max-steps 10000))
  "Like DEBUG-CONTINUE, but stops at WHERE (an address or label,
%RESOLVE-BREAKPOINT-ADDRESS) regardless of whether it has a breakpoint set
-- a one-shot \"run until here\". A banked target only stops while its bank
is mapped. Returns (VALUES REASON STEPS [CONDITION]) with REASON :UNTIL in
place of DEBUG-CONTINUE's :BREAKPOINT."
  (multiple-value-bind (address region bank)
      (%resolve-breakpoint-address session where :scope scope :bank bank)
    (declare (ignore region))
    (%run-until session
                (lambda ()
                  (and (= (%pc session) address)
                       (or (null bank) (eql bank (%mapped-bank session address)))
                       :until))
                :max-steps max-steps)))

;;; Inspection (read-only)
;;;
;;; Driven off MACHINE-DESCRIPTOR-ELEMENTS so nothing here is architecture-
;;; specific. A :REGISTER must branch on STORAGE-ELEMENT-COUNT: SREF/%SLOT-
;;; ANY (storage.lisp) *signal* UNKNOWN-STORAGE on a banked (:COUNT > 1)
;;; register (e.g. CHIP8's V0-VF, DCPU-16's A/B/C/X/Y/Z/I/J) -- a naive walk
;;; calling SREF on every register would crash on the first such machine.

(defun %register-value-text (machine element)
  (if (> (storage-element-count element) 1)
      (format nil "[~{~A~^ ~}]"
              (loop for i below (storage-element-count element)
                    for value = (regref machine (storage-element-name element) i)
                    for alias = (register-alias-at element i)
                    collect (if alias (format nil "~A=~D" alias value) value)))
      (format nil "~D" (sref machine (storage-element-name element)))))

(defun debug-state-text (session &key stream)
  "Render every storage element on SESSION's machine -- registers (scalar
and banked), flags, stacks (as live entries, top first), grouped by kind in
declaration order. Returns a string when STREAM is NIL (default); otherwise
writes to STREAM and returns NIL, same convention as LISTING-TEXT/SYMBOLS-
TEXT/DISASSEMBLY-TEXT."
  (let* ((machine (debug-session-machine session))
         (descriptor (machine-descriptor machine))
         (body (with-output-to-string (s)
                 (dolist (element (machine-descriptor-elements descriptor))
                   (ecase (storage-element-kind element)
                     (:register
                      (format s "~(~A~)~10T= ~A~%" (storage-element-name element)
                              (%register-value-text machine element)))
                     (:flag
                      (format s "~(~A~)~10T= ~D~%" (storage-element-name element)
                              (flag machine (storage-element-name element))))
                     (:stack
                      (let* ((name (storage-element-name element))
                             (depth (stack-depth machine name)))
                        (format s "~(~A~)~10T= [~{~D~^ ~}]  (depth ~D/~D)~%"
                                name
                                (loop for i below depth collect (stack-ref machine name i))
                                depth (storage-element-depth element))))
                     (:memory nil)))))) ; memory is inspected via DEBUG-MEMORY-TEXT/x, not dumped whole here
    (if stream (progn (write-string body stream) nil) body)))

(defun %session-banked-region (session address)
  "The banked MEMORY-REGION of SESSION's memory that contains ADDRESS, or NIL."
  (let ((element (descriptor-element (machine-descriptor (debug-session-machine session))
                                     (debug-session-memory session))))
    (find-if (lambda (r) (and (memory-region-banks r)
                              (<= (memory-region-start r) address (memory-region-end r))))
             (storage-element-regions element))))

(defun debug-banks-text (session &key stream)
  "One line per banked region on SESSION's machine: its name, address range
and current bank out of its bank count. Returns a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL."
  (let* ((machine (debug-session-machine session))
         (digits (debug-session-addr-digits session))
         (body (with-output-to-string (s)
                 (loop for (nil . region) in (%banked-regions (machine-descriptor machine))
                       do (format s "~(~A~)~10T~V,'0X-~V,'0X  bank ~D/~D~%"
                                  (memory-region-name region)
                                  digits (memory-region-start region)
                                  digits (memory-region-end region)
                                  (current-bank machine (memory-region-name region))
                                  (memory-region-banks region))))))
    (when (zerop (length body))
      (setf body (format nil "No banked regions.~%")))
    (if stream (progn (write-string body stream) nil) body)))

(defun debug-set-bank (session region bank)
  "Map BANK into banked region REGION on SESSION's machine. Signals
BANK-OUT-OF-RANGE for a bank the region lacks."
  (%forget-hits session)
  (setf (current-bank (debug-session-machine session) region) bank))

(defun debug-memory-text (session address count &key bank stream)
  "Render COUNT cells of SESSION's machine's memory starting at ADDRESS, in
rows of 8, each row labelled by address (ADDR-DIGITS) with each cell padded
to SESSION's own HEX-DIGITS, sized from the memory's actual cell width.
Returns a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL.

#107: reads via MPEEK, not MREF -- a hex dump is inspection, not an actual
CPU access, so it must not trigger a :DEVICE region's :READ side effects
merely by displaying memory.

BANK dumps that bank of the banked region containing ADDRESS via BANK-PEEK,
mapped in or not. Signals, before writing anything, if ADDRESS is not in a
banked region or the range runs past its end."
  (let* ((machine (debug-session-machine session))
         (region (and bank
                      (or (%session-banked-region session address)
                          (%debugger-usage-error "address ~D is not in a banked region" address))))
         (peek (if bank
                   (progn
                     (when (> (+ address count -1) (memory-region-end region))
                       (%debugger-usage-error "~D cells at ~D run past the end of region ~(~A~)"
                              count address (memory-region-name region)))
                     (lambda (a) (bank-peek machine (memory-region-name region) bank a)))
                   (lambda (a) (mpeek machine (debug-session-memory session) a))))
         (addr-digits (debug-session-addr-digits session))
         (cell-digits (debug-session-hex-digits session))
         (body (with-output-to-string (s)
                 (loop for row-start from address below (+ address count) by 8
                       do (format s "~V,'0X:" addr-digits row-start)
                          (loop for a from row-start below (min (+ row-start 8) (+ address count))
                                do (format s " ~V,'0X" cell-digits (funcall peek a)))
                          (format s "~%")))))
    (setf (debug-session-last-x-address session) (+ address count))
    (if stream (progn (write-string body stream) nil) body)))

(defun debug-where-text (session &key (context 4) (stream nil))
  "Render SESSION's current stop point: the PC, its disassembled instruction
via DISASSEMBLE-MEMORY (disassembler.lisp, passing the attached ASSEMBLY so
data renders as data and labels resolve), and -- when an ASSEMBLY is
attached -- the originating source line via LISTING-LINE-AT/ASSEMBLY-SOURCE.
CONTEXT bounds how many disassembled instructions are shown. Returns a
string when STREAM is NIL (default); otherwise writes to STREAM and returns
NIL."
  (let* ((session-assembly (debug-session-assembly session))
         (machine (debug-session-machine session))
         (pc (%pc session))
         (lines (disassemble-memory machine :memory (debug-session-memory session)
                                             :start pc :count context
                                             :assembly session-assembly))
         (body (with-output-to-string (s)
                 (format s "pc = ~V,'0X" (debug-session-addr-digits session) pc)
                 (multiple-value-bind (info offset)
                     (and session-assembly
                          (machine-label-at machine pc :memory (debug-session-memory session)
                                                       :assembly session-assembly))
                   (when info
                     (format s " <~A>" (label-offset-text info offset))))
                 (terpri s)
                 (when session-assembly
                   (let* ((line (machine-listing-line machine pc
                                                      :memory (debug-session-memory session)
                                                      :assembly session-assembly))
                          (source-line (and line (listing-line-source-text line session-assembly))))
                     (when source-line
                       (format s "~@[~A:~]~D:~A~%" (listing-line-file line)
                               (listing-line-line line) source-line))))
                 (dolist (l lines)
                   (format s "~V,'0X:  ~A~%" (debug-session-addr-digits session)
                           (disassembly-line-address l) (disassembly-line-text l))))))
    (if stream (progn (write-string body stream) nil) body)))

;;; Command dispatcher
;;;
;;; DEBUG-COMMAND parses one command line and returns response text --
;;; string in, string out, same shape as LISTING-TEXT et al. -- so every
;;; command is unit-testable by asserting on returned strings, with no
;;; *STANDARD-INPUT*/*STANDARD-OUTPUT* mocking. DEBUGGER-REPL is a thin loop
;;; on top of it.

(defun %parse-integer-maybe (text)
  "TEXT as an integer, honouring 0x/$/0b prefixes for hex/binary the way
LASM's own lexer does -- or NIL if TEXT does not parse as one."
  (let ((text (string-trim " " text)))
    (cond
      ((zerop (length text)) nil)
      ((and (> (length text) 1) (string-equal (subseq text 0 2) "0x"))
       (ignore-errors (parse-integer text :start 2 :radix 16)))
      ((char= (char text 0) #\$)
       (ignore-errors (parse-integer text :start 1 :radix 16)))
      ((and (> (length text) 1) (string-equal (subseq text 0 2) "0b"))
       (ignore-errors (parse-integer text :start 2 :radix 2)))
      (t (ignore-errors (parse-integer text))))))

(defun %where-arg (text)
  "TEXT as a breakpoint/continue-to target, as (VALUES WHERE BANK): an integer
address if it parses as one (%PARSE-INTEGER-MAYBE), otherwise the raw string
as a label name. A \"BANK:\" prefix supplies BANK; a non-numeric one signals."
  (let* ((colon (position #\: text))
         (bank (and colon (or (%parse-integer-maybe (subseq text 0 colon))
                              (%debugger-usage-error "bad bank ~S" (subseq text 0 colon)))))
         (target (string-trim " " (if colon (subseq text (1+ colon)) text))))
    (values (or (%parse-integer-maybe target) target) bank)))

(defun %count-arg (text command)
  "TEXT as (VALUES COUNT CYCLES-P): empty is 1, otherwise a positive `N` or
`N cycles`. Signals for anything else."
  (multiple-value-bind (number word) (%split-command text)
    (let ((count (if (zerop (length text)) 1 (%parse-integer-maybe number))))
      (unless (and count (plusp count)
                   (or (zerop (length word)) (string-equal word "cycles")))
        (%debugger-usage-error "~A: bad count ~S" command text))
      (values count (plusp (length word))))))

(defun %breakpoint-address-text (session address bank)
  (if bank
      (format nil "~2,'0D:~V,'0X" bank (debug-session-addr-digits session) address)
      (format nil "~V,'0X" (debug-session-addr-digits session) address)))

(defun %split-command (line)
  (let* ((line (string-trim " 	" line))
         (space (position #\Space line)))
    (if space
        (values (subseq line 0 space) (string-trim " " (subseq line (1+ space))))
        (values line ""))))

(defun %stop-text (session reason steps condition)
  (format nil "Stopped: ~(~A~)  steps=~D  pc=~V,'0X~%~@[~A~%~]" reason steps
          (debug-session-addr-digits session) (%pc session)
          (case reason
            (:fault (and condition (princ-to-string condition)))
            (:watchpoint (%watch-hit-text condition))
            (:breakpoint (and (typep condition 'condition)
                              (format nil "condition error: ~A" condition))))))

(defun %watch-hit-text (hit)
  (let ((wp (watch-hit-watchpoint hit)))
    (if (eq (watch-hit-access hit) :read)
        (format nil "Watchpoint ~D (r) ~A = ~D" (watchpoint-id wp) (watchpoint-label wp)
                (watch-hit-new hit))
        (format nil "Watchpoint ~D (w) ~A: ~:[-~;~:*~D~] -> ~D" (watchpoint-id wp) (watchpoint-label wp)
                (watch-hit-old hit) (watch-hit-new hit)))))

(defun %access-text (access)
  (ecase access (:read "r") (:write "w") (:read-write "rw")))

(defun %split-if (text)
  "TEXT split at its first \" if \" as (VALUES TARGET CONDITION), CONDITION NIL without one."
  (let ((at (search " if " text :test #'char-equal)))
    (if at
        (values (string-trim " " (subseq text 0 at)) (string-trim " " (subseq text (+ at 4))))
        (values text nil))))

(defun %split-in (text)
  "TEXT split at its last \" in \" as (VALUES TARGET SCOPE), SCOPE NIL without one."
  (let ((at (search " in " text :test #'char-equal :from-end t)))
    (if at
        (values (string-trim " " (subseq text 0 at)) (string-trim " " (subseq text (+ at 4))))
        (values text nil))))

(defun %split-index (session target scope)
  "TARGET as (VALUES NAME INDEX): NAME[EXPR] split apart with EXPR evaluated,
INDEX NIL without brackets."
  (let ((bracket (position #\[ target)))
    (if (and bracket (plusp bracket) (char= #\] (char target (1- (length target)))))
        (values (subseq target 0 bracket)
                (%eval-text session (subseq target (1+ bracket) (1- (length target))) scope))
        (values target nil))))

(defun %watch-args (text)
  "TEXT as (VALUES TARGET ACCESS), the optional trailing r/w/rw word split off."
  (multiple-value-bind (target mode)
      (let ((space (position #\Space text :from-end t)))
        (if space
            (values (string-trim " " (subseq text 0 space)) (subseq text (1+ space)))
            (values text "")))
    (let ((access (cdr (assoc mode '(("r" . :read) ("w" . :write) ("rw" . :read-write))
                              :test #'string-equal))))
      (if access (values target access) (values text :write)))))

(defun %split-assignment (text)
  "TEXT as (VALUES TARGET SCOPE EXPRESSION) around its first =, or NIL when
either side is blank."
  (let* ((equals (position #\= text))
         (target (and equals (string-trim " " (subseq text 0 equals))))
         (expression (and equals (string-trim " " (subseq text (1+ equals))))))
    (when (and equals (plusp (length target)) (plusp (length expression)))
      (multiple-value-bind (target scope) (%split-in target)
        (values target scope expression)))))

(defun %list-items (text)
  "The comma-separated items of a bracketed list TEXT such as \"[1, mem(2), 3]\",
or :NONE when TEXT is not bracketed. Commas inside parentheses do not split."
  (if (and (> (length text) 1) (char= (char text 0) #\[) (char= (char text (1- (length text))) #\]))
      (let ((depth 0) (start 1) items)
        (loop for i from 1 below (length text)
              for c = (char text i)
              do (case c
                   (#\( (incf depth))
                   (#\) (decf depth))
                   ((#\, #\]) (when (zerop depth)
                                (cl:push (string-trim " " (subseq text start i)) items)
                                (setf start (1+ i))))))
        (setf items (nreverse items))
        (if (equal items '("")) '() items))
      :none))

(defun %eval-text (session text scope)
  (multiple-value-bind (test values readers) (%compile-condition session text scope)
    (%eval-condition session test values readers)))

(defun %command-set (session rest)
  (multiple-value-bind (target scope text) (%split-assignment rest)
    (if (null target)
        "set: usage: set TARGET = EXPR"
        (let ((items (%list-items text)))
          (cond
            ((listp items)
             (format nil "~A = [~{~D~^, ~}]~%" target
                     (debug-set session target
                                (loop for item in items collect (%eval-text session item scope)))))
            (t
             (let ((value (%eval-text session text scope)))
               (multiple-value-bind (name index) (%split-index session target scope)
                 (format nil "~A = ~D~%" target
                         (if index
                             (debug-set session name value :index index)
                             (multiple-value-bind (where bank) (%where-arg target)
                               (debug-set session where value :bank bank :scope scope))))))))))))

(defun %command-write (session rest)
  (multiple-value-bind (target scope text) (%split-assignment rest)
    (cond
      ((null target) "write: usage: write TARGET = EXPR")
      ((listp (%list-items text)) (%debugger-usage-error "write only targets memory -- use set for a stack"))
      (t
       (let ((value (%eval-text session text scope)))
         (multiple-value-bind (where bank) (%where-arg target)
           (multiple-value-bind (cell stored-p)
               (debug-write session where value :bank bank :scope scope)
             (format nil "~A = ~D~:[ (dropped)~;~]~%" target cell stored-p))))))))

(defparameter *debug-help-text*
  "Commands:
  break ADDR|LABEL   set a breakpoint (LABEL may be a local, e.g. count.loop)
  break .LOCAL in GLOBAL  set a breakpoint on a local label of GLOBAL
  break BANK:ADDR    set a breakpoint that only stops while that bank is mapped
  break ... if EXPR  stop only while EXPR (registers, flags, labels, *, mem(ADDR)) is nonzero
  watch TARGET [r|w|rw]  stop when ADDR, LABEL, REG, REG[N], STACK, STACK[N] or a flag is accessed
  watch STACK.depth [w|rw]  stop when a fixed stack's depth changes
  set TARGET = EXPR  store EXPR in a register, flag, REG[N], STACK[N], ADDR, BANK:ADDR or LABEL
  set STACK.depth = EXPR  set a fixed stack's depth
  set STACK = [EXPR, ...]  replace a fixed stack's entries, bottom first
  write TARGET = EXPR  store EXPR at ADDR, BANK:ADDR or LABEL as the CPU would (ROM policy, device WRITE)
  delete ID|ADDR     remove a breakpoint or watchpoint (by id first, falling back to address)
  delete BANK:ADDR   remove the breakpoint at ADDR in one bank
  info break         list breakpoints and watchpoints
  info reg           dump registers/flags/stacks
  info banks         list banked regions and their current bank
  info sym           list symbols (requires an attached assembly)
  step [N]           execute N instructions (default 1)
  step N cycles      execute until N cycles have been spent
  back [N]           undo the last N steps (needs a session created with :history)
  reverse-continue   run back to the previous breakpoint or watchpoint hit (alias rc)
  reverse-until ADDR|LABEL  run back to the previous visit of ADDR/LABEL
  continue           run until a breakpoint, watchpoint, trap, or decode failure
  continue N cycles  as continue, also stopping once N cycles have been spent
  until ADDR|LABEL   run until ADDR/LABEL is reached (BANK:ADDR waits for a bank;
                     LABEL takes an `in GLOBAL` scope like break)
  print EXPR         print a register, alias or flag, or evaluate an expression
  print REG[N]       print a cell of a banked register (N may be an expression)
  print STACK[N]     print a live slot of a fixed stack, bottom first
  print STACK.depth  print a fixed stack's depth
                     REG[N], STACK[N] and STACK.depth also work in expressions and conditions
  x/N ADDR           dump N memory cells starting at ADDR
  x/N BANK:ADDR      dump N cells of a bank of the banked region at ADDR
  bank REGION N      map bank N into a banked region
  save PATH [binary] write the machine's state to a snapshot file (binary: compact)
  load PATH          restore the machine's state from a snapshot file
  where              show pc, current instruction, and source context
  help               this text
  quit               end the session
")

(defun debug-command (session line &key stream)
  "Parse and execute one debugger command LINE against SESSION, returning
its response text as its primary value (a string when STREAM is NIL, the
default; otherwise written to STREAM, primary value then NIL) plus a second
value, T exactly on \"quit\" -- the one command with no text of its own for
a caller to print, so it cannot be recognised by inspecting the first value
alone. An unrecognised command or a malformed argument returns/writes an
error message as ordinary response text -- it never signals, so a REPL loop
(or a caller batching commands) never needs a HANDLER-CASE of its own around
this call."
  (let ((body
          (handler-case
              (multiple-value-bind (cmd rest) (%split-command line)
                (cond
                  ((zerop (length cmd)) "")
                  ((string-equal cmd "help") *debug-help-text*)
                  ((string-equal cmd "break")
                   (if (zerop (length rest))
                       "break: missing address or label"
                       (multiple-value-bind (target condition) (%split-if rest)
                         (multiple-value-bind (target scope) (%split-in target)
                          (multiple-value-bind (where bank) (%where-arg target)
                           (let ((bp (debug-break session where :bank bank :scope scope
                                                                :condition condition)))
                             (format nil "Breakpoint ~D at ~A~%" (breakpoint-id bp)
                                     (%breakpoint-address-text session (breakpoint-address bp)
                                                               (breakpoint-bank bp)))))))))
                  ((string-equal cmd "watch")
                   (if (zerop (length rest))
                       "watch: missing target"
                       (multiple-value-bind (target access) (%watch-args rest)
                        (multiple-value-bind (target scope) (%split-in target)
                         (multiple-value-bind (name index) (%split-index session target scope)
                          (let ((wp (if index
                                        (debug-watch session name :access access :index index)
                                        (multiple-value-bind (where bank) (%where-arg target)
                                          (debug-watch session where :access access :bank bank
                                                                     :scope scope)))))
                           (format nil "Watchpoint ~D (~A) at ~A~%" (watchpoint-id wp)
                                   (%access-text access) (watchpoint-label wp))))))))
                  ((string-equal cmd "set") (%command-set session rest))
                  ((string-equal cmd "write") (%command-write session rest))
                  ((string-equal cmd "delete")
                   (if (zerop (length rest))
                       "delete: missing id or address"
                       (multiple-value-bind (target bank) (%where-arg rest)
                         (cond
                           ((and (integerp target) (null bank) (debug-unwatch session target))
                            (format nil "Deleted watchpoint ~D~%" target))
                           ((and (integerp target) (debug-unbreak session target :bank bank))
                            (format nil "Deleted breakpoint at/id ~A~%" rest))
                           (t "delete: no such breakpoint or watchpoint")))))
                  ((string-equal cmd "info")
                   (cond
                     ((string-equal rest "break")
                      (let ((bps (debug-breakpoints session))
                            (wps (debug-watchpoints session)))
                        (if (or bps wps)
                            (with-output-to-string (s)
                              (dolist (bp bps)
                                (format s "~D: ~A~@[ (~A)~]~@[ if ~A~]~%" (breakpoint-id bp)
                                        (%breakpoint-address-text session (breakpoint-address bp)
                                                                  (breakpoint-bank bp))
                                        (breakpoint-label bp) (breakpoint-condition bp)))
                              (dolist (wp wps)
                                (format s "~D: watch (~A) ~A~%" (watchpoint-id wp)
                                        (%access-text (watchpoint-access wp)) (watchpoint-label wp))))
                            (format nil "No breakpoints.~%"))))
                     ((string-equal rest "reg") (debug-state-text session))
                     ((string-equal rest "banks") (debug-banks-text session))
                     ((string-equal rest "sym")
                      (if (debug-session-assembly session)
                          (symbols-text (debug-session-assembly session))
                          (format nil "No assembly attached to this session.~%")))
                     (t (format nil "info: unknown subcommand ~S (try break/reg/banks/sym)" rest))))
                  ((string-equal cmd "step")
                   (multiple-value-bind (n cycles-p) (%count-arg rest "step")
                     (multiple-value-bind (reason steps condition)
                         (if cycles-p (debug-step-cycles session n) (debug-step session n))
                       (%stop-text session reason steps condition))))
                  ((string-equal cmd "back")
                   (multiple-value-bind (n cycles-p) (%count-arg rest "back")
                     (when cycles-p (%debugger-usage-error "back: bad count ~S" rest))
                     (multiple-value-bind (reason undone) (debug-step-back session n)
                       (%stop-text session reason undone nil))))
                  ((or (string-equal cmd "reverse-continue") (string-equal cmd "rc"))
                   (multiple-value-bind (reason undone condition) (debug-reverse-continue session)
                     (%stop-text session reason undone condition)))
                  ((string-equal cmd "reverse-until")
                   (if (zerop (length rest))
                       "reverse-until: missing address or label"
                       (multiple-value-bind (reason undone condition)
                           (multiple-value-bind (target scope) (%split-in rest)
                             (multiple-value-bind (where bank) (%where-arg target)
                               (debug-reverse-continue-to session where :bank bank :scope scope)))
                         (%stop-text session reason undone condition))))
                  ((string-equal cmd "continue")
                   (multiple-value-bind (reason steps condition)
                       (if (zerop (length rest))
                           (debug-continue session)
                           (multiple-value-bind (n cycles-p) (%count-arg rest "continue")
                             (unless cycles-p (%debugger-usage-error "continue: bad count ~S" rest))
                             (debug-continue session :cycles n)))
                     (%stop-text session reason steps condition)))
                  ((string-equal cmd "until")
                   (if (zerop (length rest))
                       "until: missing address or label"
                       (multiple-value-bind (reason steps condition)
                           (multiple-value-bind (target scope) (%split-in rest)
                             (multiple-value-bind (where bank) (%where-arg target)
                               (debug-continue-to session where :bank bank :scope scope)))
                         (%stop-text session reason steps condition))))
                  ((string-equal cmd "print")
                   (cond
                     ((zerop (length rest)) "print: missing name")
                     (t
                       (let* ((machine (debug-session-machine session))
                              (descriptor (machine-descriptor machine))
                              (symbol (find-symbol (string-upcase rest) :lasm))
                              (element (and symbol (gethash symbol (machine-descriptor-table descriptor))))
                              (alias-element (gethash rest (machine-descriptor-register-alias-elements descriptor))))
                         (cond
                           (alias-element
                            (format nil "~A = ~D~%" rest
                                    (regref machine (storage-element-name alias-element)
                                            (gethash rest (machine-descriptor-register-aliases descriptor)))))
                           ((and element (eq (storage-element-kind element) :flag))
                            (format nil "~A = ~D~%" rest (flag machine (storage-element-name element))))
                           ((and element (eq (storage-element-kind element) :register))
                            (format nil "~A = ~A~%" rest (%register-value-text machine element)))
                           (t (multiple-value-bind (test values readers)
                                  (%compile-condition session rest nil)
                                (format nil "~A = ~D~%" rest
                                        (%eval-condition session test values readers)))))))))
                  ((and (>= (length cmd) 2) (string-equal (subseq cmd 0 2) "x/"))
                   (let* ((n (or (%parse-integer-maybe (subseq cmd 2)) 8))
                          (colon (position #\: rest))
                          (bank (and colon (%parse-integer-maybe (subseq rest 0 colon))))
                          (address (and (plusp (length rest))
                                        (%parse-integer-maybe (if colon (subseq rest (1+ colon)) rest)))))
                     (cond
                       ((zerop (length rest)) "x/N: missing address")
                       ((or (null address) (and colon (null bank)))
                        (format nil "x/N: bad address ~S" rest))
                       (t (debug-memory-text session address n :bank bank)))))
                  ((string-equal cmd "bank")
                   (multiple-value-bind (region-text bank-text) (%split-command rest)
                     (let ((bank (%parse-integer-maybe bank-text))
                           (region (and (plusp (length region-text))
                                        (find-symbol (string-upcase region-text) :lasm))))
                       (if (and region bank)
                           (progn (debug-set-bank session region bank)
                                  (format nil "~(~A~) bank ~D~%" region bank))
                           "bank: usage: bank REGION N"))))
                  ((string-equal cmd "where") (debug-where-text session))
                  ((string-equal cmd "save")
                   (if (zerop (length rest))
                       "save: missing path"
                       (multiple-value-bind (path format) (%split-save-arguments rest)
                         (write-snapshot (machine-snapshot (debug-session-machine session)
                                                           :assembly (debug-session-assembly session))
                                         path :format format)
                         (format nil "saved ~A~%" path))))
                  ((string-equal cmd "load")
                   (if (zerop (length rest))
                       "load: missing path"
                       (progn (restore-snapshot (debug-session-machine session) (read-snapshot rest))
                              (%forget-hits session)
                              (setf (debug-session-shadow session) nil)
                              (format nil "loaded ~A~%" rest))))
                  ((string-equal cmd "quit") :quit)
                  (t (format nil "Unknown command ~S -- try \"help\"" cmd))))
            (file-error (c) (format nil "Error: ~A~%" c))
            (lasm-error (c) (format nil "Error: ~A~%" c)))))
    (let ((quit-p (eq body :quit))
          (text (if (eq body :quit) (format nil "Bye.~%") body)))
      (values (if stream (progn (write-string text stream) nil) text) quit-p))))

(defun %split-save-arguments (rest)
  "(VALUES PATH FORMAT) for the arguments of the save command: PATH, then an
optional trailing word `binary`."
  (let* ((line (string-trim '(#\Space #\Tab) rest))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) line :from-end t)))
    (if (and space (plusp space) (string-equal "binary" line :start2 (1+ space)))
        (values (string-trim '(#\Space #\Tab) (subseq line 0 space)) :binary)
        (values rest :sexp))))

(defun debugger-repl (session &key (input *standard-input*) (output *standard-output*) (prompt "(lasm-dbg) "))
  "A thin read/dispatch/print loop over DEBUG-COMMAND -- the ticket's
reference command-line front end. Reads one line at a time from INPUT,
dispatches it through DEBUG-COMMAND, writes the response to OUTPUT, and
exits on \"quit\" or end of input. Returns SESSION."
  (loop
    (write-string prompt output)
    (force-output output)
    (let ((line (read-line input nil nil)))
      (unless line (return))
      (multiple-value-bind (result quit-p) (debug-command session line :stream output)
        (declare (ignore result))
        (when quit-p (return)))))
  session)
