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
;;;; SCOPE -- read-only inspection only, apart from switching a banked
;;;; region's current bank; no poke/`set register` command. Reverse/step-back
;;;; execution and cycle-budgeted stepping are out of scope.

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
  (next-id 1 :type (integer 1))
  (last-x-address nil))         ; so a bare `x` without an address continues from the last one

(defun make-debug-session (machine &key assembly pc memory (lexer 'default))
  "Create a DEBUG-SESSION wrapping MACHINE (a live MACHINE instance,
storage.lisp), optionally with the ASSEMBLY that produced its program for
label breakpoints, symbol listing, and source-line context. PC/MEMORY
override the usual by-convention resolution (%RESOLVE-PC/%RESOLVE-MEMORY,
emulator.lisp), same as STEP-MACHINE's own keywords -- resolved once here,
not on every command. LEXER tokenizes breakpoint conditions."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (cell-width (%machine-cell-width machine-name memory)))
    (%make-debug-session
     :machine machine :assembly assembly :pc pc :memory memory :lexer lexer
     :cell-width cell-width :hex-digits (%listing-hex-digits cell-width))))

;;; Breakpoints

(defun %resolve-breakpoint-address (session where &key scope bank)
  "WHERE as (VALUES ADDRESS REGION BANK): an integer as-is, or a label string
resolved through ASSEMBLY-SYMBOL (listing.lisp, #37) against SESSION's attached
ASSEMBLY. A label defined under .BANK carries its region and bank; BANK
qualifies an integer address instead, and must agree with a label's own bank.
Signals a plain error when SESSION has no ASSEMBLY, when the name is
unbound, or when it names an assignment rather than a :LABEL -- its value is
not an address (the #81 ambiguity SYMBOL-INFO's KIND already resolves; a
bare ASSEMBLY-SYMBOLS lookup would reintroduce it here)."
  (etypecase where
    (integer
     (if bank
         (let ((region (or (%session-banked-region session where)
                           (error "address ~D is not in a banked region" where))))
           (unless (< bank (memory-region-banks region))
             (error "bank ~D is out of range for region ~(~A~) (~D bank~:P)"
                    bank (memory-region-name region) (memory-region-banks region)))
           (values where (memory-region-name region) bank))
         (values where nil nil)))
    (string
     (let ((assembly (debug-session-assembly session)))
       (unless assembly
         (error "no assembly attached to this session -- cannot resolve label ~S" where))
       (let ((info (assembly-symbol assembly where :scope scope)))
         (unless info
           (error "no symbol named ~S~@[ in scope ~S~]" where scope))
         (unless (eq (symbol-info-kind info) :label)
           (error "~S is a ~(~A~), not a label -- its value is not an address"
                  where (symbol-info-kind info)))
         (when (and bank (not (eql bank (symbol-info-bank info))))
           (error "~S is not in bank ~D" where bank))
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

(defun %resolve-storage (session name &optional index)
  "NAME as a scalar register, flag or register alias on SESSION's machine,
as (VALUES ELEMENT-NAME INDEX), or NIL when NAME is not a storage name.
INDEX selects a cell of a banked register. Signals when NAME is storage
that cannot be read as a single value."
  (let* ((descriptor (machine-descriptor (debug-session-machine session)))
         (alias-element (gethash name (machine-descriptor-register-alias-elements descriptor)))
         (symbol (find-symbol (string-upcase name) :lasm))
         (element (and symbol (gethash symbol (machine-descriptor-table descriptor)))))
    (cond
      (alias-element
       (when index (error "~A is a register alias and takes no index" name))
       (values (storage-element-name alias-element)
               (gethash name (machine-descriptor-register-aliases descriptor))))
      ((null element) nil)
      ((eq (storage-element-kind element) :flag)
       (when index (error "~A is a flag and takes no index" name))
       (values symbol nil))
      ((not (eq (storage-element-kind element) :register))
       (error "~A is not a register or flag" name))
      ((= (storage-element-count element) 1)
       (when index (error "~A is not a banked register" name))
       (values symbol nil))
      ((null index)
       (error "~A is a banked register -- name a cell as ~A[N] or use an alias" name name))
      ((not (< -1 index (storage-element-count element)))
       (error "index ~D is out of range for register ~A" index name))
      (t (values symbol index)))))

(defun %read-storage (machine name index)
  (%without-hook (machine)
    (cond
      (index (regref machine name index))
      ((eq (storage-element-kind (descriptor-element (machine-descriptor machine) name)) :flag)
       (flag machine name))
      (t (sref machine name)))))

(defun %condition-names (ast)
  "The distinct EXPR-LABEL names in AST. Signals on an assembler-only
operator (bank, defined, lowcell, highcell)."
  (let (names)
    (labels ((walk (node)
               (etypecase node
                 ((or expr-number expr-location) nil)
                 (expr-label (pushnew (expr-label-name node) names :test #'string=))
                 (expr-unary
                  (when (member (expr-unary-op node) '(:bank :defined :lowcell :highcell))
                    (error "~(~A~)() is not available in a condition" (expr-unary-op node)))
                  (walk (expr-unary-operand node)))
                 (expr-binary (walk (expr-binary-left node))
                              (walk (expr-binary-right node))))))
      (walk ast))
    (nreverse names)))

(defun %compile-condition (session text scope)
  "Parse TEXT and bind every name in it, as (VALUES AST VALUES READERS): a
register, flag or alias becomes a live reader, anything else a label or .equ
of SESSION's assembly, looked up under SCOPE first, then globally. Signals
on a syntax error or an unknown name."
  (let* ((tokens (coerce (tokenize text :lexer (debug-session-lexer session)) 'simple-vector))
         (end (or (position :eof tokens :key #'token-type) (length tokens)))
         (values (make-hash-table :test 'equal))
         readers)
    (multiple-value-bind (ast next) (parse-expression tokens :end end)
      (when (< next end)
        (error "unexpected ~S in condition" (token-text (aref tokens next))))
      (dolist (name (%condition-names ast))
        (multiple-value-bind (storage index) (%resolve-storage session name)
          (let* ((assembly (and (null storage) (debug-session-assembly session)))
                 (info (and assembly
                            (or (and scope (assembly-symbol assembly name :scope scope))
                                (assembly-symbol assembly name)))))
            (cond
              (storage
               (let ((machine (debug-session-machine session)))
                 (cl:push (cons name (lambda () (%read-storage machine storage index))) readers)))
              (info (setf (gethash name values) (symbol-info-value info)))
              (t (error "unknown name ~S in condition" name))))))
      (values ast values readers))))

(defun %breakpoint-triggered-p (session bp)
  "True when BP has no condition or its condition is nonzero. An error in the
condition counts as true and is left in SESSION's CONDITION-ERROR."
  (or (null (breakpoint-test bp))
      (handler-case
          (progn
            (loop for (name . reader) in (breakpoint-readers bp)
                  do (setf (gethash name (breakpoint-values bp)) (funcall reader)))
            (/= 0 (eval-expr (breakpoint-test bp) :symbols (breakpoint-values bp)
                                                  :pc (%pc session))))
        (error (c) (setf (debug-session-condition-error session) c) t))))

(defun debug-break (session where &key scope bank condition)
  "Set a breakpoint at WHERE (an address, or a label name resolved via
%RESOLVE-BREAKPOINT-ADDRESS) on SESSION. A label in a banked region, or an
address given with BANK, only stops while that bank is mapped. CONDITION, a
string, is an expression over registers, flags, register aliases, labels and
.equs of the attached assembly (SCOPE qualifies local labels), with * as the
PC; the breakpoint only stops while it is nonzero. A bad condition signals
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
takes INDEX), a label, or a memory address; a name that is both storage and
a label is storage. ACCESS is :READ, :WRITE or :READ-WRITE. SCOPE and BANK
qualify a memory target as in DEBUG-BREAK. Returns the new WATCHPOINT."
  (unless (member access '(:read :write :read-write))
    (error "bad watch access ~S -- expected :READ, :WRITE or :READ-WRITE" access))
  (multiple-value-bind (name register-index)
      (and (stringp target) (%resolve-storage session target index))
    (let ((wp (if name
                  (make-watchpoint :id (debug-session-next-id session) :name name :index register-index :access access
                                   :label (cond ((null register-index) (string-downcase target))
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
      wp)))

(defun debug-unwatch (session id)
  "Remove the watchpoint with ID from SESSION. Returns T if one was removed."
  (let ((wp (find id (debug-session-watchpoints session) :key #'watchpoint-id)))
    (when wp
      (setf (debug-session-watchpoints session) (remove wp (debug-session-watchpoints session)))
      t)))

(defun debug-watchpoints (session)
  "SESSION's watchpoints, ascending by id."
  (copy-list (debug-session-watchpoints session)))

(defun %watch-matches-p (session wp name index access)
  (and (or (eq (watchpoint-access wp) :read-write) (eq (watchpoint-access wp) access))
       (if (watchpoint-name wp)
           (and (eq name (watchpoint-name wp)) (eql index (watchpoint-index wp)))
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
                 :old (if (eq access :read)
                          value
                          (if (watchpoint-name wp)
                              (%read-storage machine name index)
                              (mpeek machine name index))))))))))

(defun %arm (session)
  (when (debug-session-watchpoints session)
    (setf (machine-access-hook (debug-session-machine session)) (%watch-hook session))))

(defun %disarm (session)
  (when (debug-session-watchpoints session)
    (setf (machine-access-hook (debug-session-machine session)) nil)))

;;; Execution
;;;
;;; DEBUG-STEP/DEBUG-CONTINUE/DEBUG-CONTINUE-TO return (VALUES REASON STEPS
;;; [CONDITION]). A trap or fault supplies the condition as the third value; a
;;; :WATCHPOINT stop supplies its WATCH-HIT, and a :BREAKPOINT stop the error
;;; from its condition, if that failed.
;;; DEBUG-STEP reports :STEP after N steps rather than :MAX-STEPS.
;;;
;;; #110: an idle STEP-MACHINE result is not a stop condition here either --
;;; DEBUG-STEP counts it as one of its N steps (so single-stepping through a
;;; sleeping machine just burns steps one at a time, same as any other
;;; instruction), and DEBUG-CONTINUE/DEBUG-CONTINUE-TO forward %RUN-LOOP's
;;; own :IDLE straight through (see %RUN-UNTIL below) exactly like :TRAP/
;;; :DECODE-FAILURE.

(defun %pc (session)
  (sref (debug-session-machine session) (debug-session-pc session)))

(defun debug-step (session &optional (n 1))
  "Execute up to N instructions on SESSION's machine one at a time, stopping
early on a trap, decode failure or watchpoint hit. Returns (VALUES REASON
STEPS [CONDITION]): REASON is :STEP (all N executed), :TRAP, :DECODE-FAILURE
or :WATCHPOINT; STEPS is the number of instructions actually executed."
  (let ((machine (debug-session-machine session))
        (pc (debug-session-pc session))
        (memory (debug-session-memory session)))
    (setf (debug-session-watch-hit session) nil)
    (unwind-protect
         (dotimes (i n (values :step n))
           (handler-case
               (progn
                 (%arm session)
                 (multiple-value-bind (result) (step-machine machine :pc pc :memory memory)
                   (%disarm session)
                   (when (eq result :decode-failure)
                     (return-from debug-step (values :decode-failure i)))
                   (let ((hit (debug-session-watch-hit session)))
                     (when hit
                       (return-from debug-step (values :watchpoint (1+ i) hit))))))
             (lasm-trap (c) (return-from debug-step (values :trap (1+ i) c)))))
      (%disarm session))))

(defun %run-until (session stop-reason stop-p &key (max-steps 10000))
  "Shared body of DEBUG-CONTINUE/DEBUG-CONTINUE-TO: %RUN-LOOP (emulator.lisp)
against SESSION's machine, remapped from %RUN-LOOP's own reason vocabulary
into the debugger's (see this section's header comment)."
  (let ((machine (debug-session-machine session)))
    (setf (debug-session-watch-hit session) nil
          (debug-session-condition-error session) nil)
    (multiple-value-bind (reason steps condition)
        (unwind-protect
             (progn
               (%arm session)
               (%run-loop machine :pc (debug-session-pc session) :memory (debug-session-memory session)
                                  :max-steps max-steps :stop-reason stop-reason
                                  :stop-p (lambda ()
                                            (%disarm session)
                                            (cond ((debug-session-watch-hit session) t)
                                                  ((funcall stop-p) t)
                                                  (t (%arm session) nil)))
                                  :idle-stop t))
          (%disarm session))
      (when (eq reason stop-reason)
        (cond ((debug-session-watch-hit session)
               (return-from %run-until
                 (values :watchpoint steps (debug-session-watch-hit session))))
              ((debug-session-condition-error session)
               (return-from %run-until
                 (values reason steps (debug-session-condition-error session))))))
      (values (case reason
                (:trap :trap)
                (:fault :fault)
                (:decode-failure :decode-failure)
                (:idle :idle) ; #110 -- the machine went idle with nothing left to wake it
                (:max-steps :max-steps)
                (t reason)) ; STOP-REASON itself (:BREAKPOINT or :UNTIL), passed through
              steps condition))))

(defun debug-continue (session &key (max-steps 10000))
  "Run SESSION's machine until it hits a breakpoint whose condition holds or a
watchpoint, traps, hits a decode failure, goes idle with nothing left to wake
it (#110), or MAX-STEPS instructions have executed with none of those
happening (a runaway-program guard, mirroring RUN's own). Returns (VALUES
REASON STEPS [CONDITION]) -- REASON one of :BREAKPOINT, :WATCHPOINT, :TRAP,
:FAULT, :DECODE-FAILURE, :IDLE, :MAX-STEPS. A condition that fails to evaluate
stops the run as :BREAKPOINT with its error as CONDITION.

STOP-P is checked *after* each step executes (%RUN-LOOP's own contract), so
continuing from a PC that is itself a breakpoint runs past it rather than
re-triggering immediately -- the same behaviour gdb's `continue` has when
already stopped on a breakpoint."
  (let ((breakpoints (debug-session-breakpoints session)))
    (%run-until session :breakpoint
                (lambda ()
                  (flet ((hit-p (key)
                           (let ((bp (gethash key breakpoints)))
                             (and bp (%breakpoint-triggered-p session bp)))))
                    (let ((pc (%pc session)))
                      (or (hit-p (cons pc nil))
                          (let ((bank (%mapped-bank session pc)))
                            (and bank (hit-p (cons pc bank))))))))
                :max-steps max-steps)))

(defun debug-continue-to (session where &key scope bank (max-steps 10000))
  "Like DEBUG-CONTINUE, but stops at WHERE (an address or label,
%RESOLVE-BREAKPOINT-ADDRESS) regardless of whether it has a breakpoint set
-- a one-shot \"run until here\". A banked target only stops while its bank
is mapped. Returns (VALUES REASON STEPS [CONDITION]) with REASON :UNTIL in
place of DEBUG-CONTINUE's :BREAKPOINT."
  (multiple-value-bind (address region bank)
      (%resolve-breakpoint-address session where :scope scope :bank bank)
    (declare (ignore region))
    (%run-until session :until
                (lambda ()
                  (and (= (%pc session) address)
                       (or (null bank) (eql bank (%mapped-bank session address)))))
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
                          (error "address ~D is not in a banked region" address))))
         (peek (if bank
                   (progn
                     (when (> (+ address count -1) (memory-region-end region))
                       (error "~D cells at ~D run past the end of region ~(~A~)"
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

(defun %pc-listing-line (session assembly pc)
  "The listing line for PC: the one in the mapped bank when PC lies in a
banked region, else (or failing that) the main image's."
  (let ((region (%session-banked-region session pc)))
    (or (and region
             (let ((name (memory-region-name region)))
               (listing-line-at assembly pc :region name
                                            :bank (current-bank (debug-session-machine session) name))))
        (listing-line-at assembly pc))))

(defun debug-where-text (session &key (context 4) (stream nil))
  "Render SESSION's current stop point: the PC, its disassembled instruction
via DISASSEMBLE-MEMORY (disassembler.lisp, passing the attached ASSEMBLY's
SYMBOL-INFO when present so labels resolve), and -- when an ASSEMBLY is
attached -- the originating source line via LISTING-LINE-AT/ASSEMBLY-SOURCE.
CONTEXT bounds how many disassembled instructions are shown. Returns a
string when STREAM is NIL (default); otherwise writes to STREAM and returns
NIL."
  (let* ((session-assembly (debug-session-assembly session))
         (machine (debug-session-machine session))
         (pc (%pc session))
         ;; TODO: labels from every bank are substituted, not just the mapped one (#234)
         (lines (disassemble-memory machine :memory (debug-session-memory session)
                                             :start pc :count context
                                             :symbol-info (and session-assembly
                                                                (assembly-symbol-info session-assembly))))
         (body (with-output-to-string (s)
                 (format s "pc = ~V,'0X~%" (debug-session-addr-digits session) pc)
                 (when session-assembly
                   (let ((line (%pc-listing-line session session-assembly pc)))
                     (when (and line (assembly-source session-assembly))
                       (let ((source-line (nth (1- (listing-line-line line))
                                                (%split-source-lines (assembly-source session-assembly)))))
                         (when source-line
                           (format s "~D:~A~%" (listing-line-line line) source-line))))))
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
                              (error "bad bank ~S" (subseq text 0 colon)))))
         (target (string-trim " " (if colon (subseq text (1+ colon)) text))))
    (values (or (%parse-integer-maybe target) target) bank)))

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
        (format nil "Watchpoint ~D (w) ~A: ~D -> ~D" (watchpoint-id wp) (watchpoint-label wp)
                (watch-hit-old hit) (watch-hit-new hit)))))

(defun %access-text (access)
  (ecase access (:read "r") (:write "w") (:read-write "rw")))

(defun %split-if (text)
  "TEXT split at its first \" if \" as (VALUES TARGET CONDITION), CONDITION NIL without one."
  (let ((at (search " if " text :test #'char-equal)))
    (if at
        (values (string-trim " " (subseq text 0 at)) (string-trim " " (subseq text (+ at 4))))
        (values text nil))))

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

(defparameter *debug-help-text*
  "Commands:
  break ADDR|LABEL   set a breakpoint
  break BANK:ADDR    set a breakpoint that only stops while that bank is mapped
  break ... if EXPR  stop only while EXPR (registers, flags, labels, *) is nonzero
  watch TARGET [r|w|rw]  stop when ADDR, LABEL, REG, REG[N] or a flag is accessed
  delete ID|ADDR     remove a breakpoint or watchpoint (by id first, falling back to address)
  delete BANK:ADDR   remove the breakpoint at ADDR in one bank
  info break         list breakpoints and watchpoints
  info reg           dump registers/flags/stacks
  info banks         list banked regions and their current bank
  info sym           list symbols (requires an attached assembly)
  step [N]           execute N instructions (default 1)
  continue           run until a breakpoint, watchpoint, trap, or decode failure
  until ADDR|LABEL   run until ADDR/LABEL is reached (BANK:ADDR waits for a bank)
  print NAME         print a register, register alias or flag's value
  x/N ADDR           dump N memory cells starting at ADDR
  x/N BANK:ADDR      dump N cells of a bank of the banked region at ADDR
  bank REGION N      map bank N into a banked region
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
                         (multiple-value-bind (where bank) (%where-arg target)
                           (let ((bp (debug-break session where :bank bank :condition condition)))
                             (format nil "Breakpoint ~D at ~A~%" (breakpoint-id bp)
                                     (%breakpoint-address-text session (breakpoint-address bp)
                                                               (breakpoint-bank bp))))))))
                  ((string-equal cmd "watch")
                   (if (zerop (length rest))
                       "watch: missing target"
                       (multiple-value-bind (target access) (%watch-args rest)
                         (let* ((bracket (position #\[ target))
                                (close (and bracket (position #\] target :start bracket)))
                                (index (and close (%parse-integer-maybe (subseq target (1+ bracket) close))))
                                (wp (if index
                                        (debug-watch session (subseq target 0 bracket)
                                                     :access access :index index)
                                        (multiple-value-bind (where bank) (%where-arg target)
                                          (debug-watch session where :access access :bank bank)))))
                           (format nil "Watchpoint ~D (~A) at ~A~%" (watchpoint-id wp)
                                   (%access-text access) (watchpoint-label wp))))))
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
                   (let ((n (or (%parse-integer-maybe rest) 1)))
                     (multiple-value-bind (reason steps condition) (debug-step session n)
                       (%stop-text session reason steps condition))))
                  ((string-equal cmd "continue")
                   (multiple-value-bind (reason steps condition) (debug-continue session)
                     (%stop-text session reason steps condition)))
                  ((string-equal cmd "until")
                   (if (zerop (length rest))
                       "until: missing address or label"
                       (multiple-value-bind (reason steps condition)
                           (multiple-value-bind (where bank) (%where-arg rest)
                             (debug-continue-to session where :bank bank))
                         (%stop-text session reason steps condition))))
                  ((string-equal cmd "print")
                   (if (zerop (length rest))
                       "print: missing name"
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
                           ((null element) (format nil "print: unknown storage element ~A" rest))
                           ((eq (storage-element-kind element) :flag)
                            (format nil "~A = ~D~%" rest (flag machine (storage-element-name element))))
                           ((eq (storage-element-kind element) :register)
                            (format nil "~A = ~A~%" rest (%register-value-text machine element)))
                           (t (format nil "print: ~A is not a register or flag" rest))))))
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
                  ((string-equal cmd "quit") :quit)
                  (t (format nil "Unknown command ~S -- try \"help\"" cmd))))
            (error (c) (format nil "Error: ~A~%" c)))))
    (let ((quit-p (eq body :quit))
          (text (if (eq body :quit) (format nil "Bye.~%") body)))
      (values (if stream (progn (write-string text stream) nil) text) quit-p))))

(defun debugger-repl (session &key (input *standard-input*) (output *standard-output*) (prompt "(lasm-db) "))
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
