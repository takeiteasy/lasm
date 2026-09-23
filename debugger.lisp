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
;;;; SCOPE -- read-only inspection only; no poke/`set register` command.
;;;; Watchpoints, reverse/step-back execution, conditional breakpoints, and
;;;; cycle-budgeted stepping are all
;;;; explicitly out of scope for this ticket; see the follow-up tickets filed
;;;; alongside this file's landing.

(in-package #:lasm)

;;; Session

(defstruct breakpoint
  (id 0 :type (integer 1))
  (address 0 :type unsigned-byte)
  (label nil :type (or null string))) ; the name it was set by, if any

(defstruct (debug-session (:constructor %make-debug-session))
  (machine nil :type machine)
  (assembly nil)                ; optional ASSEMBLY -- symbols + source context
  (pc nil :type symbol)         ; resolved once, not re-resolved per command (#70's shape)
  (memory nil :type symbol)
  (cell-width nil :type (integer 1))
  (hex-digits nil :type (integer 1))   ; digits for one MEMORY cell's value (%listing-hex-digits)
  (addr-digits 4 :type (integer 1))    ; digits for an ADDRESS -- fixed at 4, matching
                                        ; LISTING-TEXT/PRINT-DISASSEMBLY's own address column

  (breakpoints (make-hash-table) :type hash-table) ; address -> breakpoint
  (next-id 1 :type (integer 1))
  (last-x-address nil))         ; so a bare `x` without an address continues from the last one

(defun make-debug-session (machine &key assembly pc memory)
  "Create a DEBUG-SESSION wrapping MACHINE (a live MACHINE instance,
storage.lisp), optionally with the ASSEMBLY that produced its program for
label breakpoints, symbol listing, and source-line context. PC/MEMORY
override the usual by-convention resolution (%RESOLVE-PC/%RESOLVE-MEMORY,
emulator.lisp), same as STEP-MACHINE's own keywords -- resolved once here,
not on every command."
  (let* ((machine-name (machine-descriptor-name (machine-descriptor machine)))
         (pc (%resolve-pc machine-name pc))
         (memory (%resolve-memory machine-name memory))
         (cell-width (%machine-cell-width machine-name memory)))
    (%make-debug-session
     :machine machine :assembly assembly :pc pc :memory memory
     :cell-width cell-width :hex-digits (%listing-hex-digits cell-width))))

;;; Breakpoints

(defun %resolve-breakpoint-address (session where &key scope)
  "WHERE as an address: an integer as-is, or a label string resolved through
ASSEMBLY-SYMBOL (listing.lisp, #37) against SESSION's attached ASSEMBLY.
Signals a plain error when SESSION has no ASSEMBLY, when the name is
unbound, or when it names an assignment rather than a :LABEL -- its value is
not an address (the #81 ambiguity SYMBOL-INFO's KIND already resolves; a
bare ASSEMBLY-SYMBOLS lookup would reintroduce it here)."
  (etypecase where
    (integer where)
    (string
     (let ((assembly (debug-session-assembly session)))
       (unless assembly
         (error "DEBUG-BREAK: no assembly attached to this session -- cannot break on label ~S" where))
       (let ((info (assembly-symbol assembly where :scope scope)))
         (unless info
           (error "DEBUG-BREAK: no symbol named ~S~@[ in scope ~S~]" where scope))
         (unless (eq (symbol-info-kind info) :label)
           (error "DEBUG-BREAK: ~S is a ~(~A~), not a label -- its value is not an address"
                  where (symbol-info-kind info)))
         (symbol-info-value info))))))

(defun debug-break (session where &key scope)
  "Set a breakpoint at WHERE (an address, or a label name resolved via
%RESOLVE-BREAKPOINT-ADDRESS) on SESSION. Returns the new BREAKPOINT. Setting
a second breakpoint at an address that already has one replaces it (same
id space, but the old entry is gone) rather than stacking duplicates --
there is only ever one stop per address."
  (let* ((address (%resolve-breakpoint-address session where :scope scope))
         (bp (make-breakpoint :id (debug-session-next-id session)
                               :address address
                               :label (and (stringp where) where))))
    (incf (debug-session-next-id session))
    (setf (gethash address (debug-session-breakpoints session)) bp)
    bp))

(defun debug-unbreak (session id-or-address)
  "Remove a breakpoint from SESSION by its BREAKPOINT-ID or by address.
Returns T if one was removed, NIL if ID-OR-ADDRESS named none.

ID-OR-ADDRESS is tried as a BREAKPOINT-ID first (gdb's own `delete N`
convention -- ids are the number `info break`/`break` itself prints, and are
what a caller almost always means), falling back to an address match only
when no breakpoint has that id. Ids and addresses are both plain integers
with no reserved ranges of their own, so the two CAN collide (breakpoint 1
happens to sit at address 1) -- id takes priority in that case, matching
what a human typing a small integer after `delete` almost certainly meant."
  (let ((table (debug-session-breakpoints session))
        hit)
    (maphash (lambda (addr bp)
               (when (eql (breakpoint-id bp) id-or-address)
                 (setf hit addr)))
             table)
    (cond
      (hit (remhash hit table) t)
      ((gethash id-or-address table) (remhash id-or-address table) t)
      (t nil))))

(defun debug-breakpoints (session)
  "SESSION's live breakpoints, as a list of BREAKPOINT, ascending by address."
  (let (result)
    (maphash (lambda (addr bp) (declare (ignore addr)) (cl:push bp result)) (debug-session-breakpoints session))
    (sort result #'< :key #'breakpoint-address)))

;;; Execution
;;;
;;; DEBUG-STEP/DEBUG-CONTINUE/DEBUG-CONTINUE-TO return (VALUES REASON STEPS
;;; [CONDITION]). A trap or fault supplies the condition as the third value.
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
early on a trap or decode failure. Returns (VALUES REASON STEPS): REASON is
:STEP (all N executed), :TRAP, or :DECODE-FAILURE; STEPS is the number of
instructions actually executed."
  (let ((machine (debug-session-machine session))
        (pc (debug-session-pc session))
        (memory (debug-session-memory session)))
    (dotimes (i n (values :step n))
      (handler-case
          (multiple-value-bind (result) (step-machine machine :pc pc :memory memory)
            (when (eq result :decode-failure)
              (return-from debug-step (values :decode-failure i))))
        (lasm-trap (c) (return-from debug-step (values :trap (1+ i) c)))))))

(defun %run-until (session stop-reason stop-p &key (max-steps 10000))
  "Shared body of DEBUG-CONTINUE/DEBUG-CONTINUE-TO: %RUN-LOOP (emulator.lisp)
against SESSION's machine, remapped from %RUN-LOOP's own reason vocabulary
into the debugger's (see this section's header comment)."
  (let ((machine (debug-session-machine session)))
    (multiple-value-bind (reason steps condition)
        (%run-loop machine :pc (debug-session-pc session) :memory (debug-session-memory session)
                            :max-steps max-steps :stop-reason stop-reason :stop-p stop-p
                            :idle-stop t)
      (values (case reason
                (:trap :trap)
                (:fault :fault)
                (:decode-failure :decode-failure)
                (:idle :idle) ; #110 -- the machine went idle with nothing left to wake it
                (:max-steps :max-steps)
                (t reason)) ; STOP-REASON itself (:BREAKPOINT or :UNTIL), passed through
              steps condition))))

(defun debug-continue (session &key (max-steps 10000))
  "Run SESSION's machine until it hits a breakpoint, traps, hits a decode
failure, goes idle with nothing left to wake it (#110), or MAX-STEPS
instructions have executed with none of those happening (a runaway-program
guard, mirroring RUN's own). Returns (VALUES REASON STEPS [CONDITION]) --
REASON one of :BREAKPOINT, :TRAP, :FAULT, :DECODE-FAILURE, :IDLE, :MAX-STEPS.

STOP-P is checked *after* each step executes (%RUN-LOOP's own contract), so
continuing from a PC that is itself a breakpoint runs past it rather than
re-triggering immediately -- the same behaviour gdb's `continue` has when
already stopped on a breakpoint."
  (let ((breakpoints (debug-session-breakpoints session)))
    (%run-until session :breakpoint
                (lambda () (nth-value 1 (gethash (%pc session) breakpoints)))
                :max-steps max-steps)))

(defun debug-continue-to (session where &key scope (max-steps 10000))
  "Like DEBUG-CONTINUE, but stops at WHERE (an address or label,
%RESOLVE-BREAKPOINT-ADDRESS) regardless of whether it has a breakpoint set
-- a one-shot \"run until here\". Returns (VALUES REASON STEPS [CONDITION])
with REASON :UNTIL in place of DEBUG-CONTINUE's :BREAKPOINT."
  (let ((address (%resolve-breakpoint-address session where :scope scope)))
    (%run-until session :until (lambda () (= (%pc session) address)) :max-steps max-steps)))

;;; Inspection (read-only)
;;;
;;; Driven off MACHINE-DESCRIPTOR-ELEMENTS so nothing here is architecture-
;;; specific. A :REGISTER must branch on STORAGE-ELEMENT-COUNT: SREF/%SLOT-
;;; ANY (storage.lisp) *signal* UNKNOWN-STORAGE on a banked (:COUNT > 1)
;;; register (e.g. CHIP8's V0-VF, DCPU-16's A/B/C/X/Y/Z/I/J) -- a naive walk
;;; calling SREF on every register would crash on the first such machine.

(defun %register-value-text (machine element)
  (if (> (storage-element-count element) 1)
      (format nil "[~{~D~^ ~}]"
              (loop for i below (storage-element-count element)
                    collect (regref machine (storage-element-name element) i)))
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

(defun debug-memory-text (session address count &key stream)
  "Render COUNT cells of SESSION's machine's memory starting at ADDRESS, in
rows of 8, each row labelled by address (ADDR-DIGITS) with each cell padded
to SESSION's own HEX-DIGITS, sized from the memory's actual cell width.
Returns a string when STREAM is NIL
(default); otherwise writes to STREAM and returns NIL.

#107: reads via MPEEK, not MREF -- a hex dump is inspection, not an actual
CPU access, so it must not trigger a :DEVICE region's :READ side effects
merely by displaying memory."
  (let* ((machine (debug-session-machine session))
         (memory (debug-session-memory session))
         (addr-digits (debug-session-addr-digits session))
         (cell-digits (debug-session-hex-digits session))
         (body (with-output-to-string (s)
                 (loop for row-start from address below (+ address count) by 8
                       do (format s "~V,'0X:" addr-digits row-start)
                          (loop for a from row-start below (min (+ row-start 8) (+ address count))
                                do (format s " ~V,'0X" cell-digits (mpeek machine memory a)))
                          (format s "~%")))))
    (setf (debug-session-last-x-address session) (+ address count))
    (if stream (progn (write-string body stream) nil) body)))

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
         (lines (disassemble-memory machine :memory (debug-session-memory session)
                                             :start pc :count context
                                             :symbol-info (and session-assembly
                                                                (assembly-symbol-info session-assembly))))
         (body (with-output-to-string (s)
                 (format s "pc = ~V,'0X~%" (debug-session-addr-digits session) pc)
                 (when session-assembly
                   (let ((line (listing-line-at session-assembly pc)))
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
  "TEXT as a breakpoint/continue-to target: an integer address if it parses
as one (%PARSE-INTEGER-MAYBE), otherwise the raw string as a label name."
  (or (%parse-integer-maybe text) text))

(defun %split-command (line)
  (let* ((line (string-trim " 	" line))
         (space (position #\Space line)))
    (if space
        (values (subseq line 0 space) (string-trim " " (subseq line (1+ space))))
        (values line ""))))

(defparameter *debug-help-text*
  "Commands:
  break ADDR|LABEL   set a breakpoint
  delete ID|ADDR     remove a breakpoint (by id first, falling back to address)
  info break         list breakpoints
  info reg           dump registers/flags/stacks
  info sym           list symbols (requires an attached assembly)
  step [N]           execute N instructions (default 1)
  continue           run until a breakpoint, trap, or decode failure
  until ADDR|LABEL   run until ADDR/LABEL is reached
  print NAME         print a register/flag's value
  x/N ADDR           dump N memory cells starting at ADDR
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
                       (let ((bp (debug-break session (%where-arg rest))))
                         (format nil "Breakpoint ~D at ~V,'0X~%" (breakpoint-id bp)
                                 (debug-session-addr-digits session) (breakpoint-address bp)))))
                  ((string-equal cmd "delete")
                   (if (zerop (length rest))
                       "delete: missing id or address"
                       (let ((target (%parse-integer-maybe rest)))
                         (if (and target (debug-unbreak session target))
                             (format nil "Deleted breakpoint at/id ~A~%" rest)
                             "delete: no such breakpoint"))))
                  ((string-equal cmd "info")
                   (cond
                     ((string-equal rest "break")
                      (let ((bps (debug-breakpoints session)))
                        (if bps
                            (with-output-to-string (s)
                              (dolist (bp bps)
                                (format s "~D: ~V,'0X~@[ (~A)~]~%" (breakpoint-id bp)
                                        (debug-session-addr-digits session) (breakpoint-address bp)
                                        (breakpoint-label bp))))
                            (format nil "No breakpoints.~%"))))
                     ((string-equal rest "reg") (debug-state-text session))
                     ((string-equal rest "sym")
                      (if (debug-session-assembly session)
                          (symbols-text (debug-session-assembly session))
                          (format nil "No assembly attached to this session.~%")))
                     (t (format nil "info: unknown subcommand ~S (try break/reg/sym)" rest))))
                  ((string-equal cmd "step")
                   (let ((n (or (%parse-integer-maybe rest) 1)))
                     (multiple-value-bind (reason steps) (debug-step session n)
                       (format nil "Stopped: ~(~A~)  steps=~D  pc=~V,'0X~%" reason steps
                               (debug-session-addr-digits session) (%pc session)))))
                  ((string-equal cmd "continue")
                   (multiple-value-bind (reason steps condition) (debug-continue session)
                     (format nil "Stopped: ~(~A~)  steps=~D  pc=~V,'0X~%~@[~A~%~]" reason steps
                             (debug-session-addr-digits session) (%pc session)
                             (and (eq reason :fault) condition))))
                  ((string-equal cmd "until")
                   (if (zerop (length rest))
                       "until: missing address or label"
                       (multiple-value-bind (reason steps condition)
                           (debug-continue-to session (%where-arg rest))
                         (format nil "Stopped: ~(~A~)  steps=~D  pc=~V,'0X~%~@[~A~%~]" reason steps
                                 (debug-session-addr-digits session) (%pc session)
                                 (and (eq reason :fault) condition)))))
                  ((string-equal cmd "print")
                   (if (zerop (length rest))
                       "print: missing name"
                       (let* ((machine (debug-session-machine session))
                              (descriptor (machine-descriptor machine))
                              (element (gethash (intern (string-upcase rest) :lasm)
                                                 (machine-descriptor-table descriptor))))
                         (cond
                           ((null element) (format nil "print: unknown storage element ~A" rest))
                           ((eq (storage-element-kind element) :flag)
                            (format nil "~A = ~D~%" rest (flag machine (storage-element-name element))))
                           ((eq (storage-element-kind element) :register)
                            (format nil "~A = ~A~%" rest (%register-value-text machine element)))
                           (t (format nil "print: ~A is not a register or flag" rest))))))
                  ((and (>= (length cmd) 2) (string-equal (subseq cmd 0 2) "x/"))
                   (let ((n (or (%parse-integer-maybe (subseq cmd 2)) 8))
                         (address (and (plusp (length rest)) (%parse-integer-maybe rest))))
                     (cond
                       ((zerop (length rest)) "x/N: missing address")
                       ((null address) (format nil "x/N: bad address ~S" rest))
                       (t (debug-memory-text session address n)))))
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
