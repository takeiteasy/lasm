;;;; semantics.lisp
;;;; The minimal semantics vocabulary: WITH-MACHINE, SET!, MREF, PUSH, POP,
;;;; SET-FLAGS!, TRAP, and the small predicate helpers the mockups use.
;;;;
;;;; WITH-MACHINE is the single semantics entry point: M1's DEFINSTRUCTION
;;;; expands its (semantics ...) body by wrapping it in WITH-MACHINE rather
;;;; than growing a parallel expander, so the vocabulary defined here is
;;;; shared by M0's standalone examples and M1's instruction semantics alike.

(in-package #:lasm)

(declaim (inline %semantics-mref (setf %semantics-mref)))

(defun %semantics-mref (machine name address)
  (mref machine name address))

(defun (setf %semantics-mref) (value machine name address)
  (setf (mref machine name address) value))

;; #307: inside semantics, SREF, REGREF, FLAG and the stack functions are local
;; macros (WITH-MACHINE-BINDINGS) that expand to the %PLAIN-* accessors, or to
;; the %CHECKED-* ones when the named element may be gated. The macros' own
;; expansions call %PLAIN-* so they are never captured again.
(declaim (inline %plain-sref (setf %plain-sref) %plain-regref (setf %plain-regref)
                 %plain-flag (setf %plain-flag) %plain-stack-push %plain-stack-pop
                 %plain-sp-push %plain-sp-pop))

(defun %plain-sref (machine name) (sref machine name))
(defun (setf %plain-sref) (value machine name) (setf (sref machine name) value))
(defun %plain-regref (machine name index) (regref machine name index))
(defun (setf %plain-regref) (value machine name index) (setf (regref machine name index) value))
(defun %plain-flag (machine name) (flag machine name))
(defun (setf %plain-flag) (value machine name) (setf (flag machine name) value))
(defun %plain-stack-push (machine name value) (stack-push machine name value))
(defun %plain-stack-pop (machine name) (stack-pop machine name))
(defun %plain-sp-push (machine reg memory grows value) (sp-push machine reg memory grows value))
(defun %plain-sp-pop (machine reg memory grows) (sp-pop machine reg memory grows))

(defun %check-element-gate (machine name accesses &optional kind)
  "Enforce NAME's read and write :PRIVILEGE for each of ACCESSES (:READ, :WRITE),
with KIND defaulting to the element's own."
  (let ((element (descriptor-element (machine-descriptor machine) name)))
    (dolist (access accesses)
      (let ((required (if (eq access :read)
                          (storage-element-read-privilege element)
                          (storage-element-write-privilege element))))
        (when required
          (%check-privilege machine required name nil (or kind (storage-element-kind element)) access))))))

;; TODO: the element is looked up on every call; a quoted, gated name could
;; resolve its level at macroexpansion time (#307 follow-up if it ever shows in a profile).
;; #314: VALUE after NAME's field gates; OLD is called only when it has any.
(defun %gate-fields (machine name value old)
  (let ((fields (storage-element-field-privileges (descriptor-element (machine-descriptor machine) name))))
    (if fields
        (%apply-field-gates machine name fields (funcall old) value)
        value)))

(defun %checked-sref (machine name)
  (%check-element-gate machine name '(:read))
  (sref machine name))

(defun (setf %checked-sref) (value machine name)
  (%check-element-gate machine name '(:write))
  (setf (sref machine name) (%gate-fields machine name value (lambda () (sref machine name)))))

(defun %checked-regref (machine name index)
  (%check-element-gate machine name '(:read))
  (regref machine name index))

(defun (setf %checked-regref) (value machine name index)
  (%check-element-gate machine name '(:write))
  (setf (regref machine name index)
        (%gate-fields machine name value (lambda () (regref machine name index)))))

(defun %checked-flag (machine name)
  (%check-element-gate machine name '(:read))
  (flag machine name))

(defun (setf %checked-flag) (value machine name)
  (%check-element-gate machine name '(:write))
  (setf (flag machine name) value))

(defun %checked-stack-push (machine name value)
  (%check-element-gate machine name '(:write) :stack)
  (stack-push machine name value))

(defun %checked-stack-pop (machine name)
  (%check-element-gate machine name '(:read :write) :stack)
  (stack-pop machine name))

(defun %checked-sp-push (machine reg memory grows value)
  (%check-element-gate machine reg '(:write) :stack)
  (sp-push machine reg memory grows value))

(defun %checked-sp-pop (machine reg memory grows)
  (%check-element-gate machine reg '(:read :write) :stack)
  (sp-pop machine reg memory grows))

(defun %accessor-form (plain checked gates machine name args)
  "The call a semantics accessor macro expands to: PLAIN for a quoted name
with no gate, otherwise CHECKED, which looks the gate up at run time."
  (let ((quoted (and (consp name) (eq (first name) 'quote) (symbolp (second name)) (second name))))
    `(,(if (and quoted (not (assoc quoted gates))) plain checked) ,machine ,name ,@args)))

(defun %gate-stack-form (machine-var gates target accesses form)
  "FORM, preceded by a privilege check per access in ACCESSES (:READ, :WRITE)
when TARGET (a stack or a stack-pointer register) is gated (#300)."
  (destructuring-bind (&optional read write &rest fields) (cdr (assoc target gates))
    (declare (ignore fields))
    (let ((checks (loop for access in accesses
                        for required = (if (eq access :read) read write)
                        when required
                          collect `(%check-privilege ,machine-var ',required ',target nil :stack ,access))))
      (if checks
          `(progn ,@checks ,form)
          form))))

(defmacro with-machine-bindings ((machine-var machine-name) &body body)
  "Evaluate BODY with every scalar storage/flag element of the machine
descriptor MACHINE-NAME bound as a symbol-macro, plus the semantics
operators SET!, MREF, PUSH, POP, STACK-POINTER, STACK-DEPTH, STACK-REF,
SET-BANK!, SET-FLAGS!, TRAP, ELAPSE, and
INTERRUPT-RETURN.

The device bus API (DEVICE-COUNT, DEVICE-INFO, DEVICE-SEND, device.lisp)
and SIGNAL-INTERRUPT (interrupt.lisp) are plain functions, not bound here:
semantics call them with the machine passed explicitly, e.g.
(device-info machine index), the way MREF takes its machine argument.
Callers of WITH-MACHINE pass whatever variable they named. INTERRUPT-RETURN
below *is* bound as a macrolet, unlike SIGNAL-INTERRUPT, purely so an
RFI-style instruction's semantics reads as one primitive (like TRAP) rather
than a hand-written reverse-order pop sequence -- everything it needs (the
machine's :SAVE list and resolved :STACK name) is already known from the
descriptor, the same way PUSH/POP's own SOLE-STACK is.

Unlike WITH-MACHINE, this does not create a machine instance -- MACHINE-VAR
must already be bound (by the caller) to a runtime MACHINE for descriptor
MACHINE-NAME. This is the piece DEFINSTRUCTION's (semantics ...) clause
expands into (see instruction.lisp), since instruction semantics run against
a machine instance the emulator already owns rather than a fresh one.

PUSH/POP's STACK-NAME argument is optional: when omitted, it resolves to the
machine's sole :stack element, mirroring emulator.lisp's %RESOLVE-MEMORY
convention for the sole :memory element; only when the machine declares no
:stack element does the sole (stack-pointer ...)-bound register (#166) become
the default instead. A machine declaring more than one candidate of whichever
kind applies (or none at all) signals an error at macroexpansion time, since
the descriptor is already known here. STACK-NAME, given or defaulted, may
name either kind -- PUSH/POP expand to STACK-PUSH/STACK-POP for a :stack
element or SP-PUSH/SP-POP for a stack-pointer register, transparently to the
caller.

STACK-POINTER, STACK-DEPTH, and STACK-REF operate on fixed :stack elements.
Their stack name defaults only when exactly one fixed stack is declared;
STACK-REF takes the offset before an optional bare stack name.

Storage elements with :count > 1 (banked registers, #13) are bound as a
local macro instead of a symbol-macro -- symbol-macrolet can't express an
indexed form like (V 3) with a run-time index. So (V idx) reads bank IDX of
V (expanding to REGREF), and (set! (V idx) val) writes it (through SET!'s
plain SETF expansion to (SETF (REGREF ...) VAL)). See the storage-element
:count docstring in storage.lisp.

A banked register's own :names (#72) additionally bind one ordinary
symbol-macro per alias, each with its bank index baked in -- e.g. DCPU-16's
I becomes (REGREF MACHINE-VAR 'REG 6), no run-time index needed, so
(set! I val) reaches (SETF (REGREF ...) VAL) exactly like any scalar
register's symbol-macro. The (V idx) macrolet stays available alongside
these for a run-time-computed index."
  (let ((descriptor (find-machine-descriptor machine-name)))
    (let (symbol-macros stack-names memory-names banked-names)
      (dolist (element (machine-descriptor-elements descriptor))
        (case (storage-element-kind element)
          (:register
           (let* ((name (storage-element-name element))
                 (read (storage-element-read-privilege element))
                 (write (storage-element-write-privilege element))
                 (fields (storage-element-field-privileges element))
                 (required (or read write fields)))
             (if (= (storage-element-count element) 1)
                 (cl:push (if required
                              `(,name (%gated-sref ,machine-var ',name ',read ',write ',fields))
                              `(,name (%plain-sref ,machine-var ',name)))
                          symbol-macros)
                 (progn
                   (cl:push name banked-names)
                   (loop for alias in (storage-element-names element)
                         for index from 0
                         do (cl:push (if required
                                         `(,alias (%gated-regref ,machine-var ',name ,index ',read ',write ',fields))
                                         `(,alias (%plain-regref ,machine-var ',name ,index)))
                                     symbol-macros))))))
          (:flag
           (let ((name (storage-element-name element))
                 (read (storage-element-read-privilege element))
                 (write (storage-element-write-privilege element)))
             (cl:push (if (or read write)
                          `(,name (%gated-flag ,machine-var ',name ',read ',write))
                          `(,name (%plain-flag ,machine-var ',name)))
                      symbol-macros)))
          (:stack (cl:push (storage-element-name element) stack-names))
          (:memory (cl:push (storage-element-name element) memory-names))))
      (setf stack-names (nreverse stack-names))
      ;; #166: PUSH/POP also accept a register bound by a (stack-pointer ...)
      ;; clause -- POINTER-ALIST is (register memory grows), embedded as
      ;; literal data below (ASSOC) so each call-site's macroexpansion can
      ;; tell a :stack target from a :pointer one without a runtime lookup.
      ;; A bare PUSH/POP's default target keeps the sole :stack element as
      ;; the default whenever one is declared -- unchanged behavior for
      ;; every existing machine -- and only falls back to the sole
      ;; stack-pointer when the machine declares no :stack element at all.
      (let* ((sole-memory (and (= (length memory-names) 1) (first memory-names)))
             (memory-error (if memory-names
                               (format nil "MREF on machine ~S: more than one memory element declared -- name one explicitly"
                                       machine-name)
                               (format nil "MREF on machine ~S: no memory element declared"
                                       machine-name)))
             (gates (loop for element in (machine-descriptor-elements descriptor)
                          when (or (storage-element-read-privilege element)
                                   (storage-element-write-privilege element)
                                   (storage-element-field-privileges element))
                            collect (list (storage-element-name element)
                                          (storage-element-read-privilege element)
                                          (storage-element-write-privilege element)
                                          (storage-element-field-privileges element))))
             (pointer-alist (loop for sp being the hash-values of (machine-descriptor-stack-pointers descriptor)
                                   collect (list (stack-pointer-descriptor-register sp)
                                                 (stack-pointer-descriptor-memory sp)
                                                 (stack-pointer-descriptor-grows sp))))
             (pointer-names (mapcar #'first pointer-alist))
             (bank-names (mapcar (lambda (entry) (memory-region-name (cdr entry)))
                                 (%banked-regions descriptor)))
             (sole-stack (cond
                           ((= (length stack-names) 1) (first stack-names))
                           ((and (null stack-names) (= (length pointer-names) 1)) (first pointer-names))))
             (sole-fixed-stack (and (= (length stack-names) 1) (first stack-names)))
             (fixed-stack-error
               (if stack-names
                   (format nil "More than one stack element on machine ~S -- name one explicitly"
                           machine-name)
                   (format nil "No stack element on machine ~S" machine-name)))
             (stack-error (cond
                            ((and (null stack-names) (null pointer-names))
                             (format nil "PUSH/POP on machine ~S: no stack element or ~
stack-pointer declared" machine-name))
                            ((> (length stack-names) 1)
                             (format nil "PUSH/POP on machine ~S: more than one stack element ~
declared (~{~S~^ ~}) -- name one explicitly" machine-name stack-names))
                            ((and (null stack-names) (> (length pointer-names) 1))
                             (format nil "PUSH/POP on machine ~S: more than one stack-pointer ~
declared (~{~S~^ ~}) -- name one explicitly" machine-name pointer-names))))
             ;; #109: everything INTERRUPT-RETURN needs -- which places to
             ;; pop, in what order, off which stack -- is already resolved
             ;; on the descriptor (%FINISH-INTERRUPT-MODEL, machine.lisp),
             ;; unlike PUSH/POP's SOLE-STACK, which is only a *default*
             ;; still overridable per call. No per-call argument means no
             ;; need for PUSH/POP's deferred-to-expansion-time style --
             ;; INTERRUPT-FORM below is built directly, now.
             (interrupts (machine-descriptor-interrupts descriptor))
             (interrupt-error (unless interrupts
                                 (format nil "INTERRUPT-RETURN on machine ~S: no (interrupts ...) ~
clause declared" machine-name)))
             ;; #166: a :POINTER interrupt stack pops through SP-POP instead
             ;; of STACK-POP -- its memory/direction come from the bound
             ;; register's own (stack-pointer ...) clause, resolved once here.
             (interrupt-pop-form
               (when interrupts
                 (if (eq (interrupt-descriptor-stack-kind interrupts) :pointer)
                     (let ((sp (gethash (interrupt-descriptor-stack-name interrupts)
                                         (machine-descriptor-stack-pointers descriptor))))
                       `(%plain-sp-pop ,machine-var ',(interrupt-descriptor-stack-name interrupts)
                                ',(stack-pointer-descriptor-memory sp) ',(stack-pointer-descriptor-grows sp)))
                     `(%plain-stack-pop ,machine-var ',(interrupt-descriptor-stack-name interrupts)))))
             ;; #301: the privilege level is restored last, so the other pops
             ;; still run at the handler's level, and every restore goes
             ;; through %INTERRUPT-PLACE so it bypasses register gates (#300).
             (interrupt-form
               (when interrupts
                 (let* ((privilege (machine-descriptor-privilege descriptor))
                        (level (and privilege (privilege-descriptor-level privilege)))
                        (deferred (gensym "LEVEL")))
                   `(let ((,deferred nil))
                      (%interrupt-returned ,machine-var)
                      ,@(mapcar
                         (lambda (place)
                           (if (eq place level)
                               `(setf ,deferred (list ,interrupt-pop-form))
                               `(setf (%interrupt-place ,machine-var ',place) ,interrupt-pop-form)))
                         (reverse (interrupt-descriptor-save interrupts)))
                      ,@(when (member level (interrupt-descriptor-save interrupts))
                          `((setf (%interrupt-place ,machine-var ',level) (first ,deferred)))))))))
        `(symbol-macrolet ,(nreverse symbol-macros)
           (macrolet (,@(mapcar (lambda (name)
                                   (let ((required (cdr (assoc name gates))))
                                     (if required
                                         `(,name (index)
                                                 `(%gated-regref ,',machine-var ',',name ,index
                                                                 ',',(first required) ',',(second required)
                                                                 ',',(third required)))
                                         `(,name (index) `(%plain-regref ,',machine-var ',',name ,index)))))
                                 (nreverse banked-names))
                      ,@(mapcar (lambda (entry)
                                  (destructuring-bind (name plain checked) entry
                                    `(,name (machine name &rest args)
                                            (%accessor-form ',plain ',checked ',gates machine name args))))
                                '((sref %plain-sref %checked-sref)
                                  (regref %plain-regref %checked-regref)
                                  (flag %plain-flag %checked-flag)
                                  (stack-push %plain-stack-push %checked-stack-push)
                                  (stack-pop %plain-stack-pop %checked-stack-pop)
                                  (sp-push %plain-sp-push %checked-sp-push)
                                  (sp-pop %plain-sp-pop %checked-sp-pop)))
                      (set! (place value)
                        `(setf ,place ,value))
                      (mref (machine name-or-address &optional (address nil supplied-p))
                        (if supplied-p
                            `(%semantics-mref ,machine ,name-or-address ,address)
                            (let ((target ',sole-memory))
                              (unless target (%definstruction-error ',memory-error))
                              `(%semantics-mref ,machine ',target ,name-or-address))))
                      (push (value &optional (stack-name nil supplied-p))
                        (let* ((target (if supplied-p stack-name ',sole-stack))
                               (entry (assoc target ',pointer-alist)))
                          (unless target (%definstruction-error ',stack-error))
                          (%gate-stack-form
                           ',machine-var ',gates target '(:write)
                           (if entry
                               `(%plain-sp-push ,',machine-var ',target ',(second entry) ',(third entry) ,value)
                               `(%plain-stack-push ,',machine-var ',target ,value)))))
                      (pop (&optional (stack-name nil supplied-p))
                        (let* ((target (if supplied-p stack-name ',sole-stack))
                               (entry (assoc target ',pointer-alist)))
                          (unless target (%definstruction-error ',stack-error))
                          (%gate-stack-form
                           ',machine-var ',gates target '(:read :write)
                           (if entry
                               `(%plain-sp-pop ,',machine-var ',target ',(second entry) ',(third entry))
                               `(%plain-stack-pop ,',machine-var ',target)))))
                      (stack-pointer (&optional (stack-name nil supplied-p))
                        (let ((target (if supplied-p stack-name ',sole-fixed-stack)))
                          (unless target (%definstruction-error ',fixed-stack-error))
                          (%gate-stack-form ',machine-var ',gates target '(:read)
                                            `(%stack-pointer ,',machine-var ',target))))
                      (stack-depth (&optional (stack-name nil supplied-p))
                        (let ((target (if supplied-p stack-name ',sole-fixed-stack)))
                          (unless target (%definstruction-error ',fixed-stack-error))
                          (%gate-stack-form ',machine-var ',gates target '(:read)
                                            `(%stack-pointer ,',machine-var ',target))))
                      (stack-ref (offset &optional (stack-name nil supplied-p))
                        (let ((target (if supplied-p stack-name ',sole-fixed-stack)))
                          (unless target (%definstruction-error ',fixed-stack-error))
                          (%gate-stack-form ',machine-var ',gates target '(:read)
                                            `(%stack-ref ,',machine-var ',target ,offset))))
                      (set-bank! (region bank)
                        (unless (member region ',bank-names)
                          (%definstruction-error "SET-BANK! on machine ~S: ~S is not a banked region"
                                 ',machine-name region))
                        `(setf (current-bank ,',machine-var ',region) ,bank))
                      (set-flags! (&rest assignments)
                        `(progn ,@(mapcar (lambda (a)
                                             (let ((required (cdr (assoc (first a) ',gates))))
                                               (if required
                                                   `(setf (%gated-flag ,',machine-var ',(first a)
                                                                       ',(first required) ',(second required))
                                                          ,(second a))
                                                   `(setf (%plain-flag ,',machine-var ',(first a)) ,(second a)))))
                                           assignments)))
                      (trap (tag &optional data)
                        `(error 'lasm-trap :tag ,tag :data ,data))
                      ;; #110: unlike TRAP, IDLE does not unwind -- it just
                      ;; sets a flag STEP-MACHINE (emulator.lisp) checks
                      ;; next step, so the rest of this semantics body (and
                      ;; the instruction's own cycle cost) still runs to
                      ;; completion.
                      (idle ()
                        `(setf (machine-idle ,',machine-var) t))
                      ;; #159: counts and ticks devices now, so a trapping
                      ;; step's devices stay in lockstep with MACHINE-CYCLES (#178).
                      (elapse (n)
                        `(%elapse ,',machine-var ,n))
                      (interrupt-return ()
                        (when ',interrupt-error (%definstruction-error ',interrupt-error))
                        ',interrupt-form))
             ,@body))))))

(defmacro with-machine ((machine-var machine-name) &body body)
  "Evaluate BODY with a fresh runtime MACHINE instance for MACHINE-NAME bound
to MACHINE-VAR, plus every scalar storage/flag element and the semantics
vocabulary from WITH-MACHINE-BINDINGS."
  `(let ((,machine-var (make-machine ',machine-name)))
     (with-machine-bindings (,machine-var ,machine-name)
       ,@body)))

(defun zero? (value) (zerop value))

(defun bit-set? (value bit) (logbitp bit value))

(defun page-crossed? (from to &optional (page-size 256))
  "True when FROM and TO lie in different PAGE-SIZE-cell pages."
  (/= (floor from page-size) (floor to page-size)))
