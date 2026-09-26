;;;; machine.lisp
;;;; The DEFMACHINE macro: parses storage clauses into a machine-descriptor.

(in-package #:lasm)

(defun %check-positive (value what name)
  (unless (and (integerp value) (plusp value))
    (%defmachine-error "~A for ~S must be a positive integer, got ~S" what name value))
  value)

(defun %endian-valid-p (value)
  (or (member value '(:little :big))
      (and (consp value) (= (length value) 3)
           (member (first value) '(:little :big))
           (member (second value) '(:little :big))
           (integerp (third value)) (>= (third value) 2))))

(defun %check-endian (value name)
  "#66: VALUE must be :LITTLE, :BIG, or (OUTER INNER GROUP) -- OUTER and INNER
each :LITTLE or :BIG, GROUP an integer of at least 2 (see
%CELL-SIGNIFICANCE-ORDER, instruction.lisp)."
  (unless (%endian-valid-p value)
    (%defmachine-error "~S :endian must be :LITTLE, :BIG or (OUTER INNER GROUP), got ~S" name value))
  value)

(defun %endian-byte-order (endian)
  "The :LITTLE or :BIG order of the bytes inside one cell under ENDIAN."
  (if (consp endian) (second endian) endian))

;; #300, #303: the :privilege key of a register, stack, flag or region. SPEC is a
;; level name gating every access in ACCESSES, or a plist (:read L :write L
;; :execute L) gating only the listed ones. Returns one level (or NIL) per
;; access in ACCESSES; the levels themselves are checked against the machine's
;; (privilege ...) clause in %FINISH-PRIVILEGE-MODEL.
(defun %parse-privilege-spec (spec kind name accesses)
  (flet ((level-p (level) (and (symbolp level) level (not (keywordp level)))))
    (cond
      ((null spec) (mapcar (constantly nil) accesses))
      ((level-p spec) (mapcar (constantly spec) accesses))
      ((and (consp spec) (listp (cdr (last spec))) (evenp (length spec)))
       (let ((keys (loop for (key) on spec by #'cddr collect key)))
         (dolist (key keys)
           (unless (member key accesses)
             (%defmachine-error "~(~A~) ~S: :privilege key must be one of ~{~S~^, ~}, got ~S"
                    kind name accesses key)))
         (unless (= (length keys) (length (remove-duplicates keys)))
           (%defmachine-error "~(~A~) ~S: :privilege lists a key more than once: ~S" kind name spec))
         (loop for access in accesses
               collect (let ((level (getf spec access)))
                         (unless (or (null level) (level-p level))
                           (%defmachine-error "~(~A~) ~S: :privilege ~S must be a level name, got ~S"
                                  kind name access level))
                         level))))
      (t (%defmachine-error "~(~A~) ~S: :privilege must be a level name or a plist of ~{~S~^, ~} levels, got ~S"
                kind name accesses spec)))))

;; #314: (:fields ((MASK LEVEL [:on-write :violate/:ignore])...)) inside a
;; register's :privilege plist. Returns the plist without :FIELDS and the
;; parsed (MASK LEVEL POLICY) list; %FINISH-PRIVILEGE-MODEL checks the rest.
(defun %split-field-privileges (spec name)
  (if (and (consp spec) (listp (cdr (last spec))) (evenp (length spec)) (member :fields spec))
      (let ((fields (getf spec :fields))
            (rest (loop for (key value) on spec by #'cddr unless (eq key :fields) append (list key value))))
        (unless (and (listp fields) fields)
          (%defmachine-error "register ~S: :fields must be a non-empty list of (MASK LEVEL [:on-write POLICY]), got ~S"
                 name fields))
        (values rest
                (loop for entry in fields
                      collect (destructuring-bind (&optional mask level &rest options) (if (listp entry) entry (list entry))
                                (unless (and (integerp mask) (plusp mask))
                                  (%defmachine-error "register ~S: :fields mask must be a positive integer, got ~S" name mask))
                                (unless (and level (symbolp level) (not (keywordp level)))
                                  (%defmachine-error "register ~S: :fields level must be a level name, got ~S" name level))
                                (unless (and (evenp (length options)) (subsetp (loop for (key) on options by #'cddr collect key) '(:on-write)))
                                  (%defmachine-error "register ~S: :fields options must be :on-write, got ~S" name options))
                                (let ((policy (getf options :on-write :violate)))
                                  (unless (member policy '(:violate :ignore))
                                    (%defmachine-error "register ~S: :fields :on-write must be :violate or :ignore, got ~S"
                                           name policy))
                                  (list mask level policy))))))
      (values spec nil)))

(defun parse-register-clause (name-form)
  ;; (register NAME :width n [:count n] [:names (A B C ...)]) -- #72: NAMES is
  ;; an optional list of alias symbols, one per bank cell in index order
  ;; (CHIP8's V0-VF, DCPU-16's A/B/C/X/Y/Z/I/J). COUNT defaults to (length
  ;; NAMES) when NAMES is given and COUNT is not; when both are given they
  ;; must agree, since a mismatched pair almost certainly indicates a typo
  ;; in one or the other rather than an intentional partial naming.
  (%definition-bind (name &key width count names privilege) name-form
    (unless width (%defmachine-error "register ~S requires :width" name))
    (when names
      (unless (every #'symbolp names)
        (%defmachine-error "register ~S :names must be a list of symbols, got ~S" name names))
      (let ((dup (loop for (n . rest) on names
                        when (member n rest :test #'string-equal) return n)))
        (when dup
          (%defmachine-error "register ~S :names: duplicate alias ~S" name dup)))
      (if count
          (unless (= count (length names))
            (%defmachine-error "register ~S: :count ~D disagrees with :names' length ~D"
                   name count (length names)))
          (setf count (length names))))
    (setf count (or count 1))
    (multiple-value-bind (privilege field-privileges) (%split-field-privileges privilege name)
     (destructuring-bind (read-privilege write-privilege)
        (%parse-privilege-spec privilege 'register name '(:read :write))
      (make-storage-element :name name :kind :register
                             :field-privileges field-privileges
                             :width (%check-positive width ":width" name)
                             :count (%check-positive count ":count" name)
                             :names names
                             :read-privilege read-privilege
                             :write-privilege write-privilege)))))

(defun parse-stack-clause (form)
  ;; (stack NAME :width n :depth n)
  (%definition-bind (name &key width depth privilege) form
    (unless width (%defmachine-error "stack ~S requires :width" name))
    (unless depth (%defmachine-error "stack ~S requires :depth" name))
    (destructuring-bind (read-privilege write-privilege)
        (%parse-privilege-spec privilege 'stack name '(:read :write))
      (make-storage-element :name name :kind :stack
                             :width (%check-positive width ":width" name)
                             :depth (%check-positive depth ":depth" name)
                             :read-privilege read-privilege
                             :write-privilege write-privilege))))

;; #166: (stack-pointer REGISTER [:memory NAME] [:grows :down/:up] [:width N]
;; [:bounds (LOW HIGH)]) -- binds
;; an existing scalar :register element as an address pointer into a :memory
;; element, for machines whose "stack" is a plain register indexed by
;; push/pop convention rather than a lasm :stack element (DCPU-16, ANIMA-16).
;; Like (interrupts ...), this clause introduces no new namespace name -- it
;; only references existing ones -- so it's parsed shape-only here; whether
;; REGISTER/MEMORY actually name the right kind of element can't be checked
;; until every other clause is known (%FINISH-STACK-POINTERS, below).
(defun parse-stack-pointer-clause (form)
  (%definition-bind (register &key memory (grows :down) width bounds) form
    (unless (symbolp register)
      (%defmachine-error "stack-pointer ~S must be a symbol" register))
    (when (and memory (not (symbolp memory)))
      (%defmachine-error "stack-pointer ~S :memory must be a symbol, got ~S" register memory))
    (unless (member grows '(:down :up))
      (%defmachine-error "stack-pointer ~S :grows must be :DOWN or :UP, got ~S" register grows))
    (when width
      (%check-positive width ":width" register))
    (when (and bounds (not (and (consp bounds) (= (length bounds) 2) (every #'integerp bounds)
                                (<= 0 (first bounds) (second bounds)))))
      (%defmachine-error "stack-pointer ~S :bounds must be (LOW HIGH) with 0 <= LOW <= HIGH, got ~S"
                         register bounds))
    (make-stack-pointer-descriptor :register register :memory memory :grows grows
                                   :width width :bounds bounds)))

;; #166: resolves every (stack-pointer ...) clause's REGISTER/MEMORY against
;; DESCRIPTOR's own ELEMENTS, once they're fully known -- same two-pass split
;; as %FINISH-INTERRUPT-MODEL. Populates MACHINE-DESCRIPTOR-STACK-POINTERS
;; (register name -> STACK-POINTER-DESCRIPTOR, with MEMORY resolved), and
;; mutates each descriptor's MEMORY slot in place when the clause omitted it.
(defun %finish-stack-pointers (descriptor stack-pointers)
  (let ((name (machine-descriptor-name descriptor)))
    (dolist (sp stack-pointers)
      (let* ((reg (stack-pointer-descriptor-register sp))
             (element (gethash reg (machine-descriptor-table descriptor))))
        (unless (and element (eq (storage-element-kind element) :register))
          (%defmachine-error "stack-pointer on machine ~S: ~S is not a declared register element"
                 name reg))
        (when (> (storage-element-count element) 1)
          (%defmachine-error "stack-pointer on machine ~S: ~S is a banked (:count > 1) register -- ~
only a scalar register may be a stack pointer" name reg))
        (when (gethash reg (machine-descriptor-stack-pointers descriptor))
          (%defmachine-error "stack-pointer on machine ~S: more than one (stack-pointer ~S ...) clause"
                 name reg))
        (let ((given (stack-pointer-descriptor-memory sp)))
          (if given
              (let ((mem (gethash given (machine-descriptor-table descriptor))))
                (unless (and mem (eq (storage-element-kind mem) :memory))
                  (%defmachine-error "stack-pointer ~S on machine ~S: :memory ~S is not a declared ~
memory element" reg name given)))
              (let ((memories (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                                              (machine-descriptor-elements descriptor))))
                (cond
                  ((= (length memories) 1)
                   (setf (stack-pointer-descriptor-memory sp) (storage-element-name (first memories))))
                  ((null memories)
                   (%defmachine-error "stack-pointer ~S on machine ~S: no memory element is declared -- ~
name one explicitly with :memory" reg name))
                  (t (%defmachine-error "stack-pointer ~S on machine ~S: more than one memory element ~
declared (~{~S~^ ~}) -- name one explicitly with :memory"
                            reg name (mapcar #'storage-element-name memories)))))))
        (let ((memory (gethash (stack-pointer-descriptor-memory sp) (machine-descriptor-table descriptor)))
              (bounds (stack-pointer-descriptor-bounds sp)))
          (unless (stack-pointer-descriptor-width sp)
            (setf (stack-pointer-descriptor-width sp) (storage-element-cell-width memory)))
          (when (and bounds (>= (second bounds) (ash 1 (storage-element-addr-width memory))))
            (%defmachine-error "stack-pointer ~S on machine ~S: :bounds ~S exceeds memory ~S's ~D-bit address space"
                               reg name bounds (storage-element-name memory) (storage-element-addr-width memory))))
        (setf (gethash reg (machine-descriptor-stack-pointers descriptor)) sp)))))

;; #107: (region NAME start end [:kind :ram/:rom/:device] [:banks n] [:on-write
;; :ignore/:error] [:read fn] [:write fn] [:device NAME]) -- one sub-range of a memory
;; element with distinct access behavior. NAME is validated as a symbol
;; here; PARSE-MEMORY-CLAUSE cross-checks it against every other name in the
;; machine's namespace (BUILD-MACHINE-DESCRIPTOR's SEEN table) once the whole
;; clause is parsed, same as a register's #72 :NAMES aliases. START/END are
;; both inclusive; validated against ADDR-WIDTH by PARSE-MEMORY-CLAUSE, which
;; alone knows the element's address range.
(defun %parse-memory-region-form (form context)
  (%definition-bind (head name start end &key (kind :ram) banks (on-write :ignore) read write device privilege) form
    (unless (eq head 'region)
      (%defmachine-error "~A: expected (region name start end ...), got ~S" context form))
    (unless (symbolp name)
      (%defmachine-error "~A: region name must be a symbol, got ~S" context name))
    (unless (and (integerp start) (>= start 0))
      (%defmachine-error "~A region ~S: start must be a non-negative integer, got ~S" context name start))
    (unless (and (integerp end) (>= end 0))
      (%defmachine-error "~A region ~S: end must be a non-negative integer, got ~S" context name end))
    (unless (<= start end)
      (%defmachine-error "~A region ~S: start ~D must not be greater than end ~D" context name start end))
    (unless (member kind '(:ram :rom :device))
      (%defmachine-error "~A region ~S: :kind must be :RAM, :ROM or :DEVICE, got ~S" context name kind))
    (when banks
      (unless (and (integerp banks) (plusp banks))
        (%defmachine-error "~A region ~S: :banks must be a positive integer, got ~S" context name banks))
      (when (eq kind :device)
        (%defmachine-error "~A region ~S: :banks does not apply to a :DEVICE region" context name)))
    (unless (member on-write '(:ignore :error))
      (%defmachine-error "~A region ~S: :on-write must be :IGNORE or :ERROR, got ~S" context name on-write))
    (unless (or (eq on-write :ignore) (eq kind :rom))
      (%defmachine-error "~A region ~S: :on-write only applies to a :ROM region" context name))
    (when (and (or read write) (not (eq kind :device)))
      (%defmachine-error "~A region ~S: :read/:write only apply to a :DEVICE region" context name))
    (dolist (fn (list (cons :read read) (cons :write write)))
      (when (and (cdr fn) (not (or (symbolp (cdr fn)) (functionp (cdr fn)))))
        (%defmachine-error "~A region ~S: ~A must be a function designator (a symbol or a ~
function), got ~S" context name (car fn) (cdr fn))))
    (when device
      (unless (symbolp device)
        (%defmachine-error "~A region ~S: :device must be a device name, got ~S" context name device))
      (unless (eq kind :device)
        (%defmachine-error "~A region ~S: :device only applies to a :DEVICE region" context name))
      (when (or read write)
        (%defmachine-error "~A region ~S: :device cannot be combined with :read/:write" context name)))
    (destructuring-bind (read-privilege write-privilege execute-privilege)
        (%parse-privilege-spec privilege (format nil "~A region" context) name '(:read :write :execute))
      (make-memory-region :name name :start start :end end :kind kind :banks banks
                           :on-write on-write :read read :write write :device device
                           :read-privilege read-privilege :write-privilege write-privilege
                           :execute-privilege execute-privilege))))

;; Cross-region checks (#107): unique names and non-overlapping ranges,
;; applied once every (region ...) form in the clause is parsed -- mirrors
;; PARSE-INSTRUCTION-WORD-CLAUSE's own cross-layout checks after parsing
;; every (layout ...) form.
(defun %check-memory-regions (regions context)
  (loop for (region . rest) on regions
        do (when (member (memory-region-name region) rest :key #'memory-region-name)
             (%defmachine-error "~A: duplicate region name ~S" context (memory-region-name region)))
           (dolist (other rest)
             (when (and (<= (memory-region-start region) (memory-region-end other))
                        (<= (memory-region-start other) (memory-region-end region)))
               (%defmachine-error "~A: region ~S (~D-~D) overlaps region ~S (~D-~D)"
                      context (memory-region-name region) (memory-region-start region)
                      (memory-region-end region) (memory-region-name other)
                      (memory-region-start other) (memory-region-end other)))))
  regions)

(defun parse-memory-clause (form)
  ;; (memory NAME :width n :addr-width n [:cell-width n] [:endian :little/:big]
  ;;   (region NAME start end ...)...)
  ;; #66: ENDIAN defaults to :LITTLE, matching every machine before this
  ;; ticket -- see %ENCODE-VALUE-CELLS/%FETCH-CELLS for what it governs.
  ;; #107: nested (region ...) forms are split out before DESTRUCTURING-BIND
  ;; sees the rest as a plain plist, same shape as PARSE-INSTRUCTION-WORD-
  ;; CLAUSE splitting out (layout ...) forms.
  (let* ((region-forms (remove-if-not (lambda (f) (and (consp f) (eq (first f) 'region))) form))
         (plist-forms (remove-if (lambda (f) (and (consp f) (eq (first f) 'region))) form)))
    (%definition-bind (name &key width addr-width cell-width (endian :little)) plist-forms
      (unless width (%defmachine-error "memory ~S requires :width" name))
      (unless addr-width (%defmachine-error "memory ~S requires :addr-width" name))
      (%check-positive width ":width" name)
      (%check-positive addr-width ":addr-width" name)
      (let* ((context (format nil "memory ~S" name))
             (max-address (1- (ash 1 addr-width)))
             (regions (%check-memory-regions
                       (mapcar (lambda (f) (%parse-memory-region-form f context)) region-forms)
                       context)))
        (dolist (r regions)
          (when (> (memory-region-end r) max-address)
            (%defmachine-error "~A region ~S: end ~D is outside the element's address range 0-~D"
                   context (memory-region-name r) (memory-region-end r) max-address)))
        (make-storage-element :name name :kind :memory
                               :width width
                               :addr-width addr-width
                               :cell-width (%check-positive (or cell-width width) ":cell-width" name)
                               :endian (%check-endian endian name)
                               :regions regions
                               :region-index (%sorted-region-index regions))))))

(defun parse-flags-clause (form)
  ;; (flags A B C ...) -- expands to one storage-element per flag, width 1.
  ;; #300: an entry may be (NAME :privilege LEVEL).
  (loop for entry in form
        collect (if (consp entry)
                    (%definition-bind (name &key privilege) entry
                      (destructuring-bind (read-privilege write-privilege)
                          (%parse-privilege-spec privilege 'flag name '(:read :write))
                        (make-storage-element :name name :kind :flag :width 1
                                              :read-privilege read-privilege
                                              :write-privilege write-privilege)))
                    (make-storage-element :name entry :kind :flag :width 1))))

;; #75: (clock-speed n) -- the machine's nominal rate in Hz, n a positive
;; integer. Optional; a machine with no such clause leaves MACHINE-
;; DESCRIPTOR-CLOCK-SPEED NIL (storage.lisp), which is what keeps
;; RUN-FOR-DURATION's wall-time-equivalent conversion opt-in rather than
;; forcing every machine to declare a rate it doesn't care about.
(defun parse-clock-speed-clause (form)
  ;; (clock-speed n)
  (%definition-bind (hz) form
    (%check-positive hz ":clock-speed" 'clock-speed)))

;; #226: (reset-pc n) -- the value RESET (and MAKE-MACHINE) gives the PC
;; register; n a non-negative integer that BUILD-MACHINE-DESCRIPTOR checks
;; against PC's width once the elements are known.
(defun parse-reset-pc-clause (form)
  (%definition-bind (pc) form
    (unless (and (integerp pc) (>= pc 0))
      (%defmachine-error "reset-pc must be a non-negative integer, got ~S" pc))
    pc))

;; #108: (device NAME [:id n] [:version n] [:manufacturer n] [:init fn]
;;   [:tick fn] [:receive fn] [:detach fn] [:save fn] [:load fn]
;;   [:read fn] [:write fn]) -- a bus-addressed peripheral. A #107 :DEVICE
;; region may bind to it with :DEVICE (#158) to route MREF through :READ/
;; :WRITE; a device no region binds is bus-only. NAME is validated as a symbol here;
;; BUILD-MACHINE-DESCRIPTOR cross-checks it against every other name in the
;; machine's namespace, same as a region's or a register alias's name.
(defun parse-device-clause (form)
  (%definition-bind (name &key (id 0) (version 0) (manufacturer 0)
                             init tick receive detach save load read write (priority 0) non-maskable)
      form
    (unless (symbolp name)
      (%defmachine-error "device ~S: name must be a symbol" name))
    (dolist (v (list (cons :id id) (cons :version version) (cons :manufacturer manufacturer)))
      (unless (and (integerp (cdr v)) (>= (cdr v) 0))
        (%defmachine-error "device ~S: ~A must be a non-negative integer, got ~S" name (car v) (cdr v))))
    (unless (integerp priority)
      (%defmachine-error "device ~S: :priority must be an integer, got ~S" name priority))
    (dolist (fn (list (cons :init init) (cons :tick tick)
                       (cons :receive receive) (cons :detach detach)
                       (cons :save save) (cons :load load)
                       (cons :read read) (cons :write write)))
      (when (and (cdr fn) (not (or (symbolp (cdr fn)) (functionp (cdr fn)))))
        (%defmachine-error "device ~S: ~A must be a function designator (a symbol or a ~
function), got ~S" name (car fn) (cdr fn))))
    (unless (typep non-maskable 'boolean)
      (%defmachine-error "device ~S: :non-maskable must be T or NIL, got ~S" name non-maskable))
    (make-device-descriptor :name name :id id :version version :manufacturer manufacturer
                             :init init :tick tick :receive receive :detach detach
                             :save save :load load :read read :write write
                             :priority priority :non-maskable non-maskable)))

;; #109: (interrupts :vector NAME :message NAME :save (NAME...)
;;   [:nmi-vector NAME] [:stack NAME] [:queue n] [:on-overflow policy] [:mask-when fn]
;;   [:mask-flag name] [:mask-level place] [:mask-level-when fn]
;;   [:mask-level-on-deliver t/nil] [:cycles n] [:drop-on-zero-vector t/nil]
;;   [:mask-on-deliver t/nil] [:nesting :allow/:priority] [:max-depth n]
;;   [:deliver-level level]) -- the
;; machine's whole interrupt-delivery model. VECTOR/MESSAGE/SAVE/STACK/
;; MASK-FLAG are validated as symbols here only -- whether each actually
;; names a real storage element of the right kind can't be checked until
;; every (register ...)/(stack ...)/(flags ...) clause is known, so that
;; part is %FINISH-INTERRUPT-MODEL's job (below), called from BUILD-
;; MACHINE-DESCRIPTOR once ELEMENTS is built, mirroring %FINISH-
;; INSTRUCTION-WORD-LAYOUT's own two-pass split. Unlike DEVICE, this clause
;; introduces no new namespace names -- it only references existing ones --
;; so BUILD-MACHINE-DESCRIPTOR's SEEN table never needs to know about it.
(defun %interrupt-place-designator-p (place)
  "A scalar element name, or (NAME INDEX) naming one cell of a banked
register (#163)."
  (or (symbolp place)
      (and (consp place) (= (length place) 2)
           (symbolp (first place)) (integerp (second place)))))

(defun parse-interrupts-clause (form)
  (%definition-bind (&key vector nmi-vector message save stack (queue 256) (on-overflow :error)
                             mask-when mask-flag mask-level mask-level-when mask-level-on-deliver
                             (cycles 0) (drop-on-zero-vector t) mask-on-deliver (nesting :allow) max-depth deliver-level)
      form
    (unless vector (%defmachine-error "interrupts requires :vector"))
    (unless (%interrupt-place-designator-p vector)
      (%defmachine-error "interrupts :vector must be a symbol or (NAME INDEX), got ~S" vector))
    (when (and nmi-vector (not (%interrupt-place-designator-p nmi-vector)))
      (%defmachine-error "interrupts :nmi-vector must be a symbol or (NAME INDEX), got ~S" nmi-vector))
    (unless message (%defmachine-error "interrupts requires :message"))
    (unless (%interrupt-place-designator-p message)
      (%defmachine-error "interrupts :message must be a symbol or (NAME INDEX), got ~S" message))
    (unless save (%defmachine-error "interrupts requires :save"))
    (unless (and (listp save) (every #'%interrupt-place-designator-p save))
      (%defmachine-error "interrupts :save must be a list of symbols or (NAME INDEX) places, got ~S" save))
    (when (and stack (not (symbolp stack)))
      (%defmachine-error "interrupts :stack must be a symbol, got ~S" stack))
    (unless (and (integerp queue) (plusp queue))
      (%defmachine-error "interrupts :queue must be a positive integer, got ~S" queue))
    (unless (member on-overflow '(:error :trap :drop :drop-oldest))
      (%defmachine-error "interrupts :on-overflow must be :ERROR, :TRAP, :DROP or :DROP-OLDEST, got ~S"
             on-overflow))
    (when (and mask-when mask-flag)
      (%defmachine-error "interrupts: at most one of :mask-when/:mask-flag may be given"))
    (when (and mask-when (not (or (symbolp mask-when) (functionp mask-when))))
      (%defmachine-error "interrupts :mask-when must be a function designator (a symbol or a ~
function), got ~S" mask-when))
    (when (and mask-flag (not (symbolp mask-flag)))
      (%defmachine-error "interrupts :mask-flag must be a symbol, got ~S" mask-flag))
    (when (and mask-level (not (%interrupt-place-designator-p mask-level)))
      (%defmachine-error "interrupts :mask-level must be a symbol or (NAME INDEX), got ~S" mask-level))
    (when (and mask-level mask-level-when)
      (%defmachine-error "interrupts: at most one of :mask-level/:mask-level-when may be given"))
    (when (and mask-level-when (not (or (symbolp mask-level-when) (functionp mask-level-when))))
      (%defmachine-error "interrupts :mask-level-when must be a function designator (a symbol or a ~
function), got ~S" mask-level-when))
    (when (and mask-level-on-deliver (not mask-level))
      (%defmachine-error "interrupts :mask-level-on-deliver requires :mask-level"))
    (unless (and (integerp cycles) (>= cycles 0))
      (%defmachine-error "interrupts :cycles must be a non-negative integer, got ~S" cycles))
    (when (and mask-on-deliver (not mask-flag))
      (%defmachine-error "interrupts :mask-on-deliver requires :mask-flag"))
    (unless (member nesting '(:allow :priority))
      (%defmachine-error "interrupts :nesting must be :ALLOW or :PRIORITY, got ~S" nesting))
    (unless (or (null max-depth) (and (integerp max-depth) (plusp max-depth)))
      (%defmachine-error "interrupts :max-depth must be a positive integer, got ~S" max-depth))
    (unless (or (null deliver-level) (and (symbolp deliver-level) (not (keywordp deliver-level))))
      (%defmachine-error "interrupts :deliver-level must be a privilege level name, got ~S" deliver-level))
    (make-interrupt-descriptor :vector vector :nmi-vector nmi-vector :message message :save save :stack-name stack
                                :queue-depth queue :on-overflow on-overflow
                                :mask-when mask-when :mask-flag mask-flag
                                :mask-level mask-level :mask-level-when mask-level-when
                                :mask-level-on-deliver (and mask-level-on-deliver t)
                                :cycles cycles
                                :drop-on-zero-vector (and drop-on-zero-vector t)
                                :mask-on-deliver (and mask-on-deliver t)
                                :nesting nesting :max-depth max-depth
                                :deliver-level deliver-level)))

;; #164: (idle [:cycles n]) -- the cycle cost of one idle step.
(defun parse-idle-clause (form)
  (%definition-bind (&key (cycles 1)) form
    (%check-positive cycles ":cycles" 'idle)))

;; #109/#166: resolves an INTERRUPT-DESCRIPTOR's :STACK -- explicit or,
;; absent one, the machine's sole declared stack element -- exactly the way
;; WITH-MACHINE-BINDINGS's PUSH/POP macrolets do at macroexpansion time
;; (semantics.lisp), just resolved once here at DEFMACHINE time instead,
;; since the whole machine descriptor is already in hand. Returns two
;; values: the resolved stack name, and its kind (:STACK for a lasm :stack
;; element, :POINTER for a register bound by a (stack-pointer ...) clause).
;; Signals if GIVEN names neither, or (with none given) the machine declares
;; zero or more than one candidate of whichever kind is in play -- a :stack
;; element always wins the no-:stack-given default when one exists; the sole
;; stack-pointer is only the default when the machine declares no :stack
;; element at all.
(defun %resolve-interrupt-stack (descriptor given)
  (let ((name (machine-descriptor-name descriptor)))
    (if given
        (let ((element (gethash given (machine-descriptor-table descriptor))))
          (cond
            ((and element (eq (storage-element-kind element) :stack))
             (values given :stack))
            ((gethash given (machine-descriptor-stack-pointers descriptor))
             (values given :pointer))
            ((and element (eq (storage-element-kind element) :register))
             (%defmachine-error "interrupts on machine ~S: :stack ~S is a register with no ~
(stack-pointer ~S ...) clause declared" name given given))
            (t (%defmachine-error "interrupts on machine ~S: :stack ~S is not a declared stack ~
element or a register bound by (stack-pointer ...)" name given))))
        (let ((stacks (remove-if-not (lambda (e) (eq (storage-element-kind e) :stack))
                                      (machine-descriptor-elements descriptor))))
          (cond
            ((= (length stacks) 1) (values (storage-element-name (first stacks)) :stack))
            ((null stacks)
             (let ((pointers (loop for sp being the hash-values of (machine-descriptor-stack-pointers descriptor)
                                    collect (stack-pointer-descriptor-register sp))))
               (cond
                 ((= (length pointers) 1) (values (first pointers) :pointer))
                 ((null pointers)
                  (%defmachine-error "interrupts on machine ~S: :save needs a stack, but no stack ~
element or stack-pointer is declared -- name one explicitly with :stack" name))
                 (t (%defmachine-error "interrupts on machine ~S: more than one stack-pointer declared ~
(~{~S~^ ~}) -- name one explicitly with :stack" name pointers)))))
            (t (%defmachine-error "interrupts on machine ~S: more than one stack element declared ~
(~{~S~^ ~}) -- name one explicitly with :stack" name (mapcar #'storage-element-name stacks))))))))

;; #109: resolves an INTERRUPT-DESCRIPTOR's symbolic names -- :VECTOR/
;; :MESSAGE/:SAVE/:STACK/:MASK-FLAG -- against DESCRIPTOR's own ELEMENTS,
;; once they're fully known (BUILD-MACHINE-DESCRIPTOR, below, after the
;; ELEMENTS loop). Mutates DESCRIPTOR-INTERRUPTS in place (its :STACK slot
;; picks up %RESOLVE-INTERRUPT-STACK's resolved name); every other slot is
;; validated but left as parsed. A machine declaring no (interrupts ...)
;; clause never calls this.
(defun %finish-interrupt-model (descriptor)
  (let ((interrupts (machine-descriptor-interrupts descriptor))
        (name (machine-descriptor-name descriptor)))
    (labels ((element (n) (gethash n (machine-descriptor-table descriptor)))
             (require-kind (place kinds what)
               (let* ((n (if (consp place) (first place) place))
                      (e (element n)))
                 (unless e
                   (%defmachine-error "interrupts on machine ~S: ~A ~S is not a declared storage element"
                          name what n))
                 (unless (member (storage-element-kind e) kinds)
                   (%defmachine-error "interrupts on machine ~S: ~A ~S must be a ~{~S~^ or a ~} element, ~
got ~S" name what n kinds (storage-element-kind e)))
                 ;; A bare banked register is ambiguous (which bank cell?) and
                 ;; rejected at DEFMACHINE time rather than at first delivery.
                 ;; #163: (NAME INDEX) names one cell of a banked register,
                 ;; read/written through REGREF; a bare banked NAME stays
                 ;; ambiguous and rejected.
                 (if (consp place)
                     (unless (and (eq (storage-element-kind e) :register)
                                  (< -1 (second place) (storage-element-count e)))
                       (%defmachine-error "interrupts on machine ~S: ~A ~S is out of range for register ~S ~
(~D cell~:P)" name what place n (storage-element-count e)))
                     (when (> (storage-element-count e) 1)
                       (%defmachine-error "interrupts on machine ~S: ~A ~S is a banked (:count > 1) register -- ~
name one cell as (~S INDEX), or use a scalar register" name what n n))))))
      (require-kind (interrupt-descriptor-vector interrupts) '(:register) ":vector")
      (when (interrupt-descriptor-nmi-vector interrupts)
        (require-kind (interrupt-descriptor-nmi-vector interrupts) '(:register) ":nmi-vector"))
      (require-kind (interrupt-descriptor-message interrupts) '(:register) ":message")
      (dolist (n (interrupt-descriptor-save interrupts))
        (require-kind n '(:register :flag) ":save"))
      (when (interrupt-descriptor-mask-flag interrupts)
        (require-kind (interrupt-descriptor-mask-flag interrupts) '(:flag) ":mask-flag"))
      (when (interrupt-descriptor-mask-level interrupts)
        (require-kind (interrupt-descriptor-mask-level interrupts) '(:register) ":mask-level")))
    (multiple-value-bind (stack-name stack-kind)
        (%resolve-interrupt-stack descriptor (interrupt-descriptor-stack-name interrupts))
      (setf (interrupt-descriptor-stack-name interrupts) stack-name)
      (setf (interrupt-descriptor-stack-kind interrupts) stack-kind))))

;; (instruction-word :width n (field name width) (field name width) ...)
;; (#20, M4) -- a DCPU-16-shaped machine's whole instruction is one N-bit word
;; split into bit fields rather than a cell-per-operand stream. FIELDS is
;; parsed MSB-first as declared: the first field named occupies the highest
;; bits, mirroring how (opcode n)/(operand ...) subclauses already read
;; top-down in the mockups this is modeled on (LASM-plan.md sec. 3.8).
;;
;; The whole-cell check (WIDTH must be a multiple of the machine's own memory
;; cell width, #53) can't happen here -- a MEMORY clause may be declared after
;; INSTRUCTION-WORD in source order, and DEFMACHINE parses clauses one at a
;; time. BUILD-MACHINE-DESCRIPTOR finishes the layout (WIDTH-CELLS,
;; CELL-WIDTH) once every element is known.
;;
;; #64: an optional (layout NAME (field name width)...) form declares an
;; alternate bit-field split sharing this clause's own :WIDTH and OPCODE
;; field -- a per-instruction DEFINSTRUCTION names which layout it encodes
;; against (its (layout NAME) encoding subclause), so a machine can express
;; e.g. CHIP8's 1NNN (4/12) alongside 6XNN (4/4/8) in one 16-bit word. See
;; PARSE-INSTRUCTION-WORD-CLAUSE for the cross-layout checks.
;; #64: FIELD-FORMS is one layout's (field name width) forms -- the default
;; layout's own, or one (layout NAME ...) alternate's. Shared by both so the
;; per-layout rules (at least one field, FIELD head, no duplicate names
;; *within* this layout, positive widths, exactly one OPCODE field, widths
;; summing to WIDTH) can't drift between the two call sites. CONTEXT names the
;; layout in error messages -- "instruction-word" for the default, or
;; "instruction-word layout NAME" for an alternate.
(defun %parse-instruction-word-fields (field-forms width context)
  (unless field-forms
    (%defmachine-error "~A requires at least one (field name width) clause" context))
  (let ((seen (make-hash-table :test 'eq))
        (opcode-seen nil)
        (total 0)
        fields)
    (dolist (field-form field-forms)
      (%definition-bind (head name field-width) field-form
        (unless (eq head 'field)
          (%defmachine-error "~A: expected (field name width), got ~S" context field-form))
        (when (gethash name seen)
          (%defmachine-error "~A: duplicate field name ~S" context name))
        (setf (gethash name seen) t)
        (%check-positive field-width ":width" name)
        (when (eq name 'opcode) (setf opcode-seen t))
        (cl:push (list name field-width) fields)
        (incf total field-width)))
    (unless opcode-seen
      (%defmachine-error "~A requires exactly one field named OPCODE" context))
    (unless (= total width)
      (%defmachine-error "~A: field widths sum to ~D, but :width is ~D" context total width))
    ;; FIELDS was accumulated MSB-first-declared but CL:PUSH-reversed, so
    ;; NREVERSE restores declaration order before computing each field's
    ;; shift from the LSB -- the last-declared field sits at shift 0.
    (setf fields (nreverse fields))
    (let ((shift width))
      (mapcar (lambda (f)
                (%definition-bind (name field-width) f
                  (decf shift field-width)
                  (list name field-width shift)))
              fields))))

;; #64: (layout NAME (field name width)...) -- one alternate bit-field split
;; for a subset of a word-encoded machine's opcodes, e.g. CHIP8's 1NNN
;; (4/12) vs. 6XNN (4/4/8) sharing one 16-bit word. Parsed here into a bare
;; INSTRUCTION-WORD-LAYOUT (ALTERNATES always NIL -- only the default layout
;; nests alternates); PARSE-INSTRUCTION-WORD-CLAUSE cross-checks it against
;; the default (shared :WIDTH, identical OPCODE field) once every layout is
;; known.
(defun %parse-instruction-word-layout-form (form width)
  (%definition-bind (head name &rest field-forms) form
    (unless (eq head 'layout)
      (%defmachine-error "instruction-word: expected (layout name (field ...)...), got ~S" form))
    (unless (symbolp name)
      (%defmachine-error "instruction-word: layout name must be a symbol, got ~S" name))
    (when (find-if #'keywordp field-forms)
      (%defmachine-error "instruction-word layout ~S: options such as :endian belong on the ~
instruction-word clause, which every layout shares" name))
    (make-instruction-word-layout
     :name name
     :width width
     :width-cells 1 ; placeholder -- %FINISH-INSTRUCTION-WORD-LAYOUT sets the real value
     :cell-width 1  ; placeholder
     :fields (%parse-instruction-word-fields
              field-forms width (format nil "instruction-word layout ~S" name)))))

(defun parse-instruction-word-clause (form)
  (let* ((body (rest form))
         (width-pos (position :width body))
         (width (and width-pos (nth (1+ width-pos) body)))
         (body (if width-pos
                   (append (subseq body 0 width-pos) (subseq body (+ width-pos 2)))
                   body))
         (endian-pos (position :endian body))
         (declared-endian (and endian-pos (%check-endian (nth (1+ endian-pos) body) 'instruction-word)))
         (rest-forms (if endian-pos
                         (append (subseq body 0 endian-pos) (subseq body (+ endian-pos 2)))
                         body))
         ;; #64: (layout ...) forms are the machine's alternates; everything
         ;; else is the default layout's own (field ...) forms.
         (layout-forms (remove-if-not (lambda (f) (eq (first f) 'layout)) rest-forms))
         ;; #191: (extra-word-order FIELD...) -- at most one, default layout only.
         (order-forms (remove-if-not (lambda (f) (eq (first f) 'extra-word-order)) rest-forms))
         (field-forms (remove-if (lambda (f) (member (first f) '(layout extra-word-order))) rest-forms)))
    (unless width (%defmachine-error "instruction-word requires :width"))
    (%check-positive width ":width" 'instruction-word)
    (when (rest order-forms)
      (%defmachine-error "instruction-word: more than one (extra-word-order ...) clause"))
    (let* ((fields (%parse-instruction-word-fields field-forms width "instruction-word"))
           (opcode-field (find 'opcode fields :key #'first))
           (extra-word-order (rest (first order-forms)))
           (alternates (mapcar (lambda (f) (%parse-instruction-word-layout-form f width)) layout-forms)))
      ;; Cross-layout checks (#64): alternate names unique and non-NIL
      ;; (NIL always names the default), each alternate held to the same
      ;; :WIDTH as the default (%PARSE-INSTRUCTION-WORD-FIELDS' own
      ;; fields-sum-to-width check, applied per layout), and each alternate's
      ;; OPCODE field identical in width and shift to the default's -- decode
      ;; reads the OPCODE field off the machine's *default* layout alone
      ;; (%DECODE-WORD-INSTRUCTION), so every candidate must agree on where
      ;; it lives regardless of which layout actually encoded it. These two
      ;; checks are also what makes #140's relaxed co-tenancy check sound: a
      ;; descriptor's WORD-FIELD-CHOICE/WORD-CONSTANT entries carry absolute
      ;; bit positions within one shared word size, so two co-tenants naming
      ;; different layouts can still be compared bit-for-bit
      ;; (%DESCRIPTORS-DISTINGUISHABLE-P, instruction.lisp) with no need to
      ;; know which layout matched first.
      (loop for tail on extra-word-order
            do (unless (find (first tail) fields :key #'first)
                 (%defmachine-error "instruction-word: extra-word-order names ~S, which is not a declared field"
                        (first tail)))
               (when (member (first tail) (rest tail))
                 (%defmachine-error "instruction-word: extra-word-order names ~S twice" (first tail))))
      (let ((names (mapcar #'instruction-word-layout-name alternates)))
        (loop for tail on names
              when (member (first tail) (rest tail))
                do (%defmachine-error "instruction-word: duplicate layout name ~S" (first tail))))
      (dolist (alt alternates)
        (let ((alt-opcode (instruction-word-field alt 'opcode)))
          (unless (equal (rest alt-opcode) (rest opcode-field))
            (%defmachine-error "instruction-word layout ~S: OPCODE field ~S disagrees with the ~
default layout's OPCODE field ~S -- every layout must place OPCODE identically"
                   (instruction-word-layout-name alt) alt-opcode opcode-field))))
      (make-instruction-word-layout
       :name nil
       :width width
       :width-cells 1 ; placeholder -- %FINISH-INSTRUCTION-WORD-LAYOUT sets the real value
       :cell-width 1  ; placeholder
       :fields fields
       :declared-endian declared-endian
       :extra-word-order extra-word-order
       :alternates alternates))))

(defun %finish-instruction-word-layout (layout cell-width endian)
  "Fill in LAYOUT's WIDTH-CELLS, CELL-WIDTH and ENDIAN (#66) -- its own
DECLARED-ENDIAN if it has one, else ENDIAN -- and recurse into
its ALTERNATES (#64), once the machine's own memory cell width/endianness is
known (BUILD-MACHINE-DESCRIPTOR, after every MEMORY element has been parsed)
-- see PARSE-INSTRUCTION-WORD-CLAUSE's docstring for why this can't happen at
clause-parse time. Signals if the instruction word's bit width isn't a whole
number of cells. Setting ENDIAN here (rather than resolving it per-decode)
means %DECODE-WORD-INSTRUCTION (decoder.lisp) needs no extra lookup."
  (let ((width (instruction-word-layout-width layout))
        (endian (or (instruction-word-layout-declared-endian layout) endian)))
    (unless (zerop (mod width cell-width))
      (%defmachine-error "instruction-word :width ~D must be a whole number of ~D-bit cells"
             width cell-width))
    (setf (instruction-word-layout-width-cells layout) (/ width cell-width)
          (instruction-word-layout-cell-width layout) cell-width
          (instruction-word-layout-endian layout) endian)
    (dolist (alt (instruction-word-layout-alternates layout))
      (%finish-instruction-word-layout alt cell-width endian)))
  layout)

;;; Memory / cell-width resolution
;;
;; Shared by DEFINSTRUCTION (default operand width), ASSEMBLE (the assembled
;; output's element width, #53), and the emulator (LOAD-PROGRAM, STEP-MACHINE)
;; -- one place decides which memory element a machine-level operation means
;; and how wide its cells are, so those three pipelines can't drift apart on
;; a machine with more than one memory element.

(defun %descriptor-memory-elements (descriptor)
  (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                  (machine-descriptor-elements descriptor)))

(defun %descriptor-resolve-memory (descriptor memory)
  "MEMORY if given, else the sole :MEMORY element declared on DESCRIPTOR.
Signals if DESCRIPTOR declares none or more than one -- an ambiguous case
that requires the caller to say which memory element it means. Works
directly off a MACHINE-DESCRIPTOR object (rather than a name looked up via
FIND-MACHINE-DESCRIPTOR) so BUILD-MACHINE-DESCRIPTOR can call it on a
descriptor still being built, before it's registered in *MACHINES* --
%RESOLVE-MEMORY is the name-based wrapper every other caller uses."
  (or memory
      (let ((mem-elements (%descriptor-memory-elements descriptor)))
        (cond
          ((null mem-elements)
           (%emulator-usage-error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
          ((> (length mem-elements) 1)
           (%emulator-usage-error "Machine ~S: more than one memory element declared (~S) -- ~
pass :MEMORY explicitly" (machine-descriptor-name descriptor)
                  (mapcar #'storage-element-name mem-elements)))
          (t (storage-element-name (first mem-elements)))))))

(defun %compute-descriptor-cell-width (descriptor)
  "DESCRIPTOR's code cell width in bits (#53) when no MEMORY-NAME disambiguates
-- the sole memory element's, or, when DESCRIPTOR declares several, their
shared width if every one agrees. The uncached body %DESCRIPTOR-CELL-WIDTH
memoizes below."
  (let ((mem-elements (%descriptor-memory-elements descriptor)))
    (cond
      ((null mem-elements)
       (%emulator-usage-error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
      ((null (rest mem-elements))
       (storage-element-cell-width (first mem-elements)))
      (t (let ((widths (remove-duplicates (mapcar #'storage-element-cell-width mem-elements))))
           (if (null (rest widths))
               (first widths)
               (%emulator-usage-error "Machine ~S: more than one memory element declared with ~
different cell widths (~{~S~^, ~}) -- pass :MEMORY explicitly"
                      (machine-descriptor-name descriptor)
                      (mapcar (lambda (e) (list (storage-element-name e)
                                                 (storage-element-cell-width e)))
                              mem-elements))))))))

(defun %descriptor-cell-width (descriptor &optional memory-name)
  "DESCRIPTOR's code cell width in bits (#53): MEMORY-NAME's own CELL-WIDTH
when given, else the sole memory element's (or their shared width, when
DESCRIPTOR declares several that agree) -- otherwise the caller must specify
which memory element it means, same as %DESCRIPTOR-RESOLVE-MEMORY's own
ambiguity error. See %DESCRIPTOR-RESOLVE-MEMORY for why this takes a
descriptor object rather than a machine name.

#63: the no-MEMORY-NAME case is memoized on DESCRIPTOR's own
CELL-WIDTH-CACHE slot (storage.lisp) -- %COMPUTE-DESCRIPTOR-CELL-WIDTH
otherwise reconses ELEMENTS' memory sublist and calls REMOVE-DUPLICATES on
every call, and this is read once per ENCODE-INSTRUCTION plus several times
per assembler relaxation pass. Lazy, not computed at BUILD-MACHINE-
DESCRIPTOR time, so a machine with genuinely ambiguous cell widths still
signals its error at first use rather than at DEFMACHINE time. The
explicit-MEMORY-NAME path is a single DESCRIPTOR-ELEMENT hash lookup
already and stays uncached."
  (if memory-name
      (storage-element-cell-width (descriptor-element descriptor memory-name))
      (let ((cached (machine-descriptor-cell-width-cache descriptor)))
        (if (eq cached :unset)
            (setf (machine-descriptor-cell-width-cache descriptor)
                  (%compute-descriptor-cell-width descriptor))
            cached))))

(defun %resolve-memory (machine-name memory)
  "MEMORY if given, else the sole :MEMORY element declared on MACHINE-NAME.
Signals if MACHINE-NAME declares none or more than one. Name-based wrapper
around %DESCRIPTOR-RESOLVE-MEMORY for every caller outside DEFMACHINE's own
expansion (the assembler, the emulator, DEFINSTRUCTION)."
  (%descriptor-resolve-memory (find-machine-descriptor machine-name) memory))

(defun %machine-cell-width (machine-name &optional memory-name)
  "MACHINE-NAME's code cell width in bits (#53). Name-based wrapper around
%DESCRIPTOR-CELL-WIDTH for every caller outside DEFMACHINE's own expansion."
  (%descriptor-cell-width (find-machine-descriptor machine-name) memory-name))

;;; Memory / endian resolution (#66) -- exact twin of the cell-width trio
;;; above, same rationale: one place decides a machine's cell ordering so
;;; the encoder (instruction.lisp), decoder (decoder.lisp) and assembler
;;; (assembler.lisp) can't drift apart on a machine with more than one
;;; memory element.

(defun %compute-descriptor-endian (descriptor)
  "DESCRIPTOR's cell endianness (#66) when no MEMORY-NAME disambiguates --
the sole memory element's, or, when DESCRIPTOR declares several, their
shared endianness if every one agrees. The uncached body %DESCRIPTOR-ENDIAN
memoizes below."
  (let ((mem-elements (%descriptor-memory-elements descriptor)))
    (cond
      ((null mem-elements)
       (%emulator-usage-error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
      ((null (rest mem-elements))
       (storage-element-endian (first mem-elements)))
      (t (let ((endians (remove-duplicates (mapcar #'storage-element-endian mem-elements)
                                            :test #'equal)))
           (if (null (rest endians))
               (first endians)
               (%emulator-usage-error "Machine ~S: more than one memory element declared with ~
different endianness (~{~S~^, ~}) -- pass :MEMORY explicitly"
                      (machine-descriptor-name descriptor)
                      (mapcar (lambda (e) (list (storage-element-name e)
                                                 (storage-element-endian e)))
                              mem-elements))))))))

(defun %descriptor-endian (descriptor &optional memory-name)
  "DESCRIPTOR's cell endianness (#66): MEMORY-NAME's own ENDIAN when given,
else the sole memory element's (or their shared endianness, when DESCRIPTOR
declares several that agree) -- otherwise the caller must specify which
memory element it means, same as %DESCRIPTOR-CELL-WIDTH's own ambiguity
error. Memoized on DESCRIPTOR's own ENDIAN-CACHE slot (storage.lisp), same
rationale as CELL-WIDTH-CACHE (#63)."
  (if memory-name
      (storage-element-endian (descriptor-element descriptor memory-name))
      (let ((cached (machine-descriptor-endian-cache descriptor)))
        (if (eq cached :unset)
            (setf (machine-descriptor-endian-cache descriptor)
                  (%compute-descriptor-endian descriptor))
            cached))))

(defun %machine-endian (machine-name &optional memory-name)
  "MACHINE-NAME's cell endianness (#66). Name-based wrapper around
%DESCRIPTOR-ENDIAN for every caller outside DEFMACHINE's own expansion."
  (%descriptor-endian (find-machine-descriptor machine-name) memory-name))

(defun parse-undefined-opcode-clause (form)
  (%definition-bind (policy) form
    (unless (member policy '(:fault :nop :trap))
      (%defmachine-error "undefined-opcode must be :FAULT, :NOP or :TRAP, got ~S" policy))
    policy))

;; #111: (privilege :level NAME [:shift N] [:width N] :levels (LEVEL...)
;;   [:on-violation :fault/:trap/(:interrupt DATA [:priority N] [:non-maskable t/nil])]).
;; Each LEVEL is NAME or (NAME VALUE); VALUE defaults to the entry's index.
;; :LEVEL itself is resolved against the machine's elements later, in
;; %FINISH-PRIVILEGE-MODEL.
(defun parse-privilege-clause (form)
  (%definition-bind (&key level (shift 0) width levels (on-violation :fault)) form
    (let (violation-data (violation-priority 0) violation-non-maskable)
      (unless (and level (symbolp level))
        (%defmachine-error "privilege requires :level naming a flag or register, got ~S" level))
      (unless (typep shift '(integer 0))
        (%defmachine-error "privilege :shift must be a non-negative integer, got ~S" shift))
      (unless (typep width '(or null (integer 1)))
        (%defmachine-error "privilege :width must be a positive integer, got ~S" width))
      (unless (and (consp levels) (listp (cdr (last levels))))
        (%defmachine-error "privilege requires :levels, a non-empty list ordered least to most privileged, got ~S"
               levels))
      (let ((interruptp (and (consp on-violation) (eq (first on-violation) :interrupt))))
        (unless (or (member on-violation '(:fault :trap))
                    (and interruptp
                         (>= (length on-violation) 2)
                         (typep (second on-violation) '(integer 0))
                         (let ((options (cddr on-violation)))
                           (and (evenp (length options))
                                (loop for (key value) on options by #'cddr
                                      always (case key
                                               (:priority (integerp value))
                                               (:non-maskable (typep value 'boolean))))))))
          (%defmachine-error "privilege :on-violation must be :FAULT, :TRAP or (:INTERRUPT DATA [:PRIORITY n] ~
[:NON-MASKABLE t/nil]), got ~S" on-violation))
        (when interruptp
          (setf violation-data (second on-violation)
                violation-priority (or (getf (cddr on-violation) :priority) 0)
                violation-non-maskable (getf (cddr on-violation) :non-maskable)
                on-violation :interrupt)))
      (let (names values)
        (loop for entry in levels
              for index from 0
              do (let ((name (if (consp entry) (first entry) entry))
                       (value (if (consp entry) (second entry) index)))
                   (unless (and name (symbolp name) (not (keywordp name))
                                (or (atom entry) (and (= (length entry) 2))))
                     (%defmachine-error "privilege :levels entries must be NAME or (NAME VALUE), got ~S" entry))
                   (unless (and (integerp value) (>= value 0))
                     (%defmachine-error "privilege level ~S: value must be a non-negative integer, got ~S"
                            name value))
                   (when (member name names)
                     (%defmachine-error "privilege: duplicate level ~S" name))
                   (when (member value values)
                     (%defmachine-error "privilege level ~S: value ~D is already used by another level"
                            name value))
                   (cl:push name names)
                   (cl:push value values)))
        (make-privilege-descriptor :level level :shift shift :width width :levels (nreverse names) :values (nreverse values)
                                   :on-violation on-violation :violation-data violation-data
                                   :violation-priority violation-priority
                                   :violation-non-maskable violation-non-maskable)))))

(defun %finish-privilege-model (descriptor)
  "Resolve the privilege clause's :LEVEL and every region's :PRIVILEGE against
DESCRIPTOR's finished elements."
  (let* ((privilege (machine-descriptor-privilege descriptor))
         (name (machine-descriptor-name descriptor))
         (element (and privilege (gethash (privilege-descriptor-level privilege)
                                          (machine-descriptor-table descriptor)))))
    (let ((deliver-level (and (machine-descriptor-interrupts descriptor)
                              (interrupt-descriptor-deliver-level (machine-descriptor-interrupts descriptor)))))
      (when deliver-level
        (unless privilege
          (%defmachine-error "interrupts on machine ~S: :deliver-level requires a (privilege ...) clause" name))
        (unless (member deliver-level (privilege-descriptor-levels privilege))
          (%defmachine-error "interrupts on machine ~S: unknown privilege level ~S" name deliver-level))))
    (when privilege
      (unless (and element
                   (or (eq (storage-element-kind element) :flag)
                       (and (eq (storage-element-kind element) :register)
                            (= (storage-element-count element) 1))))
        (%defmachine-error "privilege on machine ~S: :level ~S must be a declared flag or scalar register"
               name (privilege-descriptor-level privilege)))
      (let* ((flagp (eq (storage-element-kind element) :flag))
             (element-width (if flagp 1 (storage-element-width element)))
             (shift (privilege-descriptor-shift privilege))
             (width (or (privilege-descriptor-width privilege) (- element-width shift))))
        (unless (and (plusp width) (<= (+ shift width) element-width))
          (%defmachine-error "privilege on machine ~S: :shift ~D and :width ~D do not fit ~S, ~D bit~:P wide"
                 name shift width (privilege-descriptor-level privilege) element-width))
        (setf (privilege-descriptor-width privilege) width))
      (when (eq (privilege-descriptor-on-violation privilege) :interrupt)
        (let ((interrupts (machine-descriptor-interrupts descriptor)))
          (unless interrupts
            (%defmachine-error "privilege on machine ~S: :on-violation :interrupt requires an (interrupts ...) clause"
                   name))
          (let* ((message (interrupt-descriptor-message interrupts))
                 (message-element (gethash (if (consp message) (first message) message)
                                           (machine-descriptor-table descriptor))))
            (unless (< (privilege-descriptor-violation-data privilege)
                       (ash 1 (storage-element-width message-element)))
              (%defmachine-error "privilege on machine ~S: violation data ~D does not fit the interrupt :message ~S"
                     name (privilege-descriptor-violation-data privilege) message)))))
      (dolist (value (privilege-descriptor-values privilege))
        (unless (< value (ash 1 (privilege-descriptor-width privilege)))
          (%defmachine-error "privilege on machine ~S: level value ~D does not fit the ~D-bit field of ~S"
                 name value (privilege-descriptor-width privilege) (privilege-descriptor-level privilege)))))
    (dolist (element (machine-descriptor-elements descriptor))
      (dolist (required (list (storage-element-read-privilege element)
                              (storage-element-write-privilege element)))
        (when required
          (unless privilege
            (%defmachine-error "~(~A~) ~S on machine ~S: :privilege requires a (privilege ...) clause"
                   (storage-element-kind element) (storage-element-name element) name))
          (unless (member required (privilege-descriptor-levels privilege))
            (%defmachine-error "~(~A~) ~S on machine ~S: unknown privilege level ~S"
                   (storage-element-kind element) (storage-element-name element) name required))))
      (let ((fields (storage-element-field-privileges element))
            (seen 0))
        (when fields
          (unless privilege
            (%defmachine-error "register ~S on machine ~S: :fields requires a (privilege ...) clause"
                   (storage-element-name element) name))
          (when (gethash (storage-element-name element) (machine-descriptor-stack-pointers descriptor))
            (%defmachine-error "register ~S on machine ~S: :fields cannot gate a stack-pointer register"
                   (storage-element-name element) name)))
        (dolist (field fields)
          (destructuring-bind (mask required policy) field
            (declare (ignore policy))
            (unless (member required (privilege-descriptor-levels privilege))
              (%defmachine-error "register ~S on machine ~S: unknown privilege level ~S"
                     (storage-element-name element) name required))
            (unless (< mask (ash 1 (storage-element-width element)))
              (%defmachine-error "register ~S on machine ~S: :fields mask #x~X does not fit ~D bits"
                     (storage-element-name element) name mask (storage-element-width element)))
            (unless (zerop (logand seen mask))
              (%defmachine-error "register ~S on machine ~S: :fields masks overlap at #x~X"
                     (storage-element-name element) name (logand seen mask)))
            (setf seen (logior seen mask)))))
      (dolist (region (storage-element-regions element))
        (dolist (required (list (memory-region-read-privilege region)
                                (memory-region-write-privilege region)
                                (memory-region-execute-privilege region)))
          (when required
            (unless privilege
              (%defmachine-error "region ~S on machine ~S: :privilege requires a (privilege ...) clause"
                     (memory-region-name region) name))
            (unless (member required (privilege-descriptor-levels privilege))
              (%defmachine-error "region ~S on machine ~S: unknown privilege level ~S"
                     (memory-region-name region) name required))))))))

(defun parse-properties-clause (form)
  (unless (evenp (length form))
    (%defmachine-error "properties requires key/value pairs, got ~S" form))
  (let ((keys (loop for (key) on form by #'cddr collect key)))
    (dolist (key keys)
      (unless (keywordp key)
        (%defmachine-error "properties key must be a keyword, got ~S" key)))
    (loop for (key . rest) on keys
          when (member key rest)
            do (%defmachine-error "properties: duplicate key ~S" key)))
  (copy-list form))

(defun parse-machine-clauses (clauses)
  (let (elements instruction-word clock-speed reset-pc devices interrupts stack-pointers privilege
        (idle-cycles 1) idle-seen (undefined-opcode :fault) undefined-opcode-seen properties properties-seen)
    (dolist (clause clauses)
      (case (first clause)
        (register (cl:push (parse-register-clause (rest clause)) elements))
        (stack (cl:push (parse-stack-clause (rest clause)) elements))
        (memory (cl:push (parse-memory-clause (rest clause)) elements))
        (flags (dolist (e (parse-flags-clause (rest clause))) (cl:push e elements)))
        (instruction-word
         (when instruction-word
           (%defmachine-error "DEFMACHINE: more than one instruction-word clause"))
         (setf instruction-word (parse-instruction-word-clause clause)))
        (clock-speed
         (when clock-speed
           (%defmachine-error "DEFMACHINE: more than one clock-speed clause"))
         (setf clock-speed (parse-clock-speed-clause (rest clause))))
        (reset-pc
         (when reset-pc
           (%defmachine-error "DEFMACHINE: more than one reset-pc clause"))
         (setf reset-pc (parse-reset-pc-clause (rest clause))))
        (device (cl:push (parse-device-clause (rest clause)) devices))
        (interrupts
         (when interrupts
           (%defmachine-error "DEFMACHINE: more than one interrupts clause"))
         (setf interrupts (parse-interrupts-clause (rest clause))))
        (stack-pointer (cl:push (parse-stack-pointer-clause (rest clause)) stack-pointers))
        (privilege
         (when privilege
           (%defmachine-error "DEFMACHINE: more than one privilege clause"))
         (setf privilege (parse-privilege-clause (rest clause))))
        (idle
         (when idle-seen
           (%defmachine-error "DEFMACHINE: more than one idle clause"))
         (setf idle-seen t
               idle-cycles (parse-idle-clause (rest clause))))
        (undefined-opcode
         (when undefined-opcode-seen
           (%defmachine-error "DEFMACHINE: more than one undefined-opcode clause"))
         (setf undefined-opcode-seen t
               undefined-opcode (parse-undefined-opcode-clause (rest clause))))
        (properties
         (when properties-seen
           (%defmachine-error "DEFMACHINE: more than one properties clause"))
         (setf properties-seen t
               properties (parse-properties-clause (rest clause))))
        ((without-instructions instruction-cycles without-storage without-devices)
         (%defmachine-error "DEFMACHINE: ~S is only valid on a machine declared with (:extends parent)"
                (first clause)))
        (t (%defmachine-error "Unknown DEFMACHINE clause head ~S in ~S" (first clause) clause))))
    (values (nreverse elements) instruction-word clock-speed (nreverse devices) interrupts
            (nreverse stack-pointers) undefined-opcode properties privilege idle-cycles reset-pc)))

(defun build-machine-descriptor (name clauses)
  (multiple-value-bind (elements instruction-word clock-speed devices interrupts stack-pointers
                        undefined-opcode properties privilege idle-cycles reset-pc)
      (parse-machine-clauses clauses)
    (let ((descriptor (make-machine-descriptor :name name :instruction-word instruction-word
                                                :clock-speed clock-speed :reset-pc reset-pc
                                                :devices devices
                                                :interrupts interrupts :privilege privilege
                                                :idle-cycles idle-cycles
                                                :undefined-opcode undefined-opcode
                                                :properties properties
                                                :source-clauses clauses))
          (seen (make-hash-table :test 'eq)))
      (dolist (element elements)
        (when (gethash (storage-element-name element) seen)
          (%defmachine-error "Duplicate storage element name ~S in machine ~S"
                 (storage-element-name element) name))
        (setf (gethash (storage-element-name element) seen) t)
        (setf (gethash (storage-element-name element) (machine-descriptor-table descriptor))
              element)
        ;; #72: an aliased register's :names share the same namespace as
        ;; every storage element name -- SEEN also catches an alias
        ;; colliding with another element (or another register's alias),
        ;; not just with a bare element name.
        (loop for alias in (storage-element-names element)
              for index from 0
              do (when (gethash alias seen)
                   (%defmachine-error "Duplicate storage element name ~S in machine ~S" alias name))
                 (setf (gethash alias seen) t)
                  (setf (gethash (symbol-name alias) (machine-descriptor-register-aliases descriptor))
                        index)
                  (setf (gethash (symbol-name alias)
                                 (machine-descriptor-register-alias-elements descriptor))
                        element))
        ;; #107: a memory element's region names share the same namespace too
        ;; -- SEEN also catches a region colliding with an element name, a
        ;; register alias, or another region, e.g. (region ram ...) inside
        ;; (memory ram ...) itself.
        (dolist (region (storage-element-regions element))
          (when (gethash (memory-region-name region) seen)
            (%defmachine-error "Duplicate storage element name ~S in machine ~S" (memory-region-name region) name))
          (setf (gethash (memory-region-name region) seen) t)))
      ;; #108: a declared device's name joins the same namespace -- SEEN also
      ;; catches a device colliding with an element name, register alias, or
      ;; region name, and two devices sharing a name.
      (dolist (device-descriptor devices)
        (when (gethash (device-descriptor-name device-descriptor) seen)
          (%defmachine-error "Duplicate storage element name ~S in machine ~S"
                 (device-descriptor-name device-descriptor) name))
        (setf (gethash (device-descriptor-name device-descriptor) seen) t))
      ;; #158: a region's :DEVICE resolves to the device's fixed bus index --
      ;; its position among the (merged) declared devices.
      (dolist (element elements)
        (dolist (region (storage-element-regions element))
          (when (memory-region-device region)
            (let ((index (position (memory-region-device region) devices
                                   :key #'device-descriptor-name)))
              (unless index
                (%defmachine-error "Region ~S in machine ~S: no device named ~S"
                       (memory-region-name region) name (memory-region-device region)))
              (setf (memory-region-device-index region) index)))))
      (setf (machine-descriptor-elements descriptor) elements)
      (when reset-pc
        (let ((pc (gethash 'pc (machine-descriptor-table descriptor))))
          (unless (and pc (eq (storage-element-kind pc) :register) (= (storage-element-count pc) 1))
            (%defmachine-error "reset-pc on machine ~S: no single register named PC" name))
          (unless (< reset-pc (ash 1 (storage-element-width pc)))
            (%defmachine-error "reset-pc ~S on machine ~S does not fit PC's ~D-bit width"
                   reset-pc name (storage-element-width pc)))))
      ;; INSTRUCTION-WORD's WIDTH-CELLS/CELL-WIDTH/ENDIAN can only be finished
      ;; now that every MEMORY element is known (#53, #66) -- see
      ;; PARSE-INSTRUCTION-WORD-CLAUSE and %FINISH-INSTRUCTION-WORD-LAYOUT.
      ;; A machine with no memory element at all (e.g. SIXTYFOO's
      ;; instruction-less ancestor) or more than one with disagreeing cell
      ;; widths/endianness only matters once an instruction-word clause is
      ;; actually declared, so %MACHINE-CELL-WIDTH/%MACHINE-ENDIAN's errors
      ;; are deferred to here.
      (when instruction-word
        (%finish-instruction-word-layout instruction-word (%descriptor-cell-width descriptor)
                                          (or (instruction-word-layout-declared-endian instruction-word)
                                              (%descriptor-endian descriptor))))
      ;; #166: STACK-POINTERS' REGISTER/MEMORY must resolve before
      ;; %FINISH-INTERRUPT-MODEL, since an (interrupts ...) clause naming a
      ;; register as its :stack looks that register up in
      ;; MACHINE-DESCRIPTOR-STACK-POINTERS below.
      (%finish-stack-pointers descriptor stack-pointers)
      ;; #109: INTERRUPTS' :VECTOR/:MESSAGE/:SAVE/:STACK/:MASK-FLAG can only
      ;; be resolved against real storage elements now that ELEMENTS is
      ;; known -- same deferred-finishing reason as INSTRUCTION-WORD above.
      (when interrupts
        (%finish-interrupt-model descriptor))
      (%finish-privilege-model descriptor)
      descriptor)))

;;; Machine families

(defun %plist-merge (parent child)
  "PARENT's keyword plist with every key of CHILD's overriding or appended."
  (let ((result (copy-list parent)))
    (loop for (key value) on child by #'cddr
          do (setf (getf result key) value))
    result))

(defun %region-form-p (form)
  (and (consp form) (eq (first form) 'region)))

(defun %flag-entry-name (entry)
  (if (consp entry) (first entry) entry))

(defun %merge-keyed-clause (parent child)
  "Merge CHILD's (HEAD NAME ...) clause over PARENT's. A memory clause's
nested (region ...) forms are replaced wholesale when CHILD gives any."
  (%definition-bind (head name &rest parent-body) parent
    (let* ((child-body (cddr child))
           (regions (if (eq head 'memory)
                        (or (remove-if-not #'%region-form-p child-body)
                            (remove-if-not #'%region-form-p parent-body))
                        nil))
           (plist (%plist-merge (remove-if #'%region-form-p parent-body)
                                (remove-if #'%region-form-p child-body))))
      (list* head name (append plist regions)))))

(defun %merge-machine-clauses (parent-clauses child-clauses)
  "PARENT-CLAUSES with CHILD-CLAUSES merged over them: a clause naming an
existing register/stack/memory/device merges into the parent's in place, a new
one is appended. Singletons replace (clock-speed, reset-pc, undefined-opcode) or merge
key by key (interrupts, properties, privilege, idle); flags are additive."
  (let ((merged (copy-list parent-clauses))
        (added '()))
    (dolist (clause child-clauses)
      (let ((head (first clause)))
        (case head
          ((instruction-word stack-pointer)
           (%defmachine-error "DEFMACHINE: a machine extending another cannot declare ~S -- inherited ~
instructions are compiled against the parent's" head))
          ((register stack memory device)
           (let ((position (position-if (lambda (p) (and (eq (first p) head)
                                                          (eq (second p) (second clause))))
                                        merged)))
             (if position
                 (setf (nth position merged)
                       (%merge-keyed-clause (nth position merged) clause))
                 (cl:push clause added))))
          (flags
           (let ((known (loop for p in merged when (eq (first p) 'flags)
                              append (mapcar #'%flag-entry-name (rest p)))))
             (let ((new (remove-if (lambda (f) (member (%flag-entry-name f) known)) (rest clause))))
               (when new (cl:push (cons 'flags new) added)))))
          ((clock-speed reset-pc undefined-opcode)
           (let ((position (position head merged :key #'first)))
             (if position
                 (setf (nth position merged) clause)
                 (cl:push clause added))))
          ((interrupts properties privilege idle)
           (let ((position (position head merged :key #'first)))
             (if position
                 (setf (nth position merged)
                       (cons head (%plist-merge (rest (nth position merged)) (rest clause))))
                 (cl:push clause added))))
          (t (%defmachine-error "Unknown DEFMACHINE clause head ~S in ~S" head clause)))))
    (append merged (nreverse added))))

(defun %clause-declares-p (clause head-names name)
  "True when CLAUSE declares NAME: a keyed clause whose head is in HEAD-NAMES, or a flags entry."
  (or (and (member (first clause) head-names) (eq (second clause) name))
      (and (eq (first clause) 'flags) (member name (rest clause) :key #'%flag-entry-name))))

(defun %remove-machine-clauses (machine-name merged child-clauses storage-names device-names)
  "MERGED with the register/flag STORAGE-NAMES and the DEVICE-NAMES removed.
Signals when CHILD-CLAUSES redeclare a removed name, and warns for a name MERGED lacks."
  (flet ((check (names heads)
           (dolist (name names)
             (when (some (lambda (c) (%clause-declares-p c heads name)) child-clauses)
               (%defmachine-error "Machine ~S: ~S is both removed and declared" machine-name name))
             (unless (some (lambda (c) (%clause-declares-p c heads name)) merged)
               (warn 'simple-style-warning
                     :format-control "Machine ~S: ~S is not a removable ~A of its parent"
                     :format-arguments (list machine-name name
                                             (if (eq (first heads) 'device) "device" "register or flag")))))))
    (check storage-names '(register stack memory))
    (check device-names '(device)))
  (loop for clause in merged
        for kept = (case (first clause)
                     ((register stack memory) (unless (member (second clause) storage-names) clause))
                     (device (unless (member (second clause) device-names) clause))
                     (flags (let ((entries (remove-if (lambda (e) (member (%flag-entry-name e) storage-names))
                                                      (rest clause))))
                              (when entries (cons 'flags entries))))
                     (t clause))
        when kept collect kept))

(defun %element-operand-cells (element)
  (ceiling (storage-element-addr-width element) (storage-element-cell-width element)))

(defun %check-inheritance-compatible (parent child)
  "Signal unless CHILD keeps everything PARENT's compiled instructions bake in."
  (let ((pname (machine-descriptor-name parent))
        (cname (machine-descriptor-name child)))
    (flet ((fail (fmt &rest args)
             (%defmachine-error "Machine ~S cannot extend ~S: ~?" cname pname fmt args)))
      (unless (equalp (machine-descriptor-instruction-word parent)
                      (machine-descriptor-instruction-word child))
        (fail "the instruction word differs"))
      (dolist (pe (remove-if (lambda (e) (member (storage-element-name e)
                                                 (machine-descriptor-removed-storage child)))
                             (machine-descriptor-elements parent)))
        (let* ((name (storage-element-name pe))
               (ce (gethash name (machine-descriptor-table child))))
          (unless (and ce (eq (storage-element-kind ce) (storage-element-kind pe)))
            (fail "storage element ~S is missing or changes kind" name))
          (case (storage-element-kind pe)
            (:register
             (unless (and (= (storage-element-count pe) (storage-element-count ce))
                          (equal (storage-element-names pe) (storage-element-names ce)))
               (fail "register ~S changes its :count or :names" name)))
            (:memory
             (unless (and (= (storage-element-cell-width pe) (storage-element-cell-width ce))
                          (equal (storage-element-endian pe) (storage-element-endian ce))
                          (= (%element-operand-cells pe) (%element-operand-cells ce)))
               (fail "memory ~S changes its :cell-width, :endian or operand cell count" name))))))
      (dolist (kind '(:memory :stack))
        (unless (= (count kind (machine-descriptor-elements parent) :key #'storage-element-kind)
                   (count kind (machine-descriptor-elements child) :key #'storage-element-kind))
          (fail "the number of ~(~A~) elements changes" kind)))
      (let ((pp (machine-descriptor-stack-pointers parent))
            (cp (machine-descriptor-stack-pointers child)))
        (unless (= (hash-table-count pp) (hash-table-count cp))
          (fail "the stack-pointer set differs"))
        (maphash (lambda (reg psp)
                   (let ((csp (gethash reg cp)))
                     (unless (and csp
                                  (eq (stack-pointer-descriptor-memory psp)
                                      (stack-pointer-descriptor-memory csp))
                                  (eq (stack-pointer-descriptor-grows psp)
                                      (stack-pointer-descriptor-grows csp))
                                  (eql (stack-pointer-descriptor-width psp)
                                       (stack-pointer-descriptor-width csp))
                                  (equal (stack-pointer-descriptor-bounds psp)
                                         (stack-pointer-descriptor-bounds csp)))
                       (fail "stack-pointer ~S differs" reg))))
                 pp))
      (let ((pp (machine-descriptor-privilege parent))
            (cp (machine-descriptor-privilege child)))
        (when pp
          (unless (and cp
                       (eq (privilege-descriptor-level pp) (privilege-descriptor-level cp))
                       (eql (privilege-descriptor-shift pp) (privilege-descriptor-shift cp))
                       (eql (privilege-descriptor-width pp) (privilege-descriptor-width cp))
                       (equal (privilege-descriptor-levels pp) (privilege-descriptor-levels cp))
                       (equal (privilege-descriptor-values pp) (privilege-descriptor-values cp)))
            (fail "the privilege level, field, levels or values differ")))
        (dolist (parent-element (machine-descriptor-elements parent))
          (let ((child-element (gethash (storage-element-name parent-element)
                                        (machine-descriptor-table child))))
            (when (and child-element
                       (not (and (eq (storage-element-read-privilege parent-element)
                                     (storage-element-read-privilege child-element))
                                 (eq (storage-element-write-privilege parent-element)
                                     (storage-element-write-privilege child-element))
                                 (equal (storage-element-field-privileges parent-element)
                                        (storage-element-field-privileges child-element)))))
              (fail "~S changes the inherited :privilege of ~S"
                    (machine-descriptor-name child) (storage-element-name parent-element))))))
      (let ((pi* (machine-descriptor-interrupts parent))
            (ci (machine-descriptor-interrupts child)))
        (when (and pi* ci)
          (unless (and (equal (interrupt-descriptor-save pi*) (interrupt-descriptor-save ci))
                       (eq (interrupt-descriptor-stack-name pi*) (interrupt-descriptor-stack-name ci))
                       (eq (interrupt-descriptor-stack-kind pi*) (interrupt-descriptor-stack-kind ci)))
            (fail "the interrupt :save list or stack differs")))))))

(defun %mnemonic-key (designator)
  (string-upcase (string designator)))

(defun %define-machine (name parent clauses)
  "Build and register machine NAME. With PARENT, CLAUSES merge over PARENT's
and the parent's instructions are copied in."
  (%with-definition (name machine-definition-error)
    (if (null parent)
        (setf (gethash name *machines*) (build-machine-descriptor name clauses))
        (let ((parent-md (or (gethash parent *machines*)
                             (%defmachine-error "Machine ~S extends ~S, which has not been defined" name parent))))
          (when (or (eq name parent) (member name (%machine-ancestors parent)))
            (%defmachine-error "Machine ~S cannot extend ~S: that would form a cycle" name parent))
          (let* ((removals (loop for c in clauses when (eq (first c) 'without-instructions)
                                 append (mapcar #'%mnemonic-key (rest c))))
                 (cycles (loop for c in clauses when (eq (first c) 'instruction-cycles)
                               append (mapcar (lambda (entry)
                                                (%definition-bind (mnemonic n) entry
                                                  (unless (and (integerp n) (>= n 0))
                                                    (%defmachine-error "instruction-cycles ~S must be a non-negative integer, got ~S"
                                                           mnemonic n))
                                                  (cons (%mnemonic-key mnemonic) n)))
                                              (rest c))))
                 (removed-storage (loop for c in clauses when (eq (first c) 'without-storage)
                                        append (rest c)))
                 (removed-devices (loop for c in clauses when (eq (first c) 'without-devices)
                                        append (rest c)))
                 (plain (remove-if (lambda (c) (member (first c) '(without-instructions instruction-cycles
                                                                   without-storage without-devices)))
                                   clauses))
                 (child (build-machine-descriptor
                         name (%remove-machine-clauses
                               name
                               (%merge-machine-clauses (machine-descriptor-source-clauses parent-md) plain)
                               plain removed-storage removed-devices))))
            (dolist (key (append removals (mapcar #'car cycles)))
              (unless (gethash key (machine-descriptor-instructions parent-md))
                (warn 'simple-style-warning :format-control "Machine ~S: ~A is not an instruction of ~S"
                   :format-arguments (list name key parent))))
            (dolist (entry cycles)
              (when (member (car entry) removals :test #'string=)
                (%defmachine-error "Machine ~S: ~A is both removed and given a cycle cost" name (car entry))))
            (setf (machine-descriptor-removed-storage child)
                  (remove-duplicates (copy-list removed-storage)))
            (%check-inheritance-compatible parent-md child)
            (setf (machine-descriptor-parent child) parent
                  (machine-descriptor-removed-instructions child)
                  (remove-duplicates (append removals (machine-descriptor-removed-instructions parent-md))
                                     :test #'string=)
                  (machine-descriptor-instruction-cycles child) cycles)
            (%inherit-instructions parent-md child)
            (setf (gethash name *machines*) child))))))

(defun %parse-machine-name (name-spec)
  "Values NAME and PARENT from a DEFMACHINE name or (NAME (:extends PARENT))."
  (%with-definition ((if (consp name-spec) (car name-spec) name-spec) machine-definition-error)
    (if (symbolp name-spec)
        (values name-spec nil)
        (%definition-bind (name &rest options) name-spec
          (let (parent parent-seen)
            (dolist (option options)
              (unless (and (consp option) (eq (first option) :extends) (= (length option) 2)
                           (symbolp (second option)))
                (%defmachine-error "DEFMACHINE ~S: unknown name option ~S; expected (:extends PARENT)"
                       name option))
              (when parent-seen
                (%defmachine-error "DEFMACHINE ~S: more than one :extends option" name))
              (setf parent-seen t parent (second option)))
            (values name parent))))))

(defmacro defmachine (name &body clauses)
  "Define a fantasy-CPU storage model named NAME from CLAUSES, each one of:
     (register NAME :width n [:count n] [:names (A B C ...)])
     (stack NAME :width n :depth n)
     (memory NAME :width n :addr-width n [:cell-width n] [:endian :little/:big]
       (region NAME start end [:kind :ram/:rom/:device] [:banks n]
                              [:on-write :ignore/:error] [:read fn] [:write fn])...)
     (flags NAME...)
     (instruction-word :width n (field NAME width)...)
     (clock-speed n)
     (reset-pc n)
     (device NAME [:id n] [:version n] [:manufacturer n]
             [:init fn] [:tick fn] [:receive fn] [:detach fn]
             [:priority n] [:non-maskable t/nil])
     (stack-pointer REGISTER [:memory name] [:grows :down/:up] [:width n] [:bounds (low high)])
     (interrupts :vector reg :message reg :save (name...)
                 [:nmi-vector reg] [:stack name] [:queue n] [:on-overflow policy]
                 [:mask-when fn] [:mask-flag name] [:mask-level place]
                 [:mask-level-when fn] [:mask-level-on-deliver t/nil] [:cycles n]
                 [:drop-on-zero-vector t/nil] [:mask-on-deliver t/nil]
                 [:nesting :allow/:priority] [:max-depth n] [:deliver-level level])
     (undefined-opcode :fault/:nop/:trap)
     (properties :key value...)

NAME may be (NAME (:extends PARENT)): the machine then inherits PARENT's
clauses and instructions. A clause naming an existing register, stack, memory
or device merges its keywords over the parent's; interrupts and properties
merge key by key; clock-speed, reset-pc and undefined-opcode replace; flags
add. Further clauses are valid only on an extending machine:
     (without-instructions MNEMONIC...)
     (instruction-cycles (MNEMONIC n)...)
     (without-storage NAME...)    ; registers and flags
     (without-devices NAME...)    ; later devices' bus indices shift down
See docs/machine-families.md.

A register's :names (#72) gives each bank cell of a banked (:count > 1)
register a symbolic alias -- e.g. CHIP8's V0-VF or DCPU-16's A/B/C/X/Y/Z/I/J
-- resolving to that cell's index. :count defaults to (length names) when
:names is given alone. An alias folds like a plain symbol in assembly source
(EVAL-EXPR, instruction.lisp) and binds as a symbol-macro over REGREF inside
semantics (WITH-MACHINE-BINDINGS, semantics.lisp). Every alias shares one
machine-wide namespace with every storage element name -- see MACHINE-
DESCRIPTOR-REGISTER-ALIASES (storage.lisp).

A memory element's :REGION forms (#107) declare sub-ranges with distinct
access behavior -- :RAM (the default, ordinary storage), :ROM (writes
dropped or, with :ON-WRITE :ERROR, signal MEMORY-WRITE-PROTECTED), and
:DEVICE (reads/writes forwarded to :READ/:WRITE instead of touching backing
storage). MREF/(SETF MREF) (storage.lisp) route through whichever region an
address falls in; MPEEK reads backing storage directly, bypassing a
:DEVICE region's :READ, for inspection paths that must not trigger device
side effects. A region name shares the same machine-wide namespace as every
other storage element name and register alias.

A :RAM or :ROM region's :BANKS n gives it n separate banks of storage, one
mapped in at a time (bank 0 after MAKE-MACHINE and RESET). CURRENT-BANK and
its SETF switch banks from host code or a :DEVICE region's :WRITE; SET-BANK!
does the same inside instruction semantics. BANK-PEEK reads or writes any
bank, mapped or not. See docs/machine-model.md.

A :DEVICE region's :READ/:WRITE are function designators -- write the bare
function name (e.g. :READ MY-DEVICE-READ), not #'MY-DEVICE-READ: DEFMACHINE
quotes its whole clause body, so a #'-form there would freeze to the literal
list (FUNCTION MY-DEVICE-READ) instead of an actual function. A bare symbol
survives quoting unevaluated and FUNCALL resolves it at call time, which
also means the function need not be defined yet at DEFMACHINE time, only by
first access.

INSTRUCTION-WORD (#20, M4) declares a fixed-width instruction word split into
named bit fields (MSB-first, one of them named OPCODE) instead of the
default opcode-byte-plus-operand-bytes encoding -- see DEFINSTRUCTION's
(operand NAME :field F (variant ...)) clause for how an instruction fills
those fields. Optional; a machine with no such clause keeps the default
byte encoding.

An optional (layout NAME (field name width)...) form (#64) declares an
alternate field split for a subset of the machine's opcodes -- every layout
shares :WIDTH and an identical OPCODE field, and a DEFINSTRUCTION names which
one it encodes against via a (layout NAME) encoding subclause. Lets one
machine express per-instruction non-uniform word layouts, e.g. a CHIP8-shaped
16-bit word whose opcode nibble alone decides whether the rest splits 4/12,
4/4/8, or 4/4/4/4.

A memory element's :ENDIAN (#66) declares which end of a multi-cell value
its low-order cell occupies -- :LITTLE (the default) or :BIG. Governs
%ENCODE-VALUE-CELLS/%FETCH-CELLS (instruction.lisp/decoder.lisp), so an
instruction operand, an INSTRUCTION-WORD's own encoded word, and a
.BYTE/.WORD directive's data all lay their cells down the same way. Several
memory elements may declare different endianness, same as :CELL-WIDTH; an
operation ambiguous about which one it means (no explicit :MEMORY, and the
elements disagree) signals the same way %MACHINE-CELL-WIDTH's own ambiguity
does.

CLOCK-SPEED (#75) declares the machine's nominal rate in Hz, used by
RUN-FOR-DURATION (emulator.lisp) to convert accumulated cycles to
wall-time-equivalent seconds. Optional; a machine with no such clause can
still use RUN-FOR-CYCLES and read MACHINE-CYCLES, just not RUN-FOR-DURATION.

RESET-PC (#226) declares the value MAKE-MACHINE and RESET give the PC register
instead of zero. Optional; the machine must have a single register named PC
that the value fits. LOAD-PROGRAM still sets PC to the load origin.

DEVICE (#108) declares a bus-addressed peripheral -- identity (an ID/
VERSION/MANUFACTURER triple, an HWQ-style instruction's own semantics decide
which registers it lands in) plus optional INIT/TICK/RECEIVE/DETACH hooks,
each a function designator for the same reason a :DEVICE region's :READ/
:WRITE are (see above). Independent of any memory region -- a machine can
declare a device without declaring any MMIO region at all, or the reverse.
Every declared device gets a fixed bus index in declaration order; a device
may also be attached at runtime (ATTACH-DEVICE, device.lisp), appended after
every declared one. See docs/devices.md for the full bus API (DEVICE-COUNT,
DEVICE-INFO, DEVICE-SEND, DETACH-DEVICE) -- deliberately not bound inside
WITH-MACHINE-BINDINGS, the same way :MEMORY elements aren't; an instruction's
semantics call these directly with MACHINE, same as MREF.

STACK-POINTER (#166) binds an existing scalar REGISTER as an address pointer
into a :MEMORY element, for machines (DCPU-16, ANIMA-16) whose stack is a
plain register indexed by push/pop convention rather than a lasm (stack ...)
element. :MEMORY defaults to the machine's sole declared memory element (an
error if it declares none or more than one). :GROWS (default :DOWN) picks
the convention: :DOWN has REGISTER point AT the top item -- push
pre-decrements then stores, pop loads then post-increments; :UP has it point
one PAST the top item -- push stores then post-increments, pop
pre-decrements then loads. PUSH/POP (semantics.lisp) and an (interrupts ...)
clause's :STACK both accept a stack-pointer register wherever they accept a
(stack ...) element's name. :WIDTH (default :MEMORY's cell width) is the
bits per push/pop/ref slot; a wider slot spans several cells in :MEMORY's
own endianness (#167). :BOUNDS (LOW HIGH) is an inclusive cell-address
window outside which a push, pop or ref signals STACK-OVERFLOW,
STACK-UNDERFLOW or STACK-INDEX-OUT-OF-RANGE before anything changes
(#168); without it there is no condition -- a wrapping register is the
machine's own business, same as the hardware it models. The indexed
address is masked to :MEMORY's :ADDR-WIDTH so REGISTER may be wider than
the address space.

INTERRUPTS (#109) declares the machine's interrupt-delivery model:
:VECTOR names the register holding the handler address, written to PC on
delivery (:NMI-VECTOR, #311, names a second one used for non-maskable
signals, and is what :DROP-ON-ZERO-VECTOR checks for them);
:MESSAGE names the register a delivered signal's data is written to; :SAVE names the registers/flags pushed, in order, before MESSAGE/VECTOR
are written -- INTERRUPT-RETURN (semantics.lisp) pops them in reverse.
:STACK names which declared stack SAVE pushes onto -- a (stack ...) element
or a (stack-pointer ...)-bound register -- defaulting to the machine's sole
:stack element, or (with none declared) its sole stack-pointer (an error on
zero or more than one candidate of whichever kind applies). A :STACK naming
a stack-pointer saves each place at its own width, spanning several
cells when it is wider than the memory's cell width.
:QUEUE (default 256) caps the number of pending, undelivered signals;
:ON-OVERFLOW (default :ERROR) picks what SIGNAL-INTERRUPT (interrupt.lisp)
does when a signal arrives past that cap -- :ERROR signals INTERRUPT-
QUEUE-FULL, :TRAP signals LASM-TRAP, :DROP discards the incoming signal,
:DROP-OLDEST evicts the queue's head first. :MASK-WHEN (a function
designator, (machine) -> generalized boolean) or :MASK-FLAG (a flag name)
-- at most one -- gates whether a *queued* signal is delivered this step;
masking never blocks enqueueing. :CYCLES (default 0) is delivery's own
extra MACHINE-CYCLES cost. :DROP-ON-ZERO-VECTOR (default T) drops a signal
outright, before it ever reaches the queue, while :VECTOR's register
currently reads 0 -- the DCPU-16/ANIMA-16 convention where a zero vector
means interrupts are off; set it NIL on a machine whose handler legitimately lives
at address 0. See docs/interrupts.md for the full delivery/masking/
overflow model and SIGNAL-INTERRUPT/INTERRUPT-RETURN.

Registration happens inside an EVAL-WHEN so the resulting machine-descriptor
is available at macroexpansion time, not only after this file is loaded --
required for M1's DEFINSTRUCTION to resolve storage names/widths against a
DEFMACHINE appearing earlier in the same file."
  (multiple-value-bind (machine-name parent) (%parse-machine-name name)
    (%definition-toplevel-form `(%define-machine ',machine-name ',parent ',clauses)
                               `',machine-name)))
