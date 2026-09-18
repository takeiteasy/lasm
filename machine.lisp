;;;; machine.lisp
;;;; The DEFMACHINE macro: parses storage clauses into a machine-descriptor.

(in-package #:lasm)

(defun %check-positive (value what name)
  (unless (and (integerp value) (plusp value))
    (error "~A for ~S must be a positive integer, got ~S" what name value))
  value)

(defun %check-endian (value name)
  "#66: VALUE must be :LITTLE or :BIG -- the only two cell orderings
%ENCODE-VALUE-CELLS/%FETCH-CELLS (instruction.lisp/decoder.lisp) know how to
lay a multi-cell value down in."
  (unless (member value '(:little :big))
    (error "memory ~S :endian must be :LITTLE or :BIG, got ~S" name value))
  value)

(defun parse-register-clause (name-form)
  ;; (register NAME :width n [:count n] [:names (A B C ...)]) -- #72: NAMES is
  ;; an optional list of alias symbols, one per bank cell in index order
  ;; (CHIP8's V0-VF, DCPU-16's A/B/C/X/Y/Z/I/J). COUNT defaults to (length
  ;; NAMES) when NAMES is given and COUNT is not; when both are given they
  ;; must agree, since a mismatched pair almost certainly indicates a typo
  ;; in one or the other rather than an intentional partial naming.
  (destructuring-bind (name &key width count names) name-form
    (unless width (error "register ~S requires :width" name))
    (when names
      (unless (every #'symbolp names)
        (error "register ~S :names must be a list of symbols, got ~S" name names))
      (let ((dup (loop for (n . rest) on names
                        when (member n rest :test #'string-equal) return n)))
        (when dup
          (error "register ~S :names: duplicate alias ~S" name dup)))
      (if count
          (unless (= count (length names))
            (error "register ~S: :count ~D disagrees with :names' length ~D"
                   name count (length names)))
          (setf count (length names))))
    (setf count (or count 1))
    (make-storage-element :name name :kind :register
                           :width (%check-positive width ":width" name)
                           :count (%check-positive count ":count" name)
                           :names names)))

(defun parse-stack-clause (form)
  ;; (stack NAME :width n :depth n)
  (destructuring-bind (name &key width depth) form
    (unless width (error "stack ~S requires :width" name))
    (unless depth (error "stack ~S requires :depth" name))
    (make-storage-element :name name :kind :stack
                           :width (%check-positive width ":width" name)
                           :depth (%check-positive depth ":depth" name))))

;; #166: (stack-pointer REGISTER [:memory NAME] [:grows :down/:up]) -- binds
;; an existing scalar :register element as an address pointer into a :memory
;; element, for machines whose "stack" is a plain register indexed by
;; push/pop convention rather than a lasm :stack element (DCPU-16, ANIMA-16).
;; Like (interrupts ...), this clause introduces no new namespace name -- it
;; only references existing ones -- so it's parsed shape-only here; whether
;; REGISTER/MEMORY actually name the right kind of element can't be checked
;; until every other clause is known (%FINISH-STACK-POINTERS, below).
(defun parse-stack-pointer-clause (form)
  (destructuring-bind (register &key memory (grows :down)) form
    (unless (symbolp register)
      (error "stack-pointer ~S must be a symbol" register))
    (when (and memory (not (symbolp memory)))
      (error "stack-pointer ~S :memory must be a symbol, got ~S" register memory))
    (unless (member grows '(:down :up))
      (error "stack-pointer ~S :grows must be :DOWN or :UP, got ~S" register grows))
    (make-stack-pointer-descriptor :register register :memory memory :grows grows)))

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
          (error "stack-pointer on machine ~S: ~S is not a declared register element"
                 name reg))
        (when (> (storage-element-count element) 1)
          (error "stack-pointer on machine ~S: ~S is a banked (:count > 1) register -- ~
only a scalar register may be a stack pointer" name reg))
        (when (gethash reg (machine-descriptor-stack-pointers descriptor))
          (error "stack-pointer on machine ~S: more than one (stack-pointer ~S ...) clause"
                 name reg))
        (let ((given (stack-pointer-descriptor-memory sp)))
          (if given
              (let ((mem (gethash given (machine-descriptor-table descriptor))))
                (unless (and mem (eq (storage-element-kind mem) :memory))
                  (error "stack-pointer ~S on machine ~S: :memory ~S is not a declared ~
memory element" reg name given)))
              (let ((memories (remove-if-not (lambda (e) (eq (storage-element-kind e) :memory))
                                              (machine-descriptor-elements descriptor))))
                (cond
                  ((= (length memories) 1)
                   (setf (stack-pointer-descriptor-memory sp) (storage-element-name (first memories))))
                  ((null memories)
                   (error "stack-pointer ~S on machine ~S: no memory element is declared -- ~
name one explicitly with :memory" reg name))
                  (t (error "stack-pointer ~S on machine ~S: more than one memory element ~
declared (~{~S~^ ~}) -- name one explicitly with :memory"
                            reg name (mapcar #'storage-element-name memories)))))))
        (setf (gethash reg (machine-descriptor-stack-pointers descriptor)) sp)))))

;; #107: (region NAME start end [:kind :ram/:rom/:device] [:on-write
;; :ignore/:error] [:read fn] [:write fn]) -- one sub-range of a memory
;; element with distinct access behavior. NAME is validated as a symbol
;; here; PARSE-MEMORY-CLAUSE cross-checks it against every other name in the
;; machine's namespace (BUILD-MACHINE-DESCRIPTOR's SEEN table) once the whole
;; clause is parsed, same as a register's #72 :NAMES aliases. START/END are
;; both inclusive; validated against ADDR-WIDTH by PARSE-MEMORY-CLAUSE, which
;; alone knows the element's address range.
(defun %parse-memory-region-form (form context)
  (destructuring-bind (head name start end &key (kind :ram) (on-write :ignore) read write) form
    (unless (eq head 'region)
      (error "~A: expected (region name start end ...), got ~S" context form))
    (unless (symbolp name)
      (error "~A: region name must be a symbol, got ~S" context name))
    (unless (and (integerp start) (>= start 0))
      (error "~A region ~S: start must be a non-negative integer, got ~S" context name start))
    (unless (and (integerp end) (>= end 0))
      (error "~A region ~S: end must be a non-negative integer, got ~S" context name end))
    (unless (<= start end)
      (error "~A region ~S: start ~D must not be greater than end ~D" context name start end))
    (unless (member kind '(:ram :rom :device))
      (error "~A region ~S: :kind must be :RAM, :ROM or :DEVICE, got ~S" context name kind))
    (unless (member on-write '(:ignore :error))
      (error "~A region ~S: :on-write must be :IGNORE or :ERROR, got ~S" context name on-write))
    (unless (or (eq on-write :ignore) (eq kind :rom))
      (error "~A region ~S: :on-write only applies to a :ROM region" context name))
    (when (and (or read write) (not (eq kind :device)))
      (error "~A region ~S: :read/:write only apply to a :DEVICE region" context name))
    (dolist (fn (list (cons :read read) (cons :write write)))
      (when (and (cdr fn) (not (or (symbolp (cdr fn)) (functionp (cdr fn)))))
        (error "~A region ~S: ~A must be a function designator (a symbol or a ~
function), got ~S" context name (car fn) (cdr fn))))
    (make-memory-region :name name :start start :end end :kind kind
                         :on-write on-write :read read :write write)))

;; Cross-region checks (#107): unique names and non-overlapping ranges,
;; applied once every (region ...) form in the clause is parsed -- mirrors
;; PARSE-INSTRUCTION-WORD-CLAUSE's own cross-layout checks after parsing
;; every (layout ...) form.
(defun %check-memory-regions (regions context)
  (loop for (region . rest) on regions
        do (when (member (memory-region-name region) rest :key #'memory-region-name)
             (error "~A: duplicate region name ~S" context (memory-region-name region)))
           (dolist (other rest)
             (when (and (<= (memory-region-start region) (memory-region-end other))
                        (<= (memory-region-start other) (memory-region-end region)))
               (error "~A: region ~S (~D-~D) overlaps region ~S (~D-~D)"
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
    (destructuring-bind (name &key width addr-width cell-width (endian :little)) plist-forms
      (unless width (error "memory ~S requires :width" name))
      (unless addr-width (error "memory ~S requires :addr-width" name))
      (%check-positive width ":width" name)
      (%check-positive addr-width ":addr-width" name)
      (let* ((context (format nil "memory ~S" name))
             (max-address (1- (ash 1 addr-width)))
             (regions (%check-memory-regions
                       (mapcar (lambda (f) (%parse-memory-region-form f context)) region-forms)
                       context)))
        (dolist (r regions)
          (when (> (memory-region-end r) max-address)
            (error "~A region ~S: end ~D is outside the element's address range 0-~D"
                   context (memory-region-name r) (memory-region-end r) max-address)))
        (make-storage-element :name name :kind :memory
                               :width width
                               :addr-width addr-width
                               :cell-width (%check-positive (or cell-width width) ":cell-width" name)
                               :endian (%check-endian endian name)
                               :regions regions)))))

(defun parse-flags-clause (form)
  ;; (flags A B C ...) -- expands to one storage-element per flag, width 1
  (loop for name in form
        collect (make-storage-element :name name :kind :flag :width 1)))

;; #75: (clock-speed n) -- the machine's nominal rate in Hz, n a positive
;; integer. Optional; a machine with no such clause leaves MACHINE-
;; DESCRIPTOR-CLOCK-SPEED NIL (storage.lisp), which is what keeps
;; RUN-FOR-DURATION's wall-time-equivalent conversion opt-in rather than
;; forcing every machine to declare a rate it doesn't care about.
(defun parse-clock-speed-clause (form)
  ;; (clock-speed n)
  (destructuring-bind (hz) form
    (%check-positive hz ":clock-speed" 'clock-speed)))

;; #108: (device NAME [:id n] [:version n] [:manufacturer n] [:init fn]
;;   [:tick fn] [:receive fn] [:detach fn] [:save fn] [:load fn]) -- a bus-addressed peripheral,
;; independent of #107's memory regions (a machine can declare one without
;; declaring any MMIO region at all). NAME is validated as a symbol here;
;; BUILD-MACHINE-DESCRIPTOR cross-checks it against every other name in the
;; machine's namespace, same as a region's or a register alias's name.
(defun parse-device-clause (form)
  (destructuring-bind (name &key (id 0) (version 0) (manufacturer 0)
                             init tick receive detach save load)
      form
    (unless (symbolp name)
      (error "device ~S: name must be a symbol" name))
    (dolist (v (list (cons :id id) (cons :version version) (cons :manufacturer manufacturer)))
      (unless (and (integerp (cdr v)) (>= (cdr v) 0))
        (error "device ~S: ~A must be a non-negative integer, got ~S" name (car v) (cdr v))))
    (dolist (fn (list (cons :init init) (cons :tick tick)
                       (cons :receive receive) (cons :detach detach)
                       (cons :save save) (cons :load load)))
      (when (and (cdr fn) (not (or (symbolp (cdr fn)) (functionp (cdr fn)))))
        (error "device ~S: ~A must be a function designator (a symbol or a ~
function), got ~S" name (car fn) (cdr fn))))
    (make-device-descriptor :name name :id id :version version :manufacturer manufacturer
                             :init init :tick tick :receive receive :detach detach
                             :save save :load load)))

;; #109: (interrupts :vector NAME :message NAME :save (NAME...)
;;   [:stack NAME] [:queue n] [:on-overflow policy] [:mask-when fn]
;;   [:mask-flag name] [:cycles n] [:drop-on-zero-vector t/nil]
;;   [:mask-on-deliver t/nil]) -- the
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
  (destructuring-bind (&key vector message save stack (queue 256) (on-overflow :error)
                             mask-when mask-flag (cycles 0) (drop-on-zero-vector t)
                             mask-on-deliver)
      form
    (unless vector (error "interrupts requires :vector"))
    (unless (%interrupt-place-designator-p vector)
      (error "interrupts :vector must be a symbol or (NAME INDEX), got ~S" vector))
    (unless message (error "interrupts requires :message"))
    (unless (%interrupt-place-designator-p message)
      (error "interrupts :message must be a symbol or (NAME INDEX), got ~S" message))
    (unless save (error "interrupts requires :save"))
    (unless (and (listp save) (every #'%interrupt-place-designator-p save))
      (error "interrupts :save must be a list of symbols or (NAME INDEX) places, got ~S" save))
    (when (and stack (not (symbolp stack)))
      (error "interrupts :stack must be a symbol, got ~S" stack))
    (unless (and (integerp queue) (plusp queue))
      (error "interrupts :queue must be a positive integer, got ~S" queue))
    (unless (member on-overflow '(:error :trap :drop :drop-oldest))
      (error "interrupts :on-overflow must be :ERROR, :TRAP, :DROP or :DROP-OLDEST, got ~S"
             on-overflow))
    (when (and mask-when mask-flag)
      (error "interrupts: at most one of :mask-when/:mask-flag may be given"))
    (when (and mask-when (not (or (symbolp mask-when) (functionp mask-when))))
      (error "interrupts :mask-when must be a function designator (a symbol or a ~
function), got ~S" mask-when))
    (when (and mask-flag (not (symbolp mask-flag)))
      (error "interrupts :mask-flag must be a symbol, got ~S" mask-flag))
    (unless (and (integerp cycles) (>= cycles 0))
      (error "interrupts :cycles must be a non-negative integer, got ~S" cycles))
    (when (and mask-on-deliver (not mask-flag))
      (error "interrupts :mask-on-deliver requires :mask-flag"))
    (make-interrupt-descriptor :vector vector :message message :save save :stack-name stack
                                :queue-depth queue :on-overflow on-overflow
                                :mask-when mask-when :mask-flag mask-flag :cycles cycles
                                :drop-on-zero-vector (and drop-on-zero-vector t)
                                :mask-on-deliver (and mask-on-deliver t))))

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
             (error "interrupts on machine ~S: :stack ~S is a register with no ~
(stack-pointer ~S ...) clause declared" name given given))
            (t (error "interrupts on machine ~S: :stack ~S is not a declared stack ~
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
                  (error "interrupts on machine ~S: :save needs a stack, but no stack ~
element or stack-pointer is declared -- name one explicitly with :stack" name))
                 (t (error "interrupts on machine ~S: more than one stack-pointer declared ~
(~{~S~^ ~}) -- name one explicitly with :stack" name pointers)))))
            (t (error "interrupts on machine ~S: more than one stack element declared ~
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
                   (error "interrupts on machine ~S: ~A ~S is not a declared storage element"
                          name what n))
                 (unless (member (storage-element-kind e) kinds)
                   (error "interrupts on machine ~S: ~A ~S must be a ~{~S~^ or a ~} element, ~
got ~S" name what n kinds (storage-element-kind e)))
                 ;; A bare banked register is ambiguous (which bank cell?) and
                 ;; rejected at DEFMACHINE time rather than at first delivery.
                 ;; #163: (NAME INDEX) names one cell of a banked register,
                 ;; read/written through REGREF; a bare banked NAME stays
                 ;; ambiguous and rejected.
                 (if (consp place)
                     (unless (and (eq (storage-element-kind e) :register)
                                  (< -1 (second place) (storage-element-count e)))
                       (error "interrupts on machine ~S: ~A ~S is out of range for register ~S ~
(~D cell~:P)" name what place n (storage-element-count e)))
                     (when (> (storage-element-count e) 1)
                       (error "interrupts on machine ~S: ~A ~S is a banked (:count > 1) register -- ~
name one cell as (~S INDEX), or use a scalar register" name what n n))))))
      (require-kind (interrupt-descriptor-vector interrupts) '(:register) ":vector")
      (require-kind (interrupt-descriptor-message interrupts) '(:register) ":message")
      (dolist (n (interrupt-descriptor-save interrupts))
        (require-kind n '(:register :flag) ":save"))
      (when (interrupt-descriptor-mask-flag interrupts)
        (require-kind (interrupt-descriptor-mask-flag interrupts) '(:flag) ":mask-flag")))
    (multiple-value-bind (stack-name stack-kind)
        (%resolve-interrupt-stack descriptor (interrupt-descriptor-stack-name interrupts))
      (setf (interrupt-descriptor-stack-name interrupts) stack-name)
      (setf (interrupt-descriptor-stack-kind interrupts) stack-kind)
      ;; #166: a :POINTER stack pushes every :SAVE place, un-split, into one
      ;; memory cell (SP-PUSH, storage.lisp) -- a place wider than the bound
      ;; memory's cell width would silently truncate on delivery instead of
      ;; failing loudly here. Splitting a wide place across cells is
      ;; unscoped follow-up work (#166 follow-up).
      (when (eq stack-kind :pointer)
        (let* ((sp (gethash stack-name (machine-descriptor-stack-pointers descriptor)))
               (memory (gethash (stack-pointer-descriptor-memory sp) (machine-descriptor-table descriptor)))
               (cell-width (or (storage-element-cell-width memory) (storage-element-width memory))))
          (dolist (place (interrupt-descriptor-save interrupts))
            (let* ((n (if (consp place) (first place) place))
                   (width (storage-element-width (gethash n (machine-descriptor-table descriptor)))))
              (when (> width cell-width)
                (error "interrupts on machine ~S: :save place ~S is ~D bits wide, too wide ~
for stack-pointer ~S's memory ~S (~D-bit cells)"
                       name place width stack-name (stack-pointer-descriptor-memory sp) cell-width)))))))))

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
    (error "~A requires at least one (field name width) clause" context))
  (let ((seen (make-hash-table :test 'eq))
        (opcode-seen nil)
        (total 0)
        fields)
    (dolist (field-form field-forms)
      (destructuring-bind (head name field-width) field-form
        (unless (eq head 'field)
          (error "~A: expected (field name width), got ~S" context field-form))
        (when (gethash name seen)
          (error "~A: duplicate field name ~S" context name))
        (setf (gethash name seen) t)
        (%check-positive field-width ":width" name)
        (when (eq name 'opcode) (setf opcode-seen t))
        (cl:push (list name field-width) fields)
        (incf total field-width)))
    (unless opcode-seen
      (error "~A requires exactly one field named OPCODE" context))
    (unless (= total width)
      (error "~A: field widths sum to ~D, but :width is ~D" context total width))
    ;; FIELDS was accumulated MSB-first-declared but CL:PUSH-reversed, so
    ;; NREVERSE restores declaration order before computing each field's
    ;; shift from the LSB -- the last-declared field sits at shift 0.
    (setf fields (nreverse fields))
    (let ((shift width))
      (mapcar (lambda (f)
                (destructuring-bind (name field-width) f
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
  (destructuring-bind (head name &rest field-forms) form
    (unless (eq head 'layout)
      (error "instruction-word: expected (layout name (field ...)...), got ~S" form))
    (unless (symbolp name)
      (error "instruction-word: layout name must be a symbol, got ~S" name))
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
         (rest-forms (if width-pos
                         (append (subseq body 0 width-pos) (subseq body (+ width-pos 2)))
                         body))
         ;; #64: (layout ...) forms are the machine's alternates; everything
         ;; else is the default layout's own (field ...) forms.
         (layout-forms (remove-if-not (lambda (f) (eq (first f) 'layout)) rest-forms))
         ;; #191: (extra-word-order FIELD...) -- at most one, default layout only.
         (order-forms (remove-if-not (lambda (f) (eq (first f) 'extra-word-order)) rest-forms))
         (field-forms (remove-if (lambda (f) (member (first f) '(layout extra-word-order))) rest-forms)))
    (unless width (error "instruction-word requires :width"))
    (%check-positive width ":width" 'instruction-word)
    (when (rest order-forms)
      (error "instruction-word: more than one (extra-word-order ...) clause"))
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
                 (error "instruction-word: extra-word-order names ~S, which is not a declared field"
                        (first tail)))
               (when (member (first tail) (rest tail))
                 (error "instruction-word: extra-word-order names ~S twice" (first tail))))
      (let ((names (mapcar #'instruction-word-layout-name alternates)))
        (loop for tail on names
              when (member (first tail) (rest tail))
                do (error "instruction-word: duplicate layout name ~S" (first tail))))
      (dolist (alt alternates)
        (let ((alt-opcode (instruction-word-field alt 'opcode)))
          (unless (equal (rest alt-opcode) (rest opcode-field))
            (error "instruction-word layout ~S: OPCODE field ~S disagrees with the ~
default layout's OPCODE field ~S -- every layout must place OPCODE identically"
                   (instruction-word-layout-name alt) alt-opcode opcode-field))))
      (make-instruction-word-layout
       :name nil
       :width width
       :width-cells 1 ; placeholder -- %FINISH-INSTRUCTION-WORD-LAYOUT sets the real value
       :cell-width 1  ; placeholder
       :fields fields
       :extra-word-order extra-word-order
       :alternates alternates))))

(defun %finish-instruction-word-layout (layout cell-width endian)
  "Fill in LAYOUT's WIDTH-CELLS, CELL-WIDTH and ENDIAN (#66), and recurse into
its ALTERNATES (#64), once the machine's own memory cell width/endianness is
known (BUILD-MACHINE-DESCRIPTOR, after every MEMORY element has been parsed)
-- see PARSE-INSTRUCTION-WORD-CLAUSE's docstring for why this can't happen at
clause-parse time. Signals if the instruction word's bit width isn't a whole
number of cells. Setting ENDIAN here (rather than resolving it per-decode)
means %DECODE-WORD-INSTRUCTION (decoder.lisp) needs no extra lookup."
  (let ((width (instruction-word-layout-width layout)))
    (unless (zerop (mod width cell-width))
      (error "instruction-word :width ~D must be a whole number of ~D-bit cells"
             width cell-width))
    (setf (instruction-word-layout-width-cells layout) (/ width cell-width)
          (instruction-word-layout-cell-width layout) cell-width
          (instruction-word-layout-endian layout) endian))
  (dolist (alt (instruction-word-layout-alternates layout))
    (%finish-instruction-word-layout alt cell-width endian))
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
           (error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
          ((> (length mem-elements) 1)
           (error "Machine ~S: more than one memory element declared (~S) -- ~
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
       (error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
      ((null (rest mem-elements))
       (storage-element-cell-width (first mem-elements)))
      (t (let ((widths (remove-duplicates (mapcar #'storage-element-cell-width mem-elements))))
           (if (null (rest widths))
               (first widths)
               (error "Machine ~S: more than one memory element declared with ~
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
       (error "Machine ~S: no memory element declared" (machine-descriptor-name descriptor)))
      ((null (rest mem-elements))
       (storage-element-endian (first mem-elements)))
      (t (let ((endians (remove-duplicates (mapcar #'storage-element-endian mem-elements))))
           (if (null (rest endians))
               (first endians)
               (error "Machine ~S: more than one memory element declared with ~
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

(defun parse-machine-clauses (clauses)
  (let (elements instruction-word clock-speed devices interrupts stack-pointers)
    (dolist (clause clauses)
      (case (first clause)
        (register (cl:push (parse-register-clause (rest clause)) elements))
        (stack (cl:push (parse-stack-clause (rest clause)) elements))
        (memory (cl:push (parse-memory-clause (rest clause)) elements))
        (flags (dolist (e (parse-flags-clause (rest clause))) (cl:push e elements)))
        (instruction-word
         (when instruction-word
           (error "DEFMACHINE: more than one instruction-word clause"))
         (setf instruction-word (parse-instruction-word-clause clause)))
        (clock-speed
         (when clock-speed
           (error "DEFMACHINE: more than one clock-speed clause"))
         (setf clock-speed (parse-clock-speed-clause (rest clause))))
        (device (cl:push (parse-device-clause (rest clause)) devices))
        (interrupts
         (when interrupts
           (error "DEFMACHINE: more than one interrupts clause"))
         (setf interrupts (parse-interrupts-clause (rest clause))))
        (stack-pointer (cl:push (parse-stack-pointer-clause (rest clause)) stack-pointers))
        (t (error "Unknown DEFMACHINE clause head ~S in ~S" (first clause) clause))))
    (values (nreverse elements) instruction-word clock-speed (nreverse devices) interrupts
            (nreverse stack-pointers))))

(defun build-machine-descriptor (name clauses)
  (multiple-value-bind (elements instruction-word clock-speed devices interrupts stack-pointers)
      (parse-machine-clauses clauses)
    (let ((descriptor (make-machine-descriptor :name name :instruction-word instruction-word
                                                :clock-speed clock-speed :devices devices
                                                :interrupts interrupts))
          (seen (make-hash-table :test 'eq)))
      (dolist (element elements)
        (when (gethash (storage-element-name element) seen)
          (error "Duplicate storage element name ~S in machine ~S"
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
                   (error "Duplicate storage element name ~S in machine ~S" alias name))
                 (setf (gethash alias seen) t)
                 (setf (gethash (symbol-name alias) (machine-descriptor-register-aliases descriptor))
                       index))
        ;; #107: a memory element's region names share the same namespace too
        ;; -- SEEN also catches a region colliding with an element name, a
        ;; register alias, or another region, e.g. (region ram ...) inside
        ;; (memory ram ...) itself.
        (dolist (region (storage-element-regions element))
          (when (gethash (memory-region-name region) seen)
            (error "Duplicate storage element name ~S in machine ~S" (memory-region-name region) name))
          (setf (gethash (memory-region-name region) seen) t)))
      ;; #108: a declared device's name joins the same namespace -- SEEN also
      ;; catches a device colliding with an element name, register alias, or
      ;; region name, and two devices sharing a name.
      (dolist (device-descriptor devices)
        (when (gethash (device-descriptor-name device-descriptor) seen)
          (error "Duplicate storage element name ~S in machine ~S"
                 (device-descriptor-name device-descriptor) name))
        (setf (gethash (device-descriptor-name device-descriptor) seen) t))
      (setf (machine-descriptor-elements descriptor) elements)
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
                                          (%descriptor-endian descriptor)))
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
      descriptor)))

(defmacro defmachine (name &body clauses)
  "Define a fantasy-CPU storage model named NAME from CLAUSES, each one of:
     (register NAME :width n [:count n] [:names (A B C ...)])
     (stack NAME :width n :depth n)
     (memory NAME :width n :addr-width n [:cell-width n] [:endian :little/:big]
       (region NAME start end [:kind :ram/:rom/:device]
                              [:on-write :ignore/:error] [:read fn] [:write fn])...)
     (flags NAME...)
     (instruction-word :width n (field NAME width)...)
     (clock-speed n)
     (device NAME [:id n] [:version n] [:manufacturer n]
             [:init fn] [:tick fn] [:receive fn] [:detach fn])
     (stack-pointer REGISTER [:memory name] [:grows :down/:up])
     (interrupts :vector reg :message reg :save (name...)
                 [:stack name] [:queue n] [:on-overflow policy]
                 [:mask-when fn] [:mask-flag name] [:cycles n]
                 [:drop-on-zero-vector t/nil] [:mask-on-deliver t/nil])

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
(stack ...) element's name; there is no overflow/underflow condition -- a
wrapping register is the machine's own business, same as the hardware it
models, and the indexed address is masked to :MEMORY's :ADDR-WIDTH so
REGISTER may be wider than the address space.

INTERRUPTS (#109) declares the machine's interrupt-delivery model:
:VECTOR names the register holding the handler address, written to PC on
delivery; :MESSAGE names the register a delivered signal's data is written
to; :SAVE names the registers/flags pushed, in order, before MESSAGE/VECTOR
are written -- INTERRUPT-RETURN (semantics.lisp) pops them in reverse.
:STACK names which declared stack SAVE pushes onto -- a (stack ...) element
or a (stack-pointer ...)-bound register -- defaulting to the machine's sole
:stack element, or (with none declared) its sole stack-pointer (an error on
zero or more than one candidate of whichever kind applies). A :STACK naming
a stack-pointer additionally requires every :SAVE place to fit the bound
memory's cell width.
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *machines*)
           (build-machine-descriptor ',name ',clauses))
     ',name))
