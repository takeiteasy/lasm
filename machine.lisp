;;;; machine.lisp
;;;; The DEFMACHINE macro: parses storage clauses into a machine-descriptor.

(in-package #:lasm)

(defun %check-positive (value what name)
  (unless (and (integerp value) (plusp value))
    (error "~A for ~S must be a positive integer, got ~S" what name value))
  value)

(defun parse-register-clause (name-form)
  ;; (register NAME :width n [:count n])
  (destructuring-bind (name &key width (count 1)) name-form
    (unless width (error "register ~S requires :width" name))
    (make-storage-element :name name :kind :register
                           :width (%check-positive width ":width" name)
                           :count (%check-positive count ":count" name))))

(defun parse-stack-clause (form)
  ;; (stack NAME :width n :depth n)
  (destructuring-bind (name &key width depth) form
    (unless width (error "stack ~S requires :width" name))
    (unless depth (error "stack ~S requires :depth" name))
    (make-storage-element :name name :kind :stack
                           :width (%check-positive width ":width" name)
                           :depth (%check-positive depth ":depth" name))))

(defun parse-memory-clause (form)
  ;; (memory NAME :width n :addr-width n [:cell-width n])
  (destructuring-bind (name &key width addr-width cell-width) form
    (unless width (error "memory ~S requires :width" name))
    (unless addr-width (error "memory ~S requires :addr-width" name))
    (make-storage-element :name name :kind :memory
                           :width (%check-positive width ":width" name)
                           :addr-width (%check-positive addr-width ":addr-width" name)
                           :cell-width (%check-positive (or cell-width width) ":cell-width" name))))

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
(defun parse-instruction-word-clause (form)
  (let* ((body (rest form))
         (width-pos (position :width body))
         (width (and width-pos (nth (1+ width-pos) body)))
         (field-forms (if width-pos
                           (append (subseq body 0 width-pos) (subseq body (+ width-pos 2)))
                           body)))
    (unless width (error "instruction-word requires :width"))
    (%check-positive width ":width" 'instruction-word)
    (unless field-forms
      (error "instruction-word requires at least one (field name width) clause"))
    (let ((seen (make-hash-table :test 'eq))
          (opcode-seen nil)
          (total 0)
          fields)
      (dolist (field-form field-forms)
        (destructuring-bind (head name field-width) field-form
          (unless (eq head 'field)
            (error "instruction-word: expected (field name width), got ~S" field-form))
          (when (gethash name seen)
            (error "instruction-word: duplicate field name ~S" name))
          (setf (gethash name seen) t)
          (%check-positive field-width ":width" name)
          (when (eq name 'opcode) (setf opcode-seen t))
          (cl:push (list name field-width) fields)
          (incf total field-width)))
      (unless opcode-seen
        (error "instruction-word requires exactly one field named OPCODE"))
      (unless (= total width)
        (error "instruction-word: field widths sum to ~D, but :width is ~D" total width))
      ;; FIELDS was accumulated MSB-first-declared but CL:PUSH-reversed, so
      ;; NREVERSE restores declaration order before computing each field's
      ;; shift from the LSB -- the last-declared field sits at shift 0.
      (setf fields (nreverse fields))
      (let ((shift width))
        (make-instruction-word-layout
         :width width
         :width-cells 1 ; placeholder -- %FINISH-INSTRUCTION-WORD-LAYOUT sets the real value
         :cell-width 1  ; placeholder
         :fields (mapcar (lambda (f)
                            (destructuring-bind (name field-width) f
                              (decf shift field-width)
                              (list name field-width shift)))
                          fields))))))

(defun %finish-instruction-word-layout (layout cell-width)
  "Fill in LAYOUT's WIDTH-CELLS and CELL-WIDTH once the machine's own memory
cell width is known (BUILD-MACHINE-DESCRIPTOR, after every MEMORY element has
been parsed) -- see PARSE-INSTRUCTION-WORD-CLAUSE's docstring for why this
can't happen at clause-parse time. Signals if the instruction word's bit
width isn't a whole number of cells."
  (let ((width (instruction-word-layout-width layout)))
    (unless (zerop (mod width cell-width))
      (error "instruction-word :width ~D must be a whole number of ~D-bit cells"
             width cell-width))
    (setf (instruction-word-layout-width-cells layout) (/ width cell-width)
          (instruction-word-layout-cell-width layout) cell-width))
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

(defun %descriptor-cell-width (descriptor &optional memory-name)
  "DESCRIPTOR's code cell width in bits (#53): MEMORY-NAME's own CELL-WIDTH
when given, else the sole memory element's. When DESCRIPTOR declares more
than one memory element and MEMORY-NAME isn't given, this only succeeds if
every element's cell width agrees -- otherwise the caller must specify which
memory element it means, same as %DESCRIPTOR-RESOLVE-MEMORY's own ambiguity
error. See %DESCRIPTOR-RESOLVE-MEMORY for why this takes a descriptor object
rather than a machine name."
  (if memory-name
      (storage-element-cell-width (descriptor-element descriptor memory-name))
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
                                  mem-elements)))))))))

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

(defun parse-machine-clauses (clauses)
  (let (elements instruction-word clock-speed)
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
        (t (error "Unknown DEFMACHINE clause head ~S in ~S" (first clause) clause))))
    (values (nreverse elements) instruction-word clock-speed)))

(defun build-machine-descriptor (name clauses)
  (multiple-value-bind (elements instruction-word clock-speed) (parse-machine-clauses clauses)
    (let ((descriptor (make-machine-descriptor :name name :instruction-word instruction-word
                                                :clock-speed clock-speed))
          (seen (make-hash-table :test 'eq)))
      (dolist (element elements)
        (when (gethash (storage-element-name element) seen)
          (error "Duplicate storage element name ~S in machine ~S"
                 (storage-element-name element) name))
        (setf (gethash (storage-element-name element) seen) t)
        (setf (gethash (storage-element-name element) (machine-descriptor-table descriptor))
              element))
      (setf (machine-descriptor-elements descriptor) elements)
      ;; INSTRUCTION-WORD's WIDTH-CELLS/CELL-WIDTH can only be finished now
      ;; that every MEMORY element is known (#53) -- see
      ;; PARSE-INSTRUCTION-WORD-CLAUSE and %FINISH-INSTRUCTION-WORD-LAYOUT.
      ;; A machine with no memory element at all (e.g. SIXTYFOO's
      ;; instruction-less ancestor) or more than one with disagreeing cell
      ;; widths only matters once an instruction-word clause is actually
      ;; declared, so %MACHINE-CELL-WIDTH's error is deferred to here.
      (when instruction-word
        (%finish-instruction-word-layout instruction-word (%descriptor-cell-width descriptor)))
      descriptor)))

(defmacro defmachine (name &body clauses)
  "Define a fantasy-CPU storage model named NAME from CLAUSES, each one of:
     (register NAME :width n [:count n])
     (stack NAME :width n :depth n)
     (memory NAME :width n :addr-width n [:cell-width n])
     (flags NAME...)
     (instruction-word :width n (field NAME width)...)
     (clock-speed n)

INSTRUCTION-WORD (#20, M4) declares a fixed-width instruction word split into
named bit fields (MSB-first, one of them named OPCODE) instead of the
default opcode-byte-plus-operand-bytes encoding -- see DEFINSTRUCTION's
(operand NAME :field F (variant ...)) clause for how an instruction fills
those fields. Optional; a machine with no such clause keeps the default
byte encoding.

CLOCK-SPEED (#75) declares the machine's nominal rate in Hz, used by
RUN-FOR-DURATION (emulator.lisp) to convert accumulated cycles to
wall-time-equivalent seconds. Optional; a machine with no such clause can
still use RUN-FOR-CYCLES and read MACHINE-CYCLES, just not RUN-FOR-DURATION.

Registration happens inside an EVAL-WHEN so the resulting machine-descriptor
is available at macroexpansion time, not only after this file is loaded --
required for M1's DEFINSTRUCTION to resolve storage names/widths against a
DEFMACHINE appearing earlier in the same file."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *machines*)
           (build-machine-descriptor ',name ',clauses))
     ',name))
