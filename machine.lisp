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

;; (instruction-word :width n (field name width) (field name width) ...)
;; (#20, M4) -- a DCPU-16-shaped machine's whole instruction is one N-bit word
;; split into bit fields rather than a byte-per-operand stream. FIELDS is
;; parsed MSB-first as declared: the first field named occupies the highest
;; bits, mirroring how (opcode n)/(operand ...) subclauses already read
;; top-down in the mockups this is modeled on (LASM-plan.md sec. 3.8).
(defun parse-instruction-word-clause (form)
  (let* ((body (rest form))
         (width-pos (position :width body))
         (width (and width-pos (nth (1+ width-pos) body)))
         (field-forms (if width-pos
                           (append (subseq body 0 width-pos) (subseq body (+ width-pos 2)))
                           body)))
    (unless width (error "instruction-word requires :width"))
    (%check-positive width ":width" 'instruction-word)
    (unless (zerop (mod width 8))
      (error "instruction-word :width ~D must be a whole number of bytes" width))
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
         :width-bytes (/ width 8)
         :fields (mapcar (lambda (f)
                            (destructuring-bind (name field-width) f
                              (decf shift field-width)
                              (list name field-width shift)))
                          fields))))))

(defun parse-machine-clauses (clauses)
  (let (elements instruction-word)
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
        (t (error "Unknown DEFMACHINE clause head ~S in ~S" (first clause) clause))))
    (values (nreverse elements) instruction-word)))

(defun build-machine-descriptor (name clauses)
  (multiple-value-bind (elements instruction-word) (parse-machine-clauses clauses)
    (let ((descriptor (make-machine-descriptor :name name :instruction-word instruction-word))
          (seen (make-hash-table :test 'eq)))
      (dolist (element elements)
        (when (gethash (storage-element-name element) seen)
          (error "Duplicate storage element name ~S in machine ~S"
                 (storage-element-name element) name))
        (setf (gethash (storage-element-name element) seen) t)
        (setf (gethash (storage-element-name element) (machine-descriptor-table descriptor))
              element))
      (setf (machine-descriptor-elements descriptor) elements)
      descriptor)))

(defmacro defmachine (name &body clauses)
  "Define a fantasy-CPU storage model named NAME from CLAUSES, each one of:
     (register NAME :width n [:count n])
     (stack NAME :width n :depth n)
     (memory NAME :width n :addr-width n [:cell-width n])
     (flags NAME...)
     (instruction-word :width n (field NAME width)...)

The last (#20, M4) declares a fixed-width instruction word split into named
bit fields (MSB-first, one of them named OPCODE) instead of the default
opcode-byte-plus-operand-bytes encoding -- see DEFINSTRUCTION's (operand NAME
:field F (variant ...)) clause for how an instruction fills those fields.
Optional; a machine with no such clause keeps the default byte encoding.

Registration happens inside an EVAL-WHEN so the resulting machine-descriptor
is available at macroexpansion time, not only after this file is loaded --
required for M1's DEFINSTRUCTION to resolve storage names/widths against a
DEFMACHINE appearing earlier in the same file."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *machines*)
           (build-machine-descriptor ',name ',clauses))
     ',name))
