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

(defun parse-machine-clauses (clauses)
  (let (elements)
    (dolist (clause clauses)
      (case (first clause)
        (register (cl:push (parse-register-clause (rest clause)) elements))
        (stack (cl:push (parse-stack-clause (rest clause)) elements))
        (memory (cl:push (parse-memory-clause (rest clause)) elements))
        (flags (dolist (e (parse-flags-clause (rest clause))) (cl:push e elements)))
        (t (error "Unknown DEFMACHINE clause head ~S in ~S" (first clause) clause))))
    (nreverse elements)))

(defun build-machine-descriptor (name clauses)
  (let ((descriptor (make-machine-descriptor :name name))
        (elements (parse-machine-clauses clauses))
        (seen (make-hash-table :test 'eq)))
    (dolist (element elements)
      (when (gethash (storage-element-name element) seen)
        (error "Duplicate storage element name ~S in machine ~S"
               (storage-element-name element) name))
      (setf (gethash (storage-element-name element) seen) t)
      (setf (gethash (storage-element-name element) (machine-descriptor-table descriptor))
            element))
    (setf (machine-descriptor-elements descriptor) elements)
    descriptor))

(defmacro defmachine (name &body clauses)
  "Define a fantasy-CPU storage model named NAME from CLAUSES, each one of:
     (register NAME :width n [:count n])
     (stack NAME :width n :depth n)
     (memory NAME :width n :addr-width n [:cell-width n])
     (flags NAME...)

Registration happens inside an EVAL-WHEN so the resulting machine-descriptor
is available at macroexpansion time, not only after this file is loaded --
required for M1's DEFINSTRUCTION to resolve storage names/widths against a
DEFMACHINE appearing earlier in the same file."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *machines*)
           (build-machine-descriptor ',name ',clauses))
     ',name))
