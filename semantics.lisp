;;;; semantics.lisp
;;;; The minimal semantics vocabulary: WITH-MACHINE, SET!, PUSH, POP,
;;;; SET-FLAGS!, TRAP, and the small predicate helpers the mockups use.
;;;;
;;;; WITH-MACHINE is the single semantics entry point: M1's DEFINSTRUCTION
;;;; expands its (semantics ...) body by wrapping it in WITH-MACHINE rather
;;;; than growing a parallel expander, so the vocabulary defined here is
;;;; shared by M0's standalone examples and M1's instruction semantics alike.

(in-package #:lasm)

(defmacro with-machine ((machine-var machine-name) &body body)
  "Evaluate BODY with every scalar storage/flag element of the machine
descriptor MACHINE-NAME bound as a symbol-macro, plus the semantics
operators SET!, PUSH, POP, SET-FLAGS!, and TRAP.

MACHINE-VAR is bound to the runtime MACHINE instance (made fresh via
MAKE-MACHINE) for the duration of BODY.

Storage elements with :count > 1 (banked registers) are not bound here --
symbol-macrolet can't express indexed access like (V 3); see the
storage-element :count docstring in storage.lisp."
  (let ((descriptor (find-machine-descriptor machine-name)))
    (let (symbol-macros)
      (dolist (element (machine-descriptor-elements descriptor))
        (case (storage-element-kind element)
          (:register
           (when (= (storage-element-count element) 1)
             (let ((name (storage-element-name element)))
               (cl:push `(,name (sref ,machine-var ',name)) symbol-macros))))
          (:flag
           (let ((name (storage-element-name element)))
             (cl:push `(,name (flag ,machine-var ',name)) symbol-macros)))
          ;; :stack and :memory elements are accessed through STACK-PUSH/
          ;; STACK-POP/MREF directly by name (as a quoted symbol), not bound
          ;; as symbol-macros, since they take an explicit operand.
          ((:stack :memory))))
      `(let ((,machine-var (make-machine ',machine-name)))
         (symbol-macrolet ,(nreverse symbol-macros)
           (macrolet ((set! (place value)
                        `(setf ,place ,value))
                      (push (value stack-name)
                        `(stack-push ,',machine-var ',stack-name ,value))
                      (pop (stack-name)
                        `(stack-pop ,',machine-var ',stack-name))
                      (set-flags! (&rest assignments)
                        `(progn ,@(mapcar (lambda (a)
                                             `(setf (flag ,',machine-var ',(first a)) ,(second a)))
                                           assignments)))
                      (trap (tag &optional data)
                        `(error 'lasm-trap :tag ,tag :data ,data)))
             ,@body))))))

(defun zero? (value) (zerop value))

(defun bit-set? (value bit) (logbitp bit value))
