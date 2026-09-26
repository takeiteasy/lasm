;;;; examples/backend.lisp
;;;;
;;;; A front end targets a machine through a backend (#113): DEFBACKEND names
;;;; the register roles, calling convention, operand kinds and operations, and
;;;; a program is a list of items -- data, not source text -- that
;;;; ASSEMBLE-ITEMS assembles directly. The machine and backend are
;;;; examples/cli/callfoo.lisp; the same program as a file is
;;;; examples/cli/double.lasm.
;;;;
;;;; Run with:  sbcl --script examples/backend.lisp

(load (merge-pathnames "boot.lisp" *load-pathname*))

(in-package #:lasm)

(load (merge-pathnames "cli/callfoo.lisp" *load-pathname*))

(let ((backend (find-backend 'callfoo-abi)))
  (format t "~&Backend ~A targets ~A~%  return register ~A, callee-saved ~{~A~^ ~}~%  call ~S~%"
          (backend-descriptor-name backend) (backend-descriptor-machine backend)
          (first (backend-register backend :return))
          (backend-register backend :callee-saved)
          (backend-descriptor-call backend))
  (assert (equal '("A") (backend-register backend :return))))

;; An operation expands to instruction forms; operands are (KIND value...).
(format t "~%(:op :load (reg a) 5) expands to ~S~%"
        (backend-expand-op 'callfoo-abi :load '((reg a) 5)))

(defparameter *items*
  '((:label main)
    (push (imm 21))                     ; argument, on the stack
    (:op :call double)
    (adds (sp) (imm 1))                 ; the caller removes it
    (hlt)

    (:label double)
    (lds (reg a) (sp-idx 1))            ; first argument, above the return address
    (:op :add (reg a) (reg a))
    (:op :return)))

(format t "~%Rendered as source:~%~A" (render-items *items* :backend 'callfoo-abi))

(let* ((assembly (assemble-items *items* :backend 'callfoo-abi))
       (machine (make-machine 'callfoo)))
  (load-program machine assembly)
  (multiple-value-bind (reason steps) (run machine)
    (format t "~%stopped: ~A after ~D steps, a = ~D~%" reason steps (regref machine 'r 0))
    (assert (eq :trap reason))
    (assert (= 7 steps))
    (assert (= 42 (regref machine 'r 0))))
  ;; The rendered text assembles to the same cells.
  (assert (equalp (assembly-cells assembly)
                  (assembly-cells (assemble (render-items *items* :backend 'callfoo-abi)
                                            :machine 'callfoo)))))

;; The convention lowers the call: the pushes, call and clean-up come from
;; callfoo-abi's call and frame clauses (docs/conventions.md).
(let* ((lowered '((:call double (imm 21))
                  (hlt)
                  (:function double (:args 1)
                    (lds (reg a) (:arg 0))
                    (:op :add (reg a) (reg a))
                    (:return))))
       (machine (make-machine 'callfoo)))
  (format t "~%Lowered from a call and a function:~%~A" (render-items lowered :backend 'callfoo-abi))
  (load-program machine (assemble-items lowered :backend 'callfoo-abi))
  (run machine)
  (assert (= 42 (regref machine 'r 0))))

;; The same program read from data: symbols are never interned.
(let ((program (read-items-from-string
                "(:program (:backend callfoo-abi) (:label start) (:op :load (reg b) 7) (hlt))")))
  (format t "~%Items read from text: ~S~%" (items-program-items program))
  (assert (equalp (assembly-cells (assemble "start:
ldi b, #7
hlt" :machine 'callfoo))
                  (assembly-cells (assemble-items (items-program-items program)
                                                  :backend (items-program-backend program))))))

;; An operand that does not match its mode is rejected.
(handler-case (assemble-items '((ldi (reg 5) (imm 1))) :backend 'callfoo-abi)
  (items-operand-mismatch (c)
    (format t "~%Rejected: ~A~%" (items-error-detail c))))

(format t "~%All assertions passed.~%")
