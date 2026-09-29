;;;; tests/identity.lisp
;;;; fiveam tests for the (identity ...) clause: what a CPU reports about itself.

(in-package #:lasm)

(fiveam:def-suite identity :in lasm)
(fiveam:in-suite identity)

(defisa ident-open
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defisa ident-strict
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (identity :required t))

(defcpu (ident-plain (:isa ident-open)))

(defcpu (ident-named (:isa ident-open))
  (identity :model "Named-1" :id #xC9000001 :version #x0107 :manufacturer #xBAAAAAAD))

(defcpu (ident-child (:extends ident-named))
  (identity :version 2))

(defcpu (ident-full (:isa ident-strict))
  (identity :model "full" :id 1 :version 2 :manufacturer 3))

(defcpu (ident-full-child (:extends ident-full))
  (identity :id 9))

(fiveam:test identity-defaults-to-the-cpu-name-and-zeros
  (fiveam:is (equal "ident-plain" (cpu-model 'ident-plain)))
  (fiveam:is (equal '(0 0 0) (multiple-value-list (cpu-info 'ident-plain)))))

(fiveam:test identity-reads-back-from-a-name-a-descriptor-and-a-machine
  (dolist (thing (list 'ident-named (find-machine-descriptor 'ident-named) (make-machine 'ident-named)))
    (fiveam:is (equal "Named-1" (cpu-model thing)))
    (fiveam:is (equal '(#xC9000001 #x0107 #xBAAAAAAD) (multiple-value-list (cpu-info thing))))
    (fiveam:is (eq 'ident-open (cpu-isa thing)))))

(fiveam:test a-child-cpu-merges-identity-key-by-key
  (fiveam:is (equal "Named-1" (cpu-model 'ident-child)))
  (fiveam:is (equal '(#xC9000001 2 #xBAAAAAAD) (multiple-value-list (cpu-info 'ident-child)))))

(fiveam:test a-required-identity-must-be-complete
  (fiveam:is (equal "full" (cpu-model 'ident-full)))
  (fiveam:is (equal '(9 2 3) (multiple-value-list (cpu-info 'ident-full-child))))
  (dolist (clauses '(() ((identity :model "m" :id 1 :version 2))
                     ((identity :id 1 :version 2 :manufacturer 3))))
    (fiveam:signals machine-definition-error
      (eval `(defcpu (ident-incomplete (:isa ident-strict)) ,@clauses)))
    (fiveam:is (null (gethash 'ident-incomplete *machines*)))))

(fiveam:test the-missing-keys-are-named
  (fiveam:is (search ":ID, :VERSION"
                     (handler-case (eval '(defcpu (ident-incomplete (:isa ident-strict))
                                           (identity :model "m" :manufacturer 3)))
                       (machine-definition-error (c) (princ-to-string c))))))

(defmachine ident-bridge
  (register a :width 8)
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16)
  (identity :required t :model "bridge" :id 5 :version 1 :manufacturer 7))

(fiveam:test defmachine-splits-identity-between-isa-and-cpu
  (fiveam:is (machine-descriptor-identity-required (find-isa-descriptor 'ident-bridge)))
  (fiveam:is (null (machine-descriptor-identity (find-isa-descriptor 'ident-bridge))))
  (fiveam:is (equal '(5 1 7) (multiple-value-list (cpu-info 'ident-bridge))))
  (fiveam:is (eq 'ident-bridge (cpu-isa 'ident-bridge))))

(fiveam:test malformed-identity-is-a-definition-error
  (dolist (clause '((identity :model 5) (identity :id -1) (identity :version "x")
                    (identity :manufacturer 1.5) (identity :bogus 1) (identity :id)
                    (identity :required 1)))
    (fiveam:signals machine-definition-error
      (eval `(defcpu (ident-bad (:isa ident-open)) ,clause))))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (ident-bad (:isa ident-open)) (identity :id 1) (identity :id 2)))))

(fiveam:test defisa-takes-only-required-and-defcpu-never
  (dolist (clause '((identity :model "m") (identity :id 1) (identity :required t :id 1) (identity :required)))
    (fiveam:signals machine-definition-error
      (eval `(defisa ident-bad-isa (register pc :width 8) (memory ram :width 8 :addr-width 8) ,clause)))
    (fiveam:is (null (gethash 'ident-bad-isa *isas*))))
  (fiveam:signals machine-definition-error
    (eval '(defcpu (ident-bad (:isa ident-open)) (identity :required t)))))
