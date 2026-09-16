;;;; examples/boot.lisp
;;;;
;;;; Shared bootstrap for every example in this directory. #75 gave LASM its
;;;; first dependency (trivial-high-precision-timer, itself depending on
;;;; CFFI on SBCL) -- both are Quicklisp libraries, so a bare `sbcl --script`
;;;; run (no ~/.sbclrc) needs Quicklisp bootstrapped explicitly before ASDF
;;;; can resolve them, same as docs/getting-started.md's install instructions
;;;; assume.
;;;;
;;;; Each example loads this file relative to its own location, then enters
;;;; the LASM package:
;;;;
;;;;   (load (merge-pathnames "boot.lisp" *load-pathname*))
;;;;   (in-package #:lasm)

(require :asdf)
(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-setup)
      (load quicklisp-setup)
      (error "Quicklisp not found at ~A -- see docs/getting-started.md" quicklisp-setup)))
(let ((here (make-pathname :name nil :type nil :defaults *load-pathname*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))
