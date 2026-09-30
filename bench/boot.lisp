;;;; bench/boot.lisp
;;;;
;;;; Bootstrap for scripts run with `sbcl --script`, which reads no ~/.sbclrc:
;;;; loads Quicklisp so ASDF can resolve LASM's dependencies, then loads LASM.
;;;; Scripts load it relative to their own location:
;;;;
;;;;   (load (merge-pathnames "../bench/boot.lisp" *load-pathname*))

(require :asdf)
(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file quicklisp-setup)
      (load quicklisp-setup)
      (error "Quicklisp not found at ~A -- see docs/getting-started.md" quicklisp-setup)))
(let ((here (make-pathname :name nil :type nil :defaults *load-truename*)))
  (asdf:load-asd (merge-pathnames "../lasm.asd" here))
  (asdf:load-system :lasm))
