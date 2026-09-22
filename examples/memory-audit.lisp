;;;; SBCL audit: sbcl --script examples/memory-audit.lisp ../star/star.asd

(load (merge-pathnames "boot.lisp" *load-pathname*))

(defun memory-checkpoint (label)
  (sb-ext:gc :full t)
  (format t "~&~A: ~,2F MiB live dynamic heap~%"
          label (/ (sb-kernel:dynamic-usage) 1048576.0)))

(memory-checkpoint "LASM")
(asdf:load-asd (truename (or (first (uiop:command-line-arguments))
                            "../star/star.asd")))
(asdf:load-system :star/anima16)
(memory-checkpoint "STAR")

(defvar *audit-machines* nil)
(defun audit-allocate-machines (count)
  (setf *audit-machines*
        (loop repeat count collect (lasm:make-machine 'lasm::anima16)))
  nil)

(defun audit-instance-footprint ()
  (setf *audit-machines* nil)
  (sb-ext:gc :full t)
  (let ((before (sb-kernel:dynamic-usage)))
    (audit-allocate-machines 100)
    (sb-ext:gc :full t)
    (format t "~&Retained heap per CPU, including list entry: ~,2F KiB~%"
            (/ (- (sb-kernel:dynamic-usage) before) 100.0 1024.0))))

(audit-instance-footprint)
(memory-checkpoint "100 CPUs")
(setf *audit-machines* nil)
(memory-checkpoint "CPUs released")

(let* ((assembly (star/anima16:assemble-anima16
                  (format nil "loop: add a, #1~%set PC, #loop~%")))
       (machine (lasm:make-machine 'lasm::anima16)))
  (lasm:load-program machine assembly)
  (let ((start (get-internal-real-time)))
    (lasm:step-machine machine)
    (format t "~&First step, including dispatch initialization: ~,3F seconds~%"
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
  (lasm:run machine :max-steps 10000)
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed))
        (start (get-internal-real-time))
        (gc-start sb-ext:*gc-run-time*))
    (multiple-value-bind (reason steps) (lasm:run machine :max-steps 1000000)
      (assert (and (eq reason :max-steps) (= steps 1000000)))
    (format t "~&Million steps: ~,2F MiB allocated, ~,3F seconds, ~,3F GC seconds~%"
            (/ (- (sb-ext:get-bytes-consed) before) 1048576.0)
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)
            (/ (- sb-ext:*gc-run-time* gc-start) internal-time-units-per-second)))))
(memory-checkpoint "Execution complete")
