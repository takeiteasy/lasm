(load (merge-pathnames "boot.lisp" *load-pathname*))
(require :sb-sprof)

(defun audit-mib (bytes)
  (/ bytes 1048576.0))

(defun audit-checkpoint (label)
  (format t "~&~A: ~,1F MiB allocated, ~,1F MiB heap~%"
          label (audit-mib (sb-ext:get-bytes-consed))
          (audit-mib (sb-kernel:dynamic-usage))))

(defvar *audit-phases* (make-hash-table :test #'equal))
(defvar *audit-sprof* nil)

(defun audit-wrap (name &optional macro-p)
  (let* ((symbol (find-symbol name :lasm))
         (original (if macro-p (macro-function symbol) (symbol-function symbol))))
    (flet ((wrapped (&rest args)
             (let ((allocated (sb-ext:get-bytes-consed)))
               (multiple-value-prog1 (apply original args)
                 (let ((stats (gethash name *audit-phases*)))
                   (incf (car stats))
                   (incf (cdr stats) (- (sb-ext:get-bytes-consed) allocated)))))))
      (setf (gethash name *audit-phases*) (cons 0 0))
      (if macro-p
          (setf (macro-function symbol) #'wrapped)
          (setf (symbol-function symbol) #'wrapped)))))

(defun audit-report-phases ()
  (maphash (lambda (name stats)
             (format t "~&~A: ~D calls, ~,1F MiB allocated (inclusive)~%"
                     name (car stats) (audit-mib (cdr stats))))
           *audit-phases*))

(defmethod asdf:perform :around ((operation asdf:compile-op) (file asdf:cl-source-file))
  (let ((path (namestring (asdf:component-pathname file))))
    (if (search "/star/anima16/" path)
        (let ((allocated (sb-ext:get-bytes-consed))
              (heap (sb-kernel:dynamic-usage)))
          (multiple-value-prog1
              (if (and *audit-sprof* (string= (file-namestring path) "basic.lisp"))
                  (sb-sprof:with-profiling (:mode :alloc :max-samples 20000)
                    (call-next-method))
                  (call-next-method))
            (format t "~&Compile ~A: +~,1F MiB allocated, ~,1F -> ~,1F MiB heap~%"
                    (file-namestring path)
                    (audit-mib (- (sb-ext:get-bytes-consed) allocated))
                    (audit-mib heap)
                    (audit-mib (sb-kernel:dynamic-usage)))))
        (call-next-method))))

(let* ((args (uiop:command-line-arguments))
       (star-asd (or (find-if-not (lambda (arg) (char= (char arg 0) #\-)) args)
                     "../star/star.asd"))
       (combined (member "--combined" args :test #'string=))
       (phases (member "--phases" args :test #'string=))
       (*audit-sprof* (member "--sprof" args :test #'string=))
       (star-tests (member "--star-tests" args :test #'string=)))
  (when phases
    (audit-wrap "DEFINSTRUCTION" t)
    (dolist (name '("%WORD-MODE-DESCRIPTOR-FORMS"
                    "%PARSE-WORD-OPERAND-SUBCLAUSES"
                    "%MAKE-WORD-INSTRUCTION-DESCRIPTORS"
                    "REGISTER-INSTRUCTION-VARIANTS!"
                    "%SIBLING-COMBOS-P"
                    "%CHECK-OPCODE-DECODABLE!"
                    "%COMPUTE-WORD-DECODE-ORDER"))
      (audit-wrap name)))
  (when combined
    (asdf:load-system :lasm/test)
    (unless (uiop:symbol-call :fiveam :run! 'lasm:lasm)
      (error "LASM tests failed")))
  (asdf:load-asd (truename star-asd))
  (unless combined (sb-ext:gc :full t))
  (let ((allocated (sb-ext:get-bytes-consed)))
    (audit-checkpoint "Before STAR")
    (asdf:load-system :star/anima16 :force t)
    (audit-checkpoint "After STAR build")
    (format t "~&STAR build allocation: ~,1F MiB~%"
            (audit-mib (- (sb-ext:get-bytes-consed) allocated)))
    (when phases (audit-report-phases))
    (when *audit-sprof* (sb-sprof:report :type :flat :max 30))
    (when star-tests
      (asdf:test-system :star/anima16)
      (audit-checkpoint "After STAR tests"))
    (sb-ext:gc :full t)
    (audit-checkpoint "After full GC")))
