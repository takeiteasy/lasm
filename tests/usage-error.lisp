;;;; tests/usage-error.lisp
;;;; fiveam tests for the USAGE-ERROR and LOOKUP-ERROR families (diagnostic.lisp).

(in-package #:lasm)

(fiveam:def-suite usage-error :in lasm)
(fiveam:in-suite usage-error)

(fiveam:test lookups-signal-typed-lookup-errors
  (loop for (fn type name) in '((find-machine-descriptor unknown-machine no-such-machine)
                                (find-mode-descriptor unknown-mode no-such-mode)
                                (find-lexer-descriptor unknown-lexer no-such-lexer))
        do (let ((c (handler-case (funcall fn name) (lookup-error (c) c))))
             (fiveam:is (typep c type))
             (fiveam:is (typep c 'usage-error))
             (fiveam:is (eq name (lookup-error-name c)))
             (fiveam:is (search (string name) (princ-to-string c)
                                :test #'char-equal)))))

(fiveam:test usage-errors-are-lasm-errors
  (dolist (type '(debugger-usage-error disassembler-usage-error output-usage-error
                  emulator-usage-error lookup-error))
    (fiveam:is (subtypep type 'usage-error))
    (fiveam:is (subtypep type 'lasm-error))))

(fiveam:test debugger-api-misuse-is-a-debugger-usage-error
  (let ((session (%dbg-session)))
    (fiveam:signals debugger-usage-error (debug-break session "no-such-label"))))

(fiveam:test debugger-command-reports-lasm-errors-as-text
  (multiple-value-bind (text) (debug-command (%dbg-session) "break no-such-label")
    (fiveam:is (search "Error:" text))))

(fiveam:test disassembler-misuse-is-a-disassembler-usage-error
  (fiveam:signals disassembler-usage-error
    (disassemble-memory (make-machine 'emu-test-machine))))

(fiveam:test emulator-misuse-is-an-emulator-usage-error
  (let ((m (make-machine 'emu-test-machine)))
    (fiveam:signals emulator-usage-error (machine-elapsed-seconds m))
    (fiveam:signals emulator-usage-error
      (load-program m (assemble "hlt" :machine 'emu-test-machine) :bank 1))))

(fiveam:test output-misuse-is-an-output-usage-error
  (fiveam:signals output-usage-error
    (bytes-to-cells (list 1 2 3) 16)))
