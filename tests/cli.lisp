;;;; tests/cli.lisp
;;;; fiveam tests for cli.lisp (#80): RUN-CLI driven with argument lists
;;;; against examples/cli/ and tests/fixtures/cli/.

(in-package #:lasm)

(fiveam:def-suite cli :in lasm)
(fiveam:in-suite cli)

(defun %cli-path (relative)
  (namestring (asdf:system-relative-pathname :lasm relative)))

(defun %cli-args (command file &rest more)
  (list* command (%cli-path file) "-m" (%cli-path "examples/cli/sixtyfoo.lasm") more))

(defun %run-cli (args)
  "Returns (VALUES EXIT-STATUS STDOUT STDERR)."
  (let* ((err (make-string-output-stream))
         (out (make-string-output-stream))
         (status (run-cli args :out out :err err)))
    (values status (get-output-stream-string out) (get-output-stream-string err))))

(fiveam:test cli-assemble-writes-binary-by-default
  (uiop:with-temporary-file (:pathname path :type "bin")
    (multiple-value-bind (status out)
        (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm") (list "-o" (namestring path))))
      (fiveam:is (= 0 status))
      (fiveam:is (search "9 bytes" out))
      (fiveam:is (equalp #(#xA2 10 #xCA #xD0 #xFD #x8D 0 #x10 0) (%cli-read-bytes path))))))

(fiveam:test cli-assemble-hex-format
  (uiop:with-temporary-file (:pathname path :type "hex")
    (fiveam:is (= 0 (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm")
                                      (list "-o" (namestring path) "--format" "hex")))))
    (fiveam:is (string= (format nil ":09000000A20ACAD0FD8D00100017~%:00000001FF~%")
                        (uiop:read-file-string path)))))

(fiveam:test cli-assemble-origin-flag
  (uiop:with-temporary-file (:pathname path :type "hex")
    (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm")
                      (list "-o" (namestring path) "--format" "hex" "--origin" "$100")))
    (fiveam:is (string= ":09010000" (subseq (uiop:read-file-string path) 0 9)))))

(fiveam:test cli-run-reports-stop-reason
  (multiple-value-bind (status out) (%run-cli (%cli-args "run" "examples/cli/counter.asm"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped: trap after 23 steps, pc = $0009" out))))

(fiveam:test cli-run-honours-max-steps
  (multiple-value-bind (status out)
      (%run-cli (append (%cli-args "run" "examples/cli/counter.asm") (list "--max-steps" "5")))
    (fiveam:is (= 0 status))
    (fiveam:is (search "stopped: max-steps after 5 steps" out))))

(fiveam:test cli-run-storage-fault-exits-one-and-shows-condition
  (dolist (extra '(() ("--cycles" "100")))
    (multiple-value-bind (status out err)
        (%run-cli (append (list "run" (%cli-path "tests/fixtures/cli/storage-fault.asm")
                                "-m" (%cli-path "tests/fixtures/cli/storage-fault.lasm"))
                          extra))
      (fiveam:is (= 1 status))
      (fiveam:is (search "stopped: fault after 1 step, pc = $0001" out))
      (fiveam:is (search "Stack underflow" out))
      (fiveam:is (string= "" err)))))

(fiveam:test cli-run-undefined-opcode-trap-exits-one
  (multiple-value-bind (status out err)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/undefined-opcode.asm")
                      "-m" (%cli-path "tests/fixtures/cli/undefined-opcode.lasm")))
    (fiveam:is (= 1 status))
    (fiveam:is (search "stopped: trap after 2 steps, pc = $0001" out))
    (fiveam:is (string= "" err))))

(fiveam:test cli-listing-and-symbols
  (multiple-value-bind (status out)
      (%run-cli (append (%cli-args "listing" "examples/cli/counter.asm") (list "--symbols")))
    (fiveam:is (= 0 status))
    (fiveam:is (search "A2 0A" out))
    (fiveam:is (search "count" out))
    (fiveam:is (search ".loop" out))))

(fiveam:test cli-disassemble-round-trips-assembled-binary
  (uiop:with-temporary-file (:pathname path :type "bin")
    (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm") (list "-o" (namestring path))))
    (multiple-value-bind (status out)
        (%run-cli (list "disassemble" (namestring path) "-m" (%cli-path "examples/cli/sixtyfoo.lasm")))
      (fiveam:is (= 0 status))
      (dolist (text '("ldx #$A" "dex" "sta $1000" "hlt"))
        (fiveam:is (search text out) "~S missing from:~%~A" text out)))))

(fiveam:test cli-disassemble-annotate-prints-cells
  (uiop:with-temporary-file (:pathname path :type "bin")
    (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm") (list "-o" (namestring path))))
    (fiveam:is (search "A2 0A" (nth-value 1 (%run-cli (list "disassemble" (namestring path) "-m"
                                                            (%cli-path "examples/cli/sixtyfoo.lasm")
                                                            "--annotate")))))))

(fiveam:test cli-definitions-do-not-leak-into-the-image
  (%run-cli (%cli-args "listing" "examples/cli/counter.asm"))
  (fiveam:is (null (gethash 'sixtyfoo *machines*)))
  (fiveam:is (null (gethash 'sixtyfoo-syntax *lexers*))))

(fiveam:test cli-load-keeps-compiler-output-off-the-streams
  (multiple-value-bind (status out err) (%run-cli (%cli-args "listing" "examples/cli/counter.asm"))
    (fiveam:is (= 0 status))
    (fiveam:is (string= "" err))
    (fiveam:is (not (search "STYLE-WARNING" out)))))

(fiveam:test cli-help-exits-zero-on-stdout
  (multiple-value-bind (status out err) (%run-cli '("--help"))
    (fiveam:is (= 0 status))
    (fiveam:is (search "usage: lasm" out))
    (fiveam:is (string= "" err))))

(fiveam:test cli-usage-errors-exit-two
  (dolist (args (list '() '("frobnicate" "x.asm" "-m" "m.lasm") '("run") '("run" "x.asm")
                      '("run" "x.asm" "-m") '("run" "x.asm" "--nope")
                      (append (%cli-args "assemble" "examples/cli/counter.asm") '("--format" "elf"))
                      (append (%cli-args "run" "examples/cli/counter.asm") '("--max-steps" "many"))))
    (multiple-value-bind (status out err) (%run-cli args)
      (fiveam:is (= 2 status) "~S exited ~D" args status)
      (fiveam:is (string= "" out))
      (fiveam:is (search "usage: lasm" err)))))

(fiveam:test cli-assembly-error-exits-one-with-diagnostic
  (multiple-value-bind (status out err)
      (%run-cli (%cli-args "assemble" "tests/fixtures/cli/bad.asm"))
    (fiveam:is (= 1 status))
    (fiveam:is (string= "" out))
    (fiveam:is (search "frobnicate" err))))

(fiveam:test cli-undefined-label-names-source-file
  (multiple-value-bind (status out err)
      (%run-cli (%cli-args "assemble" "tests/fixtures/cli/undefined.asm"))
    (fiveam:is (= 1 status))
    (fiveam:is (string= "" out))
    (fiveam:is (search "undefined.asm:1:5" err))
    (fiveam:is (search "bne missing" err))))

(fiveam:test cli-missing-input-exits-one
  (multiple-value-bind (status out err) (%run-cli (%cli-args "assemble" "tests/fixtures/cli/missing.asm"))
    (fiveam:is (= 1 status))
    (fiveam:is (string= "" out))
    (fiveam:is (search "lasm:" err))))

(fiveam:test cli-several-machines-need-machine-name
  (let ((args (list "listing" (%cli-path "tests/fixtures/cli/bad.asm")
                    "-m" (%cli-path "tests/fixtures/cli/two-machines.lasm"))))
    (multiple-value-bind (status out err) (%run-cli args)
      (fiveam:is (= 1 status))
      (fiveam:is (string= "" out))
      (fiveam:is (search "--machine-name" err)))
    (fiveam:is (search "no machine named" (nth-value 2 (%run-cli (append args '("--machine-name" "nope"))))))))

(fiveam:test cli-machine-flag-is-required
  (multiple-value-bind (status out err) (%run-cli (list "listing" (%cli-path "examples/cli/counter.asm")))
    (fiveam:is (= 2 status))
    (fiveam:is (string= "" out))
    (fiveam:is (search "-m MACHINE.lasm is required" err))))

(fiveam:test cli-disassemble-data-region-renders-bytes
  (uiop:with-temporary-file (:pathname path :type "bin")
    (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm") (list "-o" (namestring path))))
    (let ((machine (%cli-path "examples/cli/sixtyfoo.lasm")))
      (multiple-value-bind (status out)
          (%run-cli (list "disassemble" (namestring path) "-m" machine "--data-region" "0:2"))
        (fiveam:is (= 0 status))
        (fiveam:is (search ".byte $A2" out))
        (fiveam:is (not (search "ldx" out))))
      ;; repeatable, and hex forms parse
      (multiple-value-bind (status out)
          (%run-cli (list "disassemble" (namestring path) "-m" machine
                          "--data-region" "0:1" "--data-region" "$1:0x2"))
        (fiveam:is (= 0 status))
        (fiveam:is (not (search "ldx" out)))))))

(fiveam:test cli-disassemble-rejects-malformed-data-region
  (uiop:with-temporary-file (:pathname path :type "bin")
    (%run-cli (append (%cli-args "assemble" "examples/cli/counter.asm") (list "-o" (namestring path))))
    (dolist (bad '("5" "3:1" "2:2" "a:b" ":4"))
      (multiple-value-bind (status out err)
          (%run-cli (list "disassemble" (namestring path) "-m" (%cli-path "examples/cli/sixtyfoo.lasm")
                          "--data-region" bad))
        (fiveam:is (= 2 status) "~S" bad)
        (fiveam:is (string= "" out))
        (fiveam:is (search "--data-region" err))))))

(fiveam:test cli-run-fault-and-decode-failure-name-the-source-line
  (multiple-value-bind (status out)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/storage-fault.asm")
                      "-m" (%cli-path "tests/fixtures/cli/storage-fault.lasm")))
    (fiveam:is (= 1 status))
    (fiveam:is (search "(line 1: popempty)" out)))
  (multiple-value-bind (status out)
      (%run-cli (list "run" (%cli-path "tests/fixtures/cli/undefined-opcode.asm")
                      "-m" (%cli-path "tests/fixtures/cli/undefined-opcode.lasm")))
    (fiveam:is (= 1 status))
    (fiveam:is (search "(line 2: .byte 2)" out))))

(fiveam:test cli-listing-cycle-costs
  (multiple-value-bind (status out)
      (%run-cli (append (%cli-args "listing" "examples/cli/counter.asm") (list "--cycle-costs")))
    (fiveam:is (= 0 status))
    (fiveam:is (search "1    A2 0A" out)))
  (multiple-value-bind (status out) (%run-cli (%cli-args "listing" "examples/cli/counter.asm"))
    (fiveam:is (= 0 status))
    (fiveam:is (not (search "1    A2 0A" out)))))

(fiveam:test cli-load-does-not-leak-modes
  (let ((before (gethash 'immediate *modes*)))
    (%cli-call-with-definitions (list :machine-file (%cli-path "tests/fixtures/cli/local-mode.lasm"))
                                (lambda (machine lexer)
                                  (declare (ignore lexer))
                                  (fiveam:is (eq 'cli-local-mode machine))
                                  (fiveam:is (find-mode-descriptor 'cli-leak-probe))
                                  (fiveam:is (find-mode-descriptor 'cli-leak-local 'cli-local-mode))))
    (fiveam:is (eq before (gethash 'immediate *modes*)))
    (fiveam:is (null (gethash 'cli-leak-probe *modes*)))
    (fiveam:is (null (gethash 'cli-local-mode *machine-modes*)))))

(defun %twelve-args (command file &rest more)
  (list* command (%cli-path file) "-m" (%cli-path "examples/cli/twelve.lasm") more))

(fiveam:test cli-assemble-pads-twelve-bit-cells-by-default
  (uiop:with-temporary-file (:pathname path :type "bin")
    (fiveam:is (= 0 (%run-cli (append (%twelve-args "assemble" "examples/cli/twelve.asm")
                                      (list "-o" (namestring path))))))
    (fiveam:is (equalp #(#x0A #x01 #x0A #xBC #x0B #x02 #x01 #x23 0 0) (%cli-read-bytes path)))))

(fiveam:test cli-assemble-and-disassemble-bit-packed-cells
  (uiop:with-temporary-file (:pathname path :type "bin")
    (multiple-value-bind (status out)
        (%run-cli (append (%twelve-args "assemble" "examples/cli/twelve.asm")
                          (list "-o" (namestring path) "--packing" "bits")))
      (fiveam:is (= 0 status))
      (fiveam:is (search "8 bytes" out)))
    (fiveam:is (equalp #(#xA0 #x1A #xBC #xB0 #x21 #x23 0 0) (%cli-read-bytes path)))
    (let ((text (nth-value 1 (%run-cli (list "disassemble" (namestring path) "-m"
                                             (%cli-path "examples/cli/twelve.lasm")
                                             "--packing" "bits")))))
      (dolist (line '("lda #$ABC" "sta $123" "hlt"))
        (fiveam:is (search line text) "~S missing from:~%~A" line text)))))

(fiveam:test cli-packing-rejects-an-unknown-value
  (fiveam:is (= 2 (%run-cli (append (%twelve-args "assemble" "examples/cli/twelve.asm")
                                    (list "--packing" "nibbles"))))))

(fiveam:test cli-disassemble-cells-drops-bit-padding
  (uiop:with-temporary-file (:pathname path :type "bin")
    (let ((lasm (%cli-path "tests/fixtures/cli/nibble.lasm")))
      (%run-cli (list "assemble" (%cli-path "tests/fixtures/cli/nibble.asm") "-m" lasm
                      "-o" (namestring path) "--packing" "bits"))
      (flet ((nops (&rest more)
               (let ((text (nth-value 1 (%run-cli (append (list "disassemble" (namestring path) "-m" lasm
                                                                "--packing" "bits")
                                                          more)))))
                 (count-if (lambda (line) (search "nop" line))
                           (%hex-lines text)))))
        (fiveam:is (= 3 (nops "--cells" "3")))
        (fiveam:is (= 2 (%run-cli (list "disassemble" (namestring path) "-m" lasm "--cells" "x"))))))))
