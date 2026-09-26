;;;; tests/backend.lisp
;;;; #113: DEFBACKEND, structured items and .lasm programs.

(in-package #:lasm)

(fiveam:def-suite backend :in lasm)
(fiveam:in-suite backend)

;;; Fixture: examples/cli/callfoo.lisp, the machine and backend behind
;;; double.lasm, plus a machine whose LD takes a ONE-OF operand.

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "examples/cli/callfoo.lisp")))

(defmachine bk-ld-machine
  (register pc :width 16)
  (register r :width 16 :names (a b c d))
  (memory ram :width 16 :addr-width 16))

(defmode bk-ld-reg (expr :register r))
(defmode bk-ld-imm "#" expr)
(defmode bk-ld-ind "[" (expr :register r) "]")
(defmode bk-ld-abs "[" expr "]")
(defmode bk-ld-mode (expr :register r) "," (one-of bk-ld-imm bk-ld-ind bk-ld-abs))

(definstruction bk-ld-machine ld
  (modes bk-ld-mode)
  (encoding (opcode 1) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r dst) src)))

(defbackend bk-ld-abi (:machine bk-ld-machine)
  (operands (reg bk-ld-reg) (imm bk-ld-imm) (ind bk-ld-ind) (abs bk-ld-abs)))

(defparameter *double-items*
  '((:label main)
    (push (imm 21))
    (:op :call double)
    (adds (sp) (imm 1))
    (hlt)
    (:label double)
    (lds (reg a) (sp-idx 1))
    (:op :add (reg a) (reg a))
    (:op :return)))

(defparameter *double-source*
  "main:
push #21
call double
adds sp, #1
hlt
double:
lds a, [sp + 1]
add a, a
ret
")

(defun %backend-error-of (form)
  (handler-case (progn (eval form) nil)
    (backend-definition-error (c) c)))

(defun %items-error-of (items &rest keys)
  (handler-case (progn (apply #'assemble-items items keys) nil)
    (items-error (c) c)))

;;; Definition

(fiveam:test defbackend-records-its-clauses
  (let ((backend (find-backend 'callfoo-abi)))
    (fiveam:is (eq 'callfoo (backend-descriptor-machine backend)))
    (fiveam:is (equal '("A") (backend-register backend :return)))
    (fiveam:is (equal '("C" "D") (backend-register backend :callee-saved)))
    (fiveam:is (equal "SP" (backend-register backend :stack-pointer)))
    (fiveam:is (equal '(:args :stack :order :right-to-left :cleanup :caller :return-address-slots 1)
                      (backend-descriptor-call backend)))
    (fiveam:is (equal '(:grows :down :alignment 1) (backend-descriptor-frame backend)))
    (fiveam:is (equal '("REG" . call-reg) (first (backend-descriptor-operands backend))))
    (fiveam:is (eq backend (find-backend backend)))))

(fiveam:test defbackend-defaults-the-frame-direction-from-the-stack-pointer
  (eval '(defbackend bk-default-frame-abi (:machine callfoo) (registers :stack-pointer sp)))
  (fiveam:is (eq :down (getf (backend-descriptor-frame (find-backend 'bk-default-frame-abi)) :grows))))

(fiveam:test defbackend-finds-a-backend-by-name-in-any-package
  (fiveam:is (eq (find-backend 'callfoo-abi) (find-backend "callfoo-abi")))
  (fiveam:signals unknown-backend (find-backend 'no-such-backend)))

(fiveam:test defbackend-signals-a-typed-error-naming-the-definition
  (dolist (form '((defbackend bk-bad-1 (:machine no-such-machine))
                  (defbackend bk-bad-2 (:machine callfoo) (bogus))
                  (defbackend bk-bad-3 (:machine callfoo) (registers :return (nope)))
                  (defbackend bk-bad-4 (:machine callfoo) (registers :bogus (a)))
                  (defbackend bk-bad-5 (:machine callfoo) (registers :callee-saved (a) :caller-saved (a)))
                  (defbackend bk-bad-6 (:machine callfoo) (registers :return (a a)))
                  (defbackend bk-bad-7 (:machine callfoo) (registers :stack-pointer a))
                  (defbackend bk-bad-8 (:machine callfoo) (registers :stack-pointer sp) (frame :grows :up))
                  (defbackend bk-bad-9 (:machine callfoo) (call :cleanup :nobody))
                  (defbackend bk-bad-10 (:machine callfoo) (call :order :sideways))
                  (defbackend bk-bad-11 (:machine callfoo) (call :return-address-slots -1))
                  (defbackend bk-bad-12 (:machine callfoo) (registers :return (a)) (registers :return (b)))
                  (defbackend bk-bad-13 (:machine callfoo) (operands (reg no-such-mode)))
                  (defbackend bk-bad-14 (:machine callfoo) (operands (reg call-reg) (reg call-imm)))
                  (defbackend bk-bad-15 (:machine callfoo) (operands (+ call-reg)))
                  (defbackend bk-bad-16 (:machine callfoo) (ops (nop () (no-such-instruction))))
                  (defbackend bk-bad-17 (:machine callfoo) (ops (nop () (ret)) (nop () (ret))))
                  (defbackend bk-bad-18 (:machine callfoo) (ops (nop (x x) (ret))))
                  (defbackend bk-bad-19 (:machine callfoo) (ops (nop ())))
                  (defbackend bk-bad-20 (:machine callfoo) (ops (nop () (push (nope 1)))))
                  (defbackend bk-bad-21 (:machine callfoo) (ops (nop () (push stray))))
                  (defbackend bk-bad-22 (:machine callfoo) (operands (imm call-imm)) (ops (nop (v) (push (v 1)))))
                  (defbackend bk-bad-23 (:machine callfoo) (call :args :stack :args :stack))))
    (let ((c (%backend-error-of form)))
      (fiveam:is (typep c 'backend-definition-error) "~S" form)
      (fiveam:is (typep c 'definition-error))
      (fiveam:is (string-equal (string (second form)) (string (definition-error-name c)))))))

(fiveam:test defbackend-clause-heads-and-names-match-across-packages
  (unless (find-package '#:bk-other)
    (make-package '#:bk-other :use '(#:cl)))
  (let ((*package* (find-package '#:bk-other)))
    (import 'defbackend '#:bk-other)
    (eval (read-from-string
           "(defbackend bk-other-abi (:machine callfoo)
              (registers :return (a) :stack-pointer sp)
              (operands (reg call-reg) (imm call-imm))
              (ops (:ret () (ret)) (:push (v) (push (imm v)))))")))
  (let ((backend (find-backend 'bk-other-abi)))
    (fiveam:is (eq 'callfoo (backend-descriptor-machine backend)))
    (fiveam:is (eq 'call-reg (cdr (assoc "REG" (backend-descriptor-operands backend) :test #'string=))))
    (fiveam:is (equalp (assembly-cells (assemble "push #7
ret" :machine 'callfoo))
                       (assembly-cells (assemble-items '((:op :push 7) (:op :ret)) :backend 'bk-other-abi))))))

;;; Assembling items

(fiveam:test items-assemble-to-the-cells-of-the-equivalent-source
  (let ((from-items (assemble-items *double-items* :backend 'callfoo-abi))
        (from-source (assemble *double-source* :machine 'callfoo)))
    (fiveam:is (equalp (assembly-cells from-source) (assembly-cells from-items)))
    (fiveam:is (equalp (assembly-symbols from-source) (assembly-symbols from-items)))))

(fiveam:test rendered-items-assemble-to-the-same-cells
  (let* ((text (render-items *double-items* :backend 'callfoo-abi))
         (assembly (assemble-items *double-items* :backend 'callfoo-abi)))
    (fiveam:is (equalp (assembly-cells assembly)
                       (assembly-cells (assemble text :machine 'callfoo))))
    (fiveam:is (string= text (assembly-source assembly)))))

(fiveam:test items-listing-points-at-the-rendered-lines
  (let ((assembly (assemble-items *double-items* :backend 'callfoo-abi)))
    (fiveam:is (search "call double" (listing-text assembly)))))

(fiveam:test items-run-a-call-with-arguments-on-the-stack
  (let ((machine (make-machine 'callfoo))
        (assembly (assemble-items *double-items* :backend 'callfoo-abi)))
    (load-program machine assembly)
    (multiple-value-bind (reason steps) (run machine)
      (fiveam:is (eq :trap reason))
      (fiveam:is (= 7 steps))
      (fiveam:is (= 42 (regref machine 'r 0))))))

(fiveam:test items-need-a-machine-or-a-backend
  (fiveam:signals usage-error (assemble-items '((hlt))))
  (fiveam:is (equalp (assembly-cells (assemble "hlt" :machine 'callfoo))
                     (assembly-cells (assemble-items '((hlt)) :machine 'callfoo))))
  (fiveam:signals usage-error (assemble-items '((hlt)) :backend 'callfoo-abi :machine 'bk-ld-machine)))

(fiveam:test items-accept-a-mode-named-directly-and-bare-expressions
  (let ((direct (assemble-items '((ldi (:mode call-reg b) (:mode call-imm 5))) :machine 'callfoo))
        (bare (assemble-items '((call 6) (call (+ 2 4)) (call (- 0 1))) :machine 'callfoo)))
    (fiveam:is (equalp (assembly-cells (assemble "ldi b, #5" :machine 'callfoo)) (assembly-cells direct)))
    (fiveam:is (equalp (assembly-cells (assemble "call 6
call (2 + 4)
call (0 - 1)" :machine 'callfoo))
                       (assembly-cells bare)))))

(fiveam:test items-fold-expressions-labels-and-negative-numbers
  (fiveam:is (equalp (assembly-cells (assemble "start:
.word (start + 5), -3, (2 * (1 + 1))
call (end + 0)
end:" :machine 'callfoo))
                     (assembly-cells (assemble-items '((:label start)
                                                       (:directive ".word" (+ start 5) (- 3) (* 2 (+ 1 1)))
                                                       (call (+ end 0))
                                                       (:label end))
                                                     :machine 'callfoo)))))

(fiveam:test items-directives-take-strings-and-a-dot-is-optional
  (fiveam:is (equalp (assembly-cells (assemble ".word 1, 2" :machine 'callfoo))
                     (assembly-cells (assemble-items '((:directive word 1 2)) :machine 'callfoo))))
  (fiveam:is (equalp (assembly-cells (assemble ".ascii \"hi\"" :machine 'callfoo))
                     (assembly-cells (assemble-items '((:directive ".ascii" "hi")) :machine 'callfoo)))))

(fiveam:test items-names-fold-symbols-and-keep-strings
  (let ((symbols (assemble-items '((:label |Mixed|) (:label plain) (:label "Exact")) :machine 'callfoo)))
    (fiveam:is (equal '("Exact" "Mixed" "plain")
                      (sort (loop for name being the hash-keys of (assembly-symbols symbols) collect name)
                            #'string<)))))

(fiveam:test items-local-labels-follow-the-lexer
  (let ((assembly (assemble-items '((:label outer) (:label .inner) (call .inner)) :machine 'callfoo)))
    (fiveam:is (equalp (assembly-cells (assemble "outer:
.inner:
call .inner" :machine 'callfoo))
                       (assembly-cells assembly)))))

(fiveam:test backend-ops-substitute-arguments
  (fiveam:is (equal '((add (reg a) (reg b))) (backend-expand-op 'callfoo-abi :add '((reg a) (reg b)))))
  (fiveam:is (equal '((ldi (reg a) (imm 3))) (backend-expand-op 'callfoo-abi 'load '((reg a) 3))))
  (fiveam:is (equalp (assembly-cells (assemble "ldi c, #9" :machine 'callfoo))
                     (assembly-cells (assemble-items '((:op :load (reg c) 9)) :backend 'callfoo-abi)))))

;;; Malformed items

(fiveam:test malformed-items-signal-items-malformed
  (dolist (items '(((:op :nope)) ((:op :add (reg a))) ((:label)) ((:label 5)) ((:directive))
                   ((:frobnicate)) (5) (((nested))) ((call (nope 1))) ((ldi (reg a) (imm)))
                   ((ldi (reg a) (imm 1 2))) ((push (mode call-imm 1))) ((push (:mode no-such 1)))
                   ((push (:other 1))) ((call (* 1))) ((call (nope-op 1 2))) ((call 1.5))
                   ((call :key))))
    (fiveam:is (typep (%items-error-of items :backend 'callfoo-abi) 'items-malformed) "~S" items)))

(fiveam:test items-operands-need-a-backend-for-kinds-and-ops
  (fiveam:is (typep (%items-error-of '((ldi (reg a) (imm 1))) :machine 'callfoo) 'items-malformed))
  (fiveam:is (typep (%items-error-of '((:op :ret)) :machine 'callfoo) 'items-malformed)))

(fiveam:test an-operand-that-does-not-match-its-mode-is-an-error
  (fiveam:is (typep (%items-error-of '((ldi (reg 5) (imm 1))) :backend 'callfoo-abi) 'items-operand-mismatch))
  (fiveam:is (null (%items-error-of '((ldi (reg a) (imm 1))) :backend 'callfoo-abi))))

(fiveam:test an-operand-another-alternative-would-win-is-an-error
  (fiveam:is (null (%items-error-of '((ld (reg a) (ind b))) :backend 'bk-ld-abi)))
  (fiveam:is (null (%items-error-of '((ld (reg a) (abs 100))) :backend 'bk-ld-abi)))
  (fiveam:is (null (%items-error-of '((ld (reg a) (imm 100))) :backend 'bk-ld-abi)))
  (let ((c (%items-error-of '((ld (reg a) (abs b))) :backend 'bk-ld-abi)))
    (fiveam:is (typep c 'items-operand-mismatch))
    (fiveam:is (search "BK-LD-IND" (items-error-detail c)))
    (fiveam:is (equal '(ld (reg a) (abs b)) (items-error-item c)))))

(defmode bk-slot-mode (one-of bk-ld-abs bk-ld-imm) "," (one-of bk-ld-ind bk-ld-reg))

(definstruction bk-ld-machine ld2
  (modes bk-slot-mode)
  (encoding (opcode 2) (operand dst :width 1) (operand src :width 1))
  (semantics (set! (r src) dst)))

(defmode bk-tie-first "x" expr)
(defmode bk-tie-second expr "x")
(defmode bk-tie-mode (one-of bk-tie-first bk-tie-second))

(definstruction bk-ld-machine tie
  (modes bk-tie-mode)
  (encoding (opcode 3) (operand v :width 1))
  (semantics (set! (r v) 0)))

(fiveam:test an-alternative-that-only-exists-in-another-slot-is-not-a-rival
  (fiveam:is (null (%items-error-of '((ld2 (:mode bk-ld-abs b) (:mode bk-ld-reg c))) :machine 'bk-ld-machine)))
  (fiveam:is (null (%items-error-of '((ld2 (:mode bk-ld-abs 5) (:mode bk-ld-ind c))) :machine 'bk-ld-machine))))

(fiveam:test a-nested-alternative-the-assembler-did-not-pick-is-an-error
  (let ((c (%items-error-of '((ld (:mode bk-ld-mode a bk-ld-abs b))) :machine 'bk-ld-machine)))
    (fiveam:is (typep c 'items-operand-mismatch))
    (fiveam:is (search "BK-LD-IND" (items-error-detail c)))
    (fiveam:is (search "BK-LD-ABS" (items-error-detail c))))
  (fiveam:is (null (%items-error-of '((ld (:mode bk-ld-mode a bk-ld-abs 100))) :machine 'bk-ld-machine))))

(fiveam:test an-alternative-declaration-order-loses-is-an-error
  (handler-bind ((warning #'muffle-warning))
    (fiveam:is (null (%items-error-of '((:label x) (tie (:mode bk-tie-first x))) :machine 'bk-ld-machine)))
    (let ((c (%items-error-of '((:label x) (tie (:mode bk-tie-second x))) :machine 'bk-ld-machine)))
      (fiveam:is (typep c 'items-operand-mismatch))
      (fiveam:is (search "BK-TIE-FIRST" (items-error-detail c))))))

(fiveam:test width-relaxation-between-variants-is-not-a-mismatch
  (let ((assembly (assemble-items '((lda (:mode absolute 5))) :machine 'instr-test-machine)))
    (fiveam:is (eq 'zero-page (mode-descriptor-name
                               (instruction-descriptor-mode
                                (listing-line-descriptor (first (assembly-listing assembly)))))))))

(fiveam:test assembler-errors-point-into-the-rendered-source
  (let ((c (handler-case (assemble-items '((:label x) (call missing)) :machine 'callfoo)
             (lasm-error (c) c))))
    (fiveam:is (typep c 'lasm-error))
    (fiveam:is (search "missing" (princ-to-string (diagnostic-text c))))))

;;; Reading

(defparameter *double-program*
  "(:program (:backend callfoo-abi :origin 4)
     (:label main) (hlt))")

(fiveam:test read-items-reads-a-program-without-interning-symbols
  (let* ((program (read-items-from-string "(:program (:backend callfoo-abi :origin 4) (:label zzqunique) (hlt))")))
    (fiveam:is (string-equal "callfoo-abi" (string (items-program-backend program))))
    (fiveam:is (null (symbol-package (items-program-backend program))))
    (fiveam:is (= 4 (items-program-origin program)))
    (fiveam:is (null (find-symbol "ZZQUNIQUE" '#:lasm)))
    (fiveam:is (null (symbol-package (second (first (items-program-items program))))))
    (fiveam:is (equalp (assembly-cells (assemble "hlt" :machine 'callfoo :origin 4))
                       (assembly-cells (assemble-items (items-program-items program)
                                                       :backend (items-program-backend program)
                                                       :origin (items-program-origin program)))))))

(fiveam:test read-items-rejects-what-is-not-a-program
  (dolist (text '("" "(:program) (:program)" "(:other)" "(:program 5)" "(:program (:nope 1))"
                  "(:program (:origin -1))" "(:program (:origin))" "5" "(:program"
                  "(:program (:backend #.(error \"x\")))" "(:program #1=(a . #1#))"
                  "(:program 'x)" "(:program #+sbcl 1)" "(:program #(1 2) . 3)"
                  "(:program (:backend nosuchpackage:foo))" "(:program :not-a-known-keyword-here)"))
    (fiveam:signals items-malformed (read-items-from-string text))))

(fiveam:test read-items-bounds-nesting-and-number-size
  (fiveam:signals items-malformed
    (read-items-from-string (format nil "(:program ~A~A)" (make-string 5000 :initial-element #\()
                                    (make-string 5000 :initial-element #\)))))
  (fiveam:signals items-malformed
    (read-items-from-string (format nil "(:program (call ~A))"
                                    (make-string (1+ +reader-max-number-chars+) :initial-element #\9)))))

(defun %call-with-items-file (text function)
  (uiop:with-temporary-file (:pathname path :type "lasm" :stream out)
    (write-string text out)
    :close-stream
    (funcall function path)))

(fiveam:test assemble-items-file-reads-and-assembles
  (%call-with-items-file *double-program*
    (lambda (path)
      (let ((assembly (assemble-items-file path)))
        (fiveam:is (= 4 (assembly-origin assembly)))
        (fiveam:is (equalp (assembly-cells (assemble "hlt" :machine 'callfoo)) (assembly-cells assembly))))
      (fiveam:is (= 9 (assembly-origin (assemble-items-file path :origin 9))))
      (fiveam:signals usage-error (assemble-items-file path :machine 'bk-ld-machine)))))

(fiveam:test assemble-items-file-resolves-includes-beside-the-file
  (uiop:with-temporary-file (:pathname included :type "asm" :stream out)
    (write-string "hlt" out)
    :close-stream
    (%call-with-items-file (format nil "(:program (:machine callfoo) (:directive \".include\" ~S))"
                                   (file-namestring included))
      (lambda (path)
        (fiveam:is (equalp (assembly-cells (assemble "hlt" :machine 'callfoo))
                           (assembly-cells (assemble-items-file path))))))))

(fiveam:test items-file-assemblies-snapshot-and-rebuild
  (%call-with-items-file (format nil "(:program (:backend callfoo-abi) ~{~S~^ ~})" *double-items*)
    (lambda (path)
      (let* ((assembly (assemble-items-file path))
             (snapshot (machine-snapshot (make-machine 'callfoo) :assembly assembly))
             (rebuilt (snapshot-assembly snapshot :machine 'callfoo)))
        (fiveam:is (equalp (assembly-cells assembly) (assembly-cells rebuilt)))
        (fiveam:is (string= (assembly-source assembly) (assembly-source rebuilt)))))))

(fiveam:test the-shared-reader-serves-snapshots-unchanged
  (with-input-from-string (in "(:lasm-snapshot :machine)")
    (fiveam:is (equal '(:lasm-snapshot :machine)
                      (read-restricted-form in (lambda (control &rest args)
                                                 (apply #'%snapshot-fail 'snapshot-malformed control args))
                                            "test")))))

(fiveam:test a-one-of-mode-takes-its-alternative-name-first
  (fiveam:is (equalp (assembly-cells (assemble "ld a, [b]" :machine 'bk-ld-machine))
                     (assembly-cells (assemble-items '((ld (reg a) (:mode bk-ld-ind b))) :backend 'bk-ld-abi))))
  (fiveam:is (equalp (assembly-cells (assemble "ld a, [b]" :machine 'bk-ld-machine))
                     (assembly-cells (assemble-items '((ld (:mode bk-ld-mode a bk-ld-ind b))) :machine 'bk-ld-machine))))
  (fiveam:is (typep (%items-error-of '((ld (:mode bk-ld-mode a nope b))) :machine 'bk-ld-machine)
                    'items-malformed)))
