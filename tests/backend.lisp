;;;; tests/backend.lisp
;;;; #113: DEFBACKEND, structured items and .lasm programs.

(in-package #:lasm)

(fiveam:def-suite backend :in lasm)
(fiveam:in-suite backend)

;;; Fixture: examples/cli/callfoo-fp.lisp, which loads callfoo.lisp (the machine
;;; and backend behind double.lasm) and adds the frame-pointer family member,
;;; plus a machine whose LD takes a ONE-OF operand.

(let ((*package* (find-package '#:lasm)))
  (load (asdf:system-relative-pathname :lasm "examples/cli/callfoo-fp.lisp")))

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
    (fiveam:is (equal '(:grows :down :alignment 1 :slot "SP-IDX") (backend-descriptor-frame backend)))
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

(fiveam:test force-writes-the-mode-suffix-and-stops-relaxation
  (let* ((items '((lda (:force (:mode absolute 5)))))
         (assembly (assemble-items items :machine 'instr-test-machine)))
    (fiveam:is (eq 'absolute (mode-descriptor-name
                              (instruction-descriptor-mode
                               (listing-line-descriptor (first (assembly-listing assembly)))))))
    (fiveam:is (search "lda.w 5" (render-items items :machine 'instr-test-machine)))
    (fiveam:is (< (items-size '((lda (:mode absolute 5))) :machine 'instr-test-machine)
                  (items-size items :machine 'instr-test-machine)))))

(fiveam:test force-is-malformed-when-it-cannot-name-one-variant
  (flet ((detail (items &rest keys)
           (let ((c (apply #'%items-error-of items :machine 'instr-test-machine keys)))
             (and (typep c 'items-malformed) (items-error-detail c)))))
    (fiveam:is (search "no suffix" (detail '((lda (:force (:mode immediate 5)))))))
    (fiveam:is (search "not a mode of" (detail '((ldx (:force (:mode absolute 5)))))))
    (fiveam:is (search "only operand" (detail '((lda (:force (:mode absolute 5)) 1)))))
    (fiveam:is (search "expected (:force" (detail '((lda (:force 5))))))
    (fiveam:is (search "expected (:force" (detail '((lda (:force (:force (:mode absolute 5))))))))
    (fiveam:is (search "no mode suffix" (detail '((lda (:force (:mode absolute 5))))
                                                :lexer 'bk-no-underscore-syntax)))))

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

;;; Inheritance (#323)

(eval '(defmachine (bk-noret-2 (:extends callfoo))
        (without-instructions ret)))

(fiveam:test an-extending-backend-merges-the-parent-clauses-by-key
  (let ((child (find-backend 'callfoo-fp-abi)))
    (fiveam:is (eq 'callfoo-fp (backend-descriptor-machine child)))
    (fiveam:is (eq 'callfoo-abi (backend-descriptor-parent child)))
    (fiveam:is (equal '("A") (backend-register child :return)))
    (fiveam:is (equal "FP" (backend-register child :frame-pointer)))
    (fiveam:is (equal '(:grows :down :alignment 1 :slot "FP-IDX" :pointer "FP") (backend-descriptor-frame child)))
    (fiveam:is (equal (backend-descriptor-call (find-backend 'callfoo-abi)) (backend-descriptor-call child)))
    (fiveam:is (equal '("REG" "IMM" "SP-IDX" "SP" "FP-IDX")
                      (mapcar #'car (backend-descriptor-operands child))))
    (fiveam:is (assoc "ADD" (backend-descriptor-ops child) :test #'string=))
    (fiveam:is (assoc "ENTER" (backend-descriptor-ops child) :test #'string=))))

(fiveam:test an-extending-backend-replaces-list-roles-and-ops-and-defaults-the-machine
  (eval '(defbackend bk-child-1 (:extends callfoo-abi)
          (registers :callee-saved (d))
          (ops (:add (d s) (add s d)))))
  (let ((child (find-backend 'bk-child-1)))
    (fiveam:is (eq 'callfoo (backend-descriptor-machine child)))
    (fiveam:is (equal '("D") (backend-register child :callee-saved)))
    (fiveam:is (equal '("A") (backend-register child :return)))
    (fiveam:is (equal '((add s d)) (cddr (assoc "ADD" (backend-descriptor-ops child) :test #'string=))))
    (fiveam:is (assoc "CALL" (backend-descriptor-ops child) :test #'string=))
    (fiveam:is (equal '((add d s))
                      (cddr (assoc "ADD" (backend-descriptor-ops (find-backend 'callfoo-abi)) :test #'string=))))))

(fiveam:test an-extending-backend-takes-its-options-in-either-order-and-assembles-inherited-ops
  (eval '(defbackend bk-child-2 (:machine callfoo-fp :extends callfoo-abi)))
  (fiveam:is (eq 'callfoo-fp (backend-descriptor-machine (find-backend 'bk-child-2))))
  (fiveam:is (equalp (assembly-cells (assemble-items *double-items* :backend 'callfoo-abi))
                     (assembly-cells (assemble-items *double-items* :backend 'bk-child-2)))))

(fiveam:test a-child-backend-is-checked-against-its-own-machine
  (let ((c (%backend-error-of '(defbackend bk-child-3 (:extends callfoo-abi :machine bk-noret-2)))))
    (fiveam:is (typep c 'backend-definition-error))
    (fiveam:is (search "no instruction ret" (string-downcase (princ-to-string c)))))
  (fiveam:is (null (%backend-error-of '(defbackend bk-child-4 (:extends callfoo-abi :machine bk-noret-2)
                                        (ops (:return () (hlt)))))))
  (fiveam:is (null (%backend-error-of '(defbackend bk-child-5 (:extends callfoo-abi :machine bk-noret-2)
                                        (without-ops :return)))))
  (fiveam:is (null (assoc "RETURN" (backend-descriptor-ops (find-backend 'bk-child-5)) :test #'string=))))

(fiveam:test extending-backends-signal-typed-definition-errors
  (dolist (form '((defbackend bk-child-e1 (:extends no-such-backend))
                  (defbackend bk-child-e2 (:extends callfoo-abi :machine bk-ld-machine))
                  (defbackend bk-child-e3 (:extends callfoo-abi :machine no-such-machine))
                  (defbackend bk-child-e4 (:extends callfoo-abi :extends callfoo-abi))
                  (defbackend bk-child-e5 (:extends callfoo-abi) (without-ops no-such-op))
                  (defbackend bk-child-e6 (:machine callfoo) (without-ops add))
                  (defbackend bk-child-e7 (:extends callfoo-abi) (without-ops add) (without-ops add))
                  (defbackend bk-child-e8 (:extends callfoo-abi) (call :cleanup :nobody))
                  (defbackend bk-child-e9 (:extends callfoo-abi) (registers :bogus (a)))
                  (defbackend callfoo-abi (:extends callfoo-abi))))
    (fiveam:is (typep (%backend-error-of form) 'backend-definition-error) "~S" form)))

(fiveam:test a-child-backend-does-not-follow-later-changes-to-its-parent
  (eval '(defbackend bk-snap-parent (:machine callfoo) (registers :return (a))))
  (eval '(defbackend bk-snap-child (:extends bk-snap-parent)))
  (eval '(defbackend bk-snap-parent (:machine callfoo) (registers :return (b))))
  (fiveam:is (equal '("A") (backend-register 'bk-snap-child :return))))

;;; The frame pointer declaration (#321)

(fiveam:test defbackend-reconciles-the-frame-pointer-role-and-declaration
  (eval '(defbackend bk-fp-1 (:extends callfoo-abi :machine callfoo-fp) (frame :pointer fp)))
  (fiveam:is (equal "FP" (backend-register 'bk-fp-1 :frame-pointer)))
  (eval '(defbackend bk-fp-2 (:extends callfoo-abi :machine callfoo-fp) (registers :frame-pointer fp)))
  (fiveam:is (null (getf (backend-descriptor-frame (find-backend 'bk-fp-2)) :pointer)))
  (dolist (form '((defbackend bk-fp-e1 (:extends callfoo-abi :machine callfoo-fp)
                    (registers :frame-pointer fp) (frame :pointer sp))
                  (defbackend bk-fp-e2 (:extends callfoo-abi :machine callfoo-fp) (frame :pointer nope))
                  (defbackend bk-fp-e3 (:extends callfoo-abi :machine callfoo-fp) (frame :pointer sp))
                  (defbackend bk-fp-e4 (:extends callfoo-abi :machine callfoo-fp) (frame :pointer a))
                  (defbackend bk-fp-e5 (:extends callfoo-abi :machine callfoo-fp)
                    (call :args (a b)) (frame :pointer b))))
    (fiveam:is (typep (%backend-error-of form) 'backend-definition-error) "~S" form)))

;;; Sizing and labels in operations (#324, #325)

(defmachine bk-br-machine
  (register pc :width 16)
  (register r :width 16 :names (a b))
  (memory ram :width 8 :addr-width 16))

(defmode bk-br-reg (expr :register r))

(definstruction bk-br-machine tst
  (modes bk-br-reg)
  (encoding (opcode 1) (operand x :width 1))
  (semantics nil))

(definstruction bk-br-machine jz
  (modes relative)
  (encoding (opcode 2) (operand :mode))
  (semantics nil))

(definstruction bk-br-machine br
  (modes
    (relative (opcode 3) (semantics nil))
    (absolute (opcode 4) (semantics nil))))

(definstruction bk-br-machine nop
  (encoding (opcode 5))
  (semantics nil))

(defbackend bk-br-abi (:machine bk-br-machine)
  (operands (reg bk-br-reg))
  (ops (:cjz (r target) (tst r) (jz skip) (br target) (:label skip))
       (:twice () (br again) (:label again) (br again2) (:label again2))))

(defbackend bk-br-child-abi (:extends bk-br-abi)
  (ops (:nothing () (nop))))

(defun %cell-count (items &rest keys)
  (length (assembly-cells (apply #'assemble-items items keys))))

(fiveam:test items-size-equals-the-assembled-size-of-a-closed-program
  (let ((items '((br end) (nop) (:label end) (nop))))
    (fiveam:is (= (%cell-count items :machine 'bk-br-machine)
                  (items-size items :machine 'bk-br-machine)))
    (fiveam:is (= 4 (items-size items :machine 'bk-br-machine)))))

(fiveam:test items-size-sizes-a-label-the-items-never-define-by-assume
  (let ((items '((br elsewhere) (nop))))
    (fiveam:is (= 4 (items-size items :machine 'bk-br-machine)))
    (fiveam:is (= 4 (items-size items :machine 'bk-br-machine :assume :widest)))
    (fiveam:is (= 3 (items-size items :machine 'bk-br-machine :assume :narrowest)))))

(fiveam:test items-size-keeps-a-branch-to-a-label-the-items-define-short
  (fiveam:is (= 3 (items-size '((br next) (:label next) (nop)) :machine 'bk-br-machine :assume :widest))))

(fiveam:test items-size-counts-reserved-space-and-honours-the-origin
  (fiveam:is (= 6 (items-size '((:directive res 5) (nop)) :machine 'bk-br-machine)))
  (fiveam:is (= 2 (items-size '((nop) (nop)) :machine 'bk-br-machine :origin 300))))

(fiveam:test items-size-rejects-an-unknown-assumption
  (fiveam:signals usage-error (items-size '((nop)) :machine 'bk-br-machine :assume :middle)))

(fiveam:test items-size-signals-items-errors
  (fiveam:signals items-malformed (items-size '((:op :nosuch)) :backend 'bk-br-abi)))

(fiveam:test items-size-of-a-backend-program-matches-assembling-it
  (let ((items '((:op :cjz (reg a) done) (nop) (:label done) (nop))))
    (fiveam:is (= (%cell-count items :backend 'bk-br-abi)
                  (items-size items :backend 'bk-br-abi)))))

(defun %cjz-items ()
  '((:label start) (:op :cjz (reg a) start) (:op :cjz (reg b) start) (nop)))

(fiveam:test an-operation-label-is-unique-to-each-expansion
  (let* ((source (render-items (%cjz-items) :backend 'bk-br-abi))
         (assembly (assemble-items (%cjz-items) :backend 'bk-br-abi)))
    (fiveam:is (search ".skip__LASM_1:" source))
    (fiveam:is (search ".skip__LASM_2:" source))
    (fiveam:is (equalp (assembly-cells assembly)
                       (assembly-cells (assemble source :machine 'bk-br-machine))))
    (fiveam:is (equalp #(1 0 2 2 3 250 1 1 2 2 3 244 5) (assembly-cells assembly)))))

(fiveam:test an-operation-label-is-global-before-any-global-label
  (let ((source (render-items '((:op :cjz (reg a) end) (:label end) (nop)) :backend 'bk-br-abi)))
    (fiveam:is (search "skip__LASM_1:" source))
    (fiveam:is (not (search ".skip__LASM_1" source)))))

(fiveam:test an-operation-label-leaves-a-users-local-label-in-scope
  (let ((items '((:label main) (:label .again) (:op :cjz (reg a) main) (br .again))))
    (fiveam:is (= (items-size items :backend 'bk-br-abi)
                  (%cell-count items :backend 'bk-br-abi)))))

(fiveam:test an-argument-named-like-an-operation-label-is-not-captured
  (let* ((items '((:label skip) (:op :cjz (reg a) skip) (nop)))
         (source (render-items items :backend 'bk-br-abi)))
    (fiveam:is (search "br skip" source))
    (fiveam:is (search "jz .skip__LASM_1" source))
    (fiveam:is (= 7 (%cell-count items :backend 'bk-br-abi)))))

(fiveam:test an-operation-with-several-labels-names-each
  (let ((source (render-items '((:label f) (:op :twice)) :backend 'bk-br-abi)))
    (fiveam:is (search ".again__LASM_1:" source))
    (fiveam:is (search ".again2__LASM_2:" source))))

(fiveam:test generated-labels-avoid-names-the-items-use
  (let ((source (render-items '((:label f) (:label ".skip__LASM_1") (:op :cjz (reg a) f)) :backend 'bk-br-abi)))
    (fiveam:is (search ".skip__LASM_2:" source))))

(fiveam:test an-inherited-operation-keeps-its-labels
  (let ((source (render-items '((:label f) (:op :cjz (reg a) f)) :backend 'bk-br-child-abi)))
    (fiveam:is (search ".skip__LASM_1:" source))))

(fiveam:test a-lowering-operation-may-define-a-label
  (eval '(defbackend bk-br-hook-abi (:extends callfoo-abi)
          (ops (:return () (:label done) (ret)))))
  (let ((source (render-items '((:function f (:args 0) (:return))) :backend 'bk-br-hook-abi)))
    (fiveam:is (search ".done__LASM_1:" source))))

(fiveam:test backend-expand-op-returns-label-forms-unrenamed
  (fiveam:is (equal '((tst (reg a)) (jz skip) (br (reg b)) (:label skip))
                    (backend-expand-op 'bk-br-abi :cjz '((reg a) (reg b))))))

(fiveam:test defbackend-checks-operation-labels
  (dolist (ops '(((:op (r) (tst r) (:label x) (:label x)))
                 ((:op (r) (tst r) (:label r)))
                 ((:op (r) (tst r) (:label)))
                 ((:op (r) (tst r) (:label :x)))
                 ((:op (r) (jz nowhere)))))
    (fiveam:is (typep (%backend-error-of `(defbackend bk-br-bad (:machine bk-br-machine)
                                            (operands (reg bk-br-reg))
                                            (ops ,@ops)))
                      'backend-definition-error))))

(deflexer bk-no-underscore-syntax
  (comment-styles (";" :line))
  (number-formats (:dec :default))
  (label-suffix ":")
  (local-label-prefix ".")
  (ident-chars :alnum "."))

(fiveam:test generated-labels-re-lex-under-a-lexer-without-underscore
  (let* ((items '((:label f) (:op :cjz (reg a) f) (nop)))
         (source (render-items items :backend 'bk-br-abi :lexer 'bk-no-underscore-syntax)))
    (fiveam:is (search ".skipLASM1:" source))
    (fiveam:is (equalp (assembly-cells (assemble-items items :backend 'bk-br-abi :lexer 'bk-no-underscore-syntax))
                       (assembly-cells (assemble source :machine 'bk-br-machine
                                                        :lexer 'bk-no-underscore-syntax))))))
