;;;; tests/static-items.lisp
;;;; #456: a (:function ...) whose locals are labelled words, not stack slots. zpfoo
;;;; (tests/fixtures/cli/zpfoo.lisp) addresses a word by two zero-page cells, so its
;;;; static words are placed in page zero with (:static-frames).

(in-package #:lasm)

(fiveam:def-suite static-items :in lasm)
(fiveam:in-suite static-items)

(defparameter +zs-ops+
  '((:poke-label (label s) (lda (:lo s)) (sta (zp label)) (lda (:hi s)) (sta (zp (+ label 1))))
    (:peek-label (d label) (lda (zp label)) (sta (:lo d)) (lda (zp (+ label 1))) (sta (:hi d)))))

;; With stack operations too, so a function asks for static frames itself.
(eval `(defbackend zs-label-abi (:extends zpfoo-lang-abi)
         (frame :static t :label-slot zp)
         (ops ,@+zs-ops+)))

;; No stack operations: a function is static unless it asks for a stack.
(eval `(defbackend zs-bare-abi (:isa zpfoo)
         (registers :pairs ((w0 #x11 #x10) (w1 #x13 #x12) (w2 #x15 #x14))
                    :return (w0) :scratch (w0 w1) :callee-saved (w2)
                    :stack-pointer sp :program-counter pc :operand zp)
         (call :args (w1) :return-address-slots 1)
         (frame :static t :label-slot zp)
         (operands (zp zf-zp) (imm zf-imm))
         (ops (:move (d s) (lda (:lo s)) (sta (:lo d)) (lda (:hi s)) (sta (:hi d)))
              (:const (r v) (ldi (imm (:lo v))) (sta (:lo r)) (ldi (imm (:hi v))) (sta (:hi r)))
              (:add (d s) (clc) (lda (:lo d)) (adc (:lo s)) (sta (:lo d)) (lda (:hi d)) (adc (:hi s)) (sta (:hi d)))
              (:call (f) (call f))
              (:return () (ret))
              (:halt () (hlt))
              ,@+zs-ops+)))

(defparameter +zs-words+
  '((:directive org 64) (:static-frames) (:directive org 512))
  "Reserves the static words in page zero, then starts the code at #x200.")

(defun %zs-run (items backend &key frames)
  "Assemble +ZS-WORDS+ and ITEMS, run them on zpfoo from #x200 and return the machine and assembly."
  (let ((m (make-machine 'zpfoo))
        (assembly (assemble-items (append +zs-words+ items) :backend backend :frames frames)))
    (load-program m assembly)
    (setf (sref m 'pc) #x200
          (sref m 'sp) #xff00)
    (run m :max-steps 4000)
    (values m assembly)))

(defun %zs-word (m low)
  (+ (mref m 'ram low) (* 256 (mref m 'ram (1+ low)))))

(fiveam:test a-static-local-holds-a-word-through-an-operation
  (let ((items '((:call sum) (:op :halt)
                 (:function sum (:locals 2)
                   (:op :const (zp w0) 35)
                   (:op :move (:local 0) (zp w0))
                   (:op :const (zp w0) 7)
                   (:op :move (:local 1) (zp w0))
                   (:op :move (zp w0) (:local 0))
                   (:op :add (zp w0) (:local 1))
                   (:return)))))
    (fiveam:is (= 42 (%zs-word (%zs-run items 'zs-label-abi :frames :static) #x10)))
    (fiveam:is (= 42 (%zs-word (%zs-run items 'zs-bare-abi) #x10)))))

(fiveam:test a-half-of-a-static-local-is-a-cell-of-its-word
  (let ((m (%zs-run '((:call f) (:op :halt)
                      (:function f (:locals 1)
                        (:op :const (zp w0) #x1234)
                        (:op :move (:local 0) (zp w0))
                        (lda (:lo (:local 0)))
                        (sta (zp #x40))
                        (lda (:hi (:local 0)))
                        (sta (zp #x41))
                        (:return)))
                    'zs-bare-abi)))
    (fiveam:is (= #x34 (mref m 'ram #x40)))
    (fiveam:is (= #x12 (mref m 'ram #x41)))))

(fiveam:test a-static-local-is-a-call-argument
  (let ((m (%zs-run '((:call outer 20) (:op :halt)
                      (:function outer (:args 1 :locals 1)
                        (:op :move (:local 0) (:arg 0))
                        (:call inner (:local 0))
                        (:return))
                      (:function inner (:args 1)
                        (:op :const (zp w0) 2)
                        (:op :add (zp w0) (:arg 0))
                        (:return)))
                    'zs-bare-abi)))
    (fiveam:is (= 22 (%zs-word m #x10)))))

(fiveam:test a-static-function-saves-its-registers-in-words
  (let ((m (%zs-run '((:op :const (zp w2) 99) (:call clobber) (:op :halt)
                      (:function clobber (:save (w2))
                        (:op :const (zp w2) 1)
                        (:return)))
                    'zs-bare-abi)))
    (fiveam:is (= 99 (%zs-word m #x14)))))

(fiveam:test static-words-follow-the-function-without-a-placement-item
  (let ((symbols (assembly-symbols (assemble-items '((:function f (:locals 2) (:return))
                                                     (:label after))
                                                   :backend 'zs-bare-abi :origin #x200))))
    (fiveam:is (= #x201 (gethash "sffx0" symbols)))
    (fiveam:is (= #x203 (gethash "sffx1" symbols)))
    (fiveam:is (= #x205 (gethash "after" symbols)))))

(fiveam:test static-words-are-spliced-at-the-placement-item
  (let ((symbols (assembly-symbols (assemble-items '((:static-frames)
                                                     (:function f (:locals 1) (:return))
                                                     (:function g (:locals 2) (:return)))
                                                   :backend 'zs-bare-abi :origin 0))))
    (fiveam:is (= 0 (gethash "sffx0" symbols)))
    (fiveam:is (= 2 (gethash "sfgx0" symbols)))
    (fiveam:is (= 4 (gethash "sfgx1" symbols)))
    (fiveam:is (= 6 (gethash "f" symbols)))))

(fiveam:test the-function-option-overrides-the-frames-choice
  (let ((stack '((:function f (:locals 1 :frames stack) (:return))))
        (static '((:function f (:locals 1 :frames static) (:return)))))
    (fiveam:is (search "subs" (render-items stack :backend 'zs-label-abi)))
    (fiveam:is (null (search "subs" (render-items static :backend 'zs-label-abi))))
    (fiveam:is (search "subs" (render-items stack :backend 'zs-label-abi :frames :static)))
    (fiveam:is (null (search "subs" (render-items static :backend 'zs-label-abi :frames :stack))))))

(fiveam:test the-frames-choice-and-the-backend-pick-static-frames
  (let ((items '((:function f (:locals 1) (:return)))))
    (fiveam:is (search "subs" (render-items items :backend 'zs-label-abi)))
    (fiveam:is (null (search "subs" (render-items items :backend 'zs-label-abi :frames :static))))
    (fiveam:is (search "sffx0" (render-items items :backend 'zs-bare-abi)))
    (fiveam:signals usage-error (render-items items :backend 'zs-bare-abi :frames :heap))))

(defun %zs-error (items backend)
  (handler-case (progn (assemble-items items :backend backend) nil)
    (items-malformed (c) (princ-to-string c))))

(fiveam:test a-static-function-reports-what-it-cannot-do
  (fiveam:is (search ":label-slot"
                     (%zs-error '((:function f (:locals 1 :frames static) (:op :move (zp w0) (:local 0)) (:return)))
                                'zpfoo-lang-abi)))
  (fiveam:is (search "at most 1 argument"
                     (%zs-error '((:function f (:args 2) (:return))) 'zs-bare-abi)))
  (fiveam:is (search "static"
                     (%zs-error '((:function f (:frame t :frames static) (:return))) 'zs-label-abi)))
  (fiveam:is (search "static or stack"
                     (%zs-error '((:function f (:frames heap) (:return))) 'zs-label-abi)))
  (fiveam:is (search "top level"
                     (%zs-error '((:function f () (:static-frames) (:return))) 'zs-bare-abi)))
  (fiveam:is (search "at most one"
                     (%zs-error '((:static-frames) (:static-frames)) 'zs-bare-abi)))
  (fiveam:is (search "has 1 local"
                     (%zs-error '((:function f (:locals 1) (lda (:lo (:local 1))) (:return))) 'zs-bare-abi))))

(fiveam:test a-program-header-and-a-file-choose-static-frames
  (let ((text "(:program (:backend zs-label-abi :frames static :origin 512)
  (:function f (:locals 1) (:return)))"))
    (fiveam:is (eq :static (items-program-frames (read-items-from-string text))))
    (%call-with-items-file text
      (lambda (path)
        (fiveam:is (null (search "subs" (assembly-source (assemble-items-file path)))))
        (fiveam:is (search "subs" (assembly-source (assemble-items-file path :frames :stack))))))))
