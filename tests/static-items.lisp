;;;; tests/static-items.lisp
;;;; #456: a (:function ...) whose locals are labelled words, not stack slots. zpfoo
;;;; (tests/fixtures/cli/zpfoo.lisp) addresses a word by two zero-page cells, so its
;;;; static words are placed in page zero with (:static-frames).

(in-package #:lasm)

(fiveam:def-suite static-items :in lasm)
(fiveam:in-suite static-items)

;; No stack operations: a function is static unless it asks for a stack.
(defbackend zs-bare-abi (:isa zpfoo)
  (registers :words ((w0 #x11 #x10) (w1 #x13 #x12) (w2 #x15 #x14))
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
       (:poke-label (label s) (lda (:lo s)) (sta (zp label)) (lda (:hi s)) (sta (zp (+ label 1))))
       (:peek-label (d label) (lda (zp label)) (sta (:lo d)) (lda (zp (+ label 1))) (sta (:hi d)))))

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
    (fiveam:is (= 42 (%zs-word (%zs-run items 'zpfoo-label-abi :frames :static) #x10)))
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
    (fiveam:is (= 0 (gethash "sfgx0" symbols)))
    (fiveam:is (= 2 (gethash "sfgx1" symbols)))
    (fiveam:is (= 4 (gethash "f" symbols)))))

(fiveam:test the-function-option-overrides-the-frames-choice
  (let ((stack '((:function f (:locals 1 :frames stack) (:return))))
        (static '((:function f (:locals 1 :frames static) (:return)))))
    (fiveam:is (search "subs" (render-items stack :backend 'zpfoo-label-abi)))
    (fiveam:is (null (search "subs" (render-items static :backend 'zpfoo-label-abi))))
    (fiveam:is (search "subs" (render-items stack :backend 'zpfoo-label-abi :frames :static)))
    (fiveam:is (null (search "subs" (render-items static :backend 'zpfoo-label-abi :frames :stack))))))

(fiveam:test the-frames-choice-and-the-backend-pick-static-frames
  (let ((items '((:function f (:locals 1) (:return)))))
    (fiveam:is (search "subs" (render-items items :backend 'zpfoo-label-abi)))
    (fiveam:is (null (search "subs" (render-items items :backend 'zpfoo-label-abi :frames :static))))
    (fiveam:is (search "sffx0" (render-items items :backend 'zs-bare-abi)))
    (fiveam:signals usage-error (render-items items :backend 'zs-bare-abi :frames :heap))))

(defun %zs-error (items backend)
  (handler-case (progn (assemble-items items :backend backend) nil)
    (items-malformed (c) (princ-to-string c))))

(fiveam:test a-static-function-reports-what-it-cannot-do
  (fiveam:is (search ":label-slot"
                     (%zs-error '((:function f (:locals 1 :frames static) (:op :move (zp w0) (:local 0)) (:return)))
                                'zpfoo-lang-abi)))
  (fiveam:is (search "f takes 1 argument past the registers, the call passes 2"
                     (%zs-error '((:function f (:args 2) (:return)) (:call f 1 2 3)) 'zs-bare-abi)))
  (fiveam:is (search "static"
                     (%zs-error '((:function f (:frame t :frames static) (:return))) 'zpfoo-label-abi)))
  (fiveam:is (search "static or stack"
                     (%zs-error '((:function f (:frames heap) (:return))) 'zpfoo-label-abi)))
  (fiveam:is (search "top level"
                     (%zs-error '((:function f () (:static-frames) (:return))) 'zs-bare-abi)))
  (fiveam:is (search "at most one"
                     (%zs-error '((:static-frames) (:static-frames)) 'zs-bare-abi)))
  (fiveam:is (search "has 1 local"
                     (%zs-error '((:function f (:locals 1) (lda (:lo (:local 1))) (:return))) 'zs-bare-abi))))

(fiveam:test a-program-header-and-a-file-choose-static-frames
  (let ((text "(:program (:backend zpfoo-label-abi :frames static :origin 512)
  (:function f (:locals 1) (:return)))"))
    (fiveam:is (eq :static (items-program-frames (read-items-from-string text))))
    (%call-with-items-file text
      (lambda (path)
        (fiveam:is (null (search "subs" (assembly-source (assemble-items-file path)))))
        (fiveam:is (search "subs" (assembly-source (assemble-items-file path :frames :stack))))))))

;;; #465: a static function that can call itself

(fiveam:test a-static-function-that-calls-itself-is-rejected
  (let ((message (%zs-error '((:function f (:args 1) (:call f (:arg 0)) (:return))) 'zs-bare-abi)))
    (fiveam:is (search "f calls itself" message))
    (fiveam:is (search ":frames stack" message))))

(fiveam:test static-functions-in-a-cycle-are-rejected
  (fiveam:is (search "a calls b calls a is recursive"
                     (%zs-error '((:function a () (:call b) (:return))
                                  (:function b () (:call a) (:return)))
                                'zs-bare-abi)))
  (fiveam:is (search "b calls c calls b is recursive"
                     (%zs-error '((:function a () (:call b) (:return))
                                  (:function b () (:call c) (:return))
                                  (:function c () (:call b) (:return)))
                                'zs-bare-abi))))

(fiveam:test any-mention-of-a-function-is-a-call
  (fiveam:is (search "f calls itself"
                     (%zs-error '((:function f () (:op :call f) (:return))) 'zs-bare-abi)))
  (fiveam:is (search "f calls itself"
                     (%zs-error '((:function f () (jmp f))) 'zs-bare-abi)))
  (fiveam:is (null (%zs-error '((:function f () (:label f2) (jmp f2) (:return))) 'zs-bare-abi))))

(fiveam:test a-cycle-through-a-static-function-is-rejected-but-not-one-of-stack-functions
  (fiveam:is (search "recursive"
                     (%zs-error '((:function a (:frames stack) (:call b) (:return))
                                  (:function b (:frames static) (:call a) (:return)))
                                'zpfoo-label-abi)))
  (fiveam:is (null (%zs-error '((:function a (:frames stack) (:call b) (:return))
                                (:function b (:frames stack) (:call a) (:return)))
                              'zpfoo-label-abi))))

;;; #463: arguments past the backend's argument registers

(fiveam:test a-static-function-takes-arguments-past-its-registers
  (let ((m (%zs-run '((:call f 1 20 300) (:op :halt)
                      (:function f (:args 3)
                        (:op :move (zp w0) (:arg 0))
                        (:op :add (zp w0) (:arg 1))
                        (:op :add (zp w0) (:arg 2))
                        (:return)))
                    'zs-bare-abi)))
    (fiveam:is (= 321 (%zs-word m #x10)))))

(fiveam:test argument-words-come-before-locals-and-saved-registers
  (multiple-value-bind (m assembly)
      (%zs-run '((:op :const (zp w2) 99) (:call outer 20) (:op :halt)
                 (:function outer (:args 1 :locals 1)
                   (:op :move (:local 0) (:arg 0))
                   (:call g 3 (:local 0))
                   (:return))
                 (:function g (:args 2 :locals 1 :save (w2))
                   (:op :move (:local 0) (:arg 1))
                   (:op :const (zp w2) 5)
                   (:op :move (zp w0) (:arg 0))
                   (:op :add (zp w0) (:local 0))
                   (:return)))
               'zs-bare-abi)
    (let ((symbols (assembly-symbols assembly)))
      (fiveam:is (= 23 (%zs-word m #x10)))
      (fiveam:is (= 99 (%zs-word m #x14)))
      (fiveam:is (= 2 (- (gethash "sfgx1" symbols) (gethash "sfgx0" symbols))))
      (fiveam:is (= 4 (- (gethash "sfgx2" symbols) (gethash "sfgx0" symbols)))))))

(fiveam:test a-call-to-a-stack-function-still-pushes-its-arguments
  (let ((text (render-items '((:call f 1 2 3) (:function f (:args 3 :frames stack) (:return)))
                            :backend 'zpfoo-label-abi)))
    (fiveam:is (search "pha" text))
    (fiveam:is (null (search "sffx0" text)))))

;;; #464: static words shared by call graph

(fiveam:test functions-that-do-not-call-each-other-share-words
  (let ((m (%zs-run '((:call a) (:op :move (zp w2) (zp w0)) (:call b) (:op :add (zp w0) (zp w2)) (:op :halt)
                      (:function a (:locals 1)
                        (:op :const (zp w0) 30)
                        (:op :move (:local 0) (zp w0))
                        (:op :move (zp w0) (:local 0))
                        (:return))
                      (:function b (:locals 1)
                        (:op :const (zp w0) 12)
                        (:op :move (:local 0) (zp w0))
                        (:op :move (zp w0) (:local 0))
                        (:return)))
                    'zs-bare-abi)))
    (fiveam:is (= 42 (%zs-word m #x10))))
  (let ((symbols (assembly-symbols
                  (assemble-items (append +zs-words+ '((:function a (:locals 1) (:return))
                                                       (:function b (:locals 1) (:return))))
                                  :backend 'zs-bare-abi))))
    (fiveam:is (= (gethash "sfax0" symbols) (gethash "sfbx0" symbols)))))

(fiveam:test a-function-and-what-it-calls-keep-apart
  (multiple-value-bind (m assembly)
      (%zs-run '((:call outer 20) (:op :halt)
                 (:function outer (:args 1 :locals 1)
                   (:op :move (:local 0) (:arg 0))
                   (:call inner (:local 0))
                   (:op :add (zp w0) (:local 0))
                   (:return))
                 (:function inner (:args 1 :locals 1)
                   (:op :move (:local 0) (:arg 0))
                   (:op :add (:local 0) (:local 0))
                   (:op :move (zp w0) (:local 0))
                   (:return)))
               'zs-bare-abi)
    (let ((symbols (assembly-symbols assembly)))
      (fiveam:is (= 60 (%zs-word m #x10)))
      (fiveam:is (= 2 (- (gethash "sfinnerx0" symbols) (gethash "sfouterx0" symbols)))))))

(fiveam:test a-static-function-reached-through-a-stack-function-keeps-apart-from-its-caller
  (let ((symbols (assembly-symbols
                  (assemble-items (append +zs-words+
                                          '((:function top (:locals 1 :frames static) (:call mid) (:return))
                                            (:function mid (:frames stack) (:call leaf) (:return))
                                            (:function leaf (:locals 1 :frames static) (:return))))
                                  :backend 'zpfoo-label-abi))))
    (fiveam:is (= 2 (- (gethash "sfleafx0" symbols) (gethash "sftopx0" symbols))))))

;;; #468: functions that run from an interrupt

(defbackend zs-int-bare-abi (:extends zs-bare-abi)
  (ops (:return-interrupt () (hlt))))

(defbackend zs-int-abi (:extends zpfoo-label-abi)
  (ops (:return-interrupt () (hlt))))

(defun %zs-symbols (items backend)
  (assembly-symbols (assemble-items (append +zs-words+ items) :backend backend)))

(fiveam:test an-interrupt-function-keeps-words-no-other-function-shares
  (let ((symbols (%zs-symbols '((:function main (:locals 1) (:return))
                                (:function irq (:interrupt t :locals 1) (:return)))
                              'zs-int-bare-abi)))
    (fiveam:is (= 2 (- (gethash "sfirqx0" symbols) (gethash "sfmainx0" symbols))))))

(fiveam:test what-an-interrupt-function-calls-lies-in-its-region
  (let ((symbols (%zs-symbols '((:function main (:locals 1) (:return))
                                (:function irq (:interrupt t :locals 1) (:call helper) (:return))
                                (:function helper (:locals 1) (:return)))
                              'zs-int-bare-abi)))
    (fiveam:is (= 2 (- (gethash "sfirqx0" symbols) (gethash "sfmainx0" symbols))))
    (fiveam:is (= 4 (- (gethash "sfhelperx0" symbols) (gethash "sfmainx0" symbols))))))

(fiveam:test interrupt-functions-keep-apart-from-each-other
  (let ((symbols (%zs-symbols '((:function nmi (:interrupt t :locals 1) (:return))
                                (:function irq (:interrupt t :locals 1) (:return))
                                (:function main (:locals 1) (:return)))
                              'zs-int-bare-abi)))
    (fiveam:is (= 2 (- (gethash "sfnmix0" symbols) (gethash "sfmainx0" symbols))))
    (fiveam:is (= 4 (- (gethash "sfirqx0" symbols) (gethash "sfmainx0" symbols))))))

(fiveam:test mentioning-an-interrupt-function-is-not-a-call
  (let ((symbols (%zs-symbols '((:function main (:locals 1) (jmp irq))
                                (:function irq (:interrupt t :locals 1) (:call shared) (:return))
                                (:function shared (:locals 1) (:return)))
                              'zs-int-bare-abi)))
    (fiveam:is (= 2 (- (gethash "sfirqx0" symbols) (gethash "sfmainx0" symbols))))
    (fiveam:is (= 4 (- (gethash "sfsharedx0" symbols) (gethash "sfmainx0" symbols))))))

(fiveam:test a-static-function-run-from-an-interrupt-and-the-main-line-is-rejected
  (let ((message (%zs-error '((:function main () (:call shared) (:return))
                              (:function irq (:interrupt t) (:call shared) (:return))
                              (:function shared (:locals 1) (:return)))
                            'zs-int-bare-abi)))
    (fiveam:is (search "interrupt irq calls shared, and main calls shared" message))
    (fiveam:is (search ":frames stack" message))))

(fiveam:test a-static-function-run-from-two-interrupts-is-rejected
  (fiveam:is (search "interrupt irq calls shared, and interrupt nmi calls shared"
                     (%zs-error '((:function irq (:interrupt t) (:call shared) (:return))
                                  (:function nmi (:interrupt t) (:call shared) (:return))
                                  (:function shared (:locals 1) (:return)))
                                'zs-int-bare-abi))))

(fiveam:test a-stack-function-may-be-run-from-an-interrupt-and-the-main-line
  (fiveam:is (null (%zs-error '((:function main (:frames static) (:call shared) (:return))
                                (:function irq (:interrupt t :frames static) (:call shared) (:return))
                                (:function shared (:frames stack) (:return)))
                              'zs-int-abi))))

(fiveam:test a-stack-function-does-not-hide-a-static-function-it-calls
  (fiveam:is (search "interrupt irq calls f calls g, and main calls f calls g"
                     (%zs-error '((:function main (:frames static) (:call f) (:return))
                                  (:function irq (:interrupt t :frames static) (:call f) (:return))
                                  (:function f (:frames stack) (:call g) (:return))
                                  (:function g (:frames static :locals 1) (:return)))
                                'zs-int-abi))))

(fiveam:test an-interrupt-function-returns-with-the-interrupt-return
  (let ((static (render-items '((:function irq (:interrupt t :save (w2)) (:return))) :backend 'zs-int-bare-abi))
        (stack (render-items '((:function irq (:interrupt t :frames stack :locals 1) (:return))) :backend 'zs-int-abi)))
    (fiveam:is (search "hlt" static))
    (fiveam:is (null (search "ret" static)))
    (fiveam:is (< (search "lda" static) (search "hlt" static)))
    (fiveam:is (< (search "adds" stack) (search "hlt" stack)))
    (fiveam:is (null (search "ret" stack)))))

(fiveam:test an-interrupt-function-needs-the-backends-interrupt-return
  (fiveam:is (search "return-interrupt"
                     (%zs-error '((:function irq (:interrupt t) (:return))) 'zs-bare-abi))))

(fiveam:test the-interrupt-option-is-t-or-nil-and-takes-no-arguments
  (fiveam:is (search ":interrupt to be t or nil"
                     (%zs-error '((:function irq (:interrupt 5) (:return))) 'zs-int-bare-abi)))
  (fiveam:is (search "takes no arguments"
                     (%zs-error '((:function irq (:interrupt t :args 1) (:return))) 'zs-int-bare-abi)))
  (fiveam:is (null (%zs-error '((:function f (:interrupt nil) (:return))) 'zs-bare-abi))))

(fiveam:test a-lasm-file-reads-the-interrupt-option
  (%call-with-items-file "(:program (:backend zs-int-bare-abi :origin 512)
  (:function main (:locals 1) (:return))
  (:function irq (:interrupt t :locals 1) (:return))
  (:function idle (:interrupt nil :locals 1) (:return))
  (:static-frames))"
    (lambda (path)
      (let ((source (assembly-source (assemble-items-file path)))
            (symbols (assembly-symbols (assemble-items-file path))))
        (fiveam:is (search "hlt" source))
        (fiveam:is (= 2 (- (gethash "sfirqx0" symbols) (gethash "sfmainx0" symbols))))
        (fiveam:is (= (gethash "sfmainx0" symbols) (gethash "sfidlex0" symbols)))))))

(fiveam:test a-lasm-file-reads-the-frame-option
  (flet ((assemble-text (option)
           (%call-with-items-file
               (format nil "(:program (:backend zpfoo-label-abi :origin 512)
  (:function f (:frame ~A :frames stack) (:return)))" option)
             (lambda (path)
               (handler-case (progn (assemble-items-file path) nil)
                 (items-malformed (c) (princ-to-string c)))))))
    (fiveam:is (null (assemble-text "nil")))
    (fiveam:is (search ":frame t needs" (assemble-text "t")))
    (fiveam:is (search "expected :frame to be t or nil" (assemble-text "maybe")))))
