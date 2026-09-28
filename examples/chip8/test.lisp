;;;; test.lisp

(defpackage #:chip8/test
  (:use #:cl #:chip8)
  (:export #:run-tests))

(in-package #:chip8/test)

(fiveam:def-suite chip8-tests)
(fiveam:in-suite chip8-tests)

(defun run-tests ()
  (fiveam:run! 'chip8-tests))

(defun rom (opcodes)
  "OPCODES, 16-bit words, as bytes followed by a jump to itself."
  (let ((halt (+ #x1000 #x200 (* 2 (length opcodes)))))
    (coerce (loop for word in (append opcodes (list halt))
                  collect (ldb (byte 8 8) word)
                  collect (ldb (byte 8 0) word))
            'vector)))

(defun run-rom (opcodes &key (seed 1) keys)
  "The machine after ROM's OPCODES have run to their halt, with KEYS held down."
  (let ((machine (load-chip8 (rom opcodes) :seed seed)))
    (dolist (key keys)
      (key-down machine key))
    (fiveam:is (eq :halted (run-chip8 machine)))
    machine))

(defun regs (machine &rest numbers)
  (mapcar (lambda (n) (v-reg machine n)) numbers))

(defun row (machine y)
  "The first 8 pixels of row Y."
  (subseq (nth y (display-rows machine)) 0 8))

(defun lit (machine y &rest xs)
  (mapcar (lambda (x) (pixel machine x y)) xs))

(fiveam:test a-rom-halts-on-a-jump-to-itself
  (fiveam:is (= #x200 (chip8-pc (run-rom '()))))
  (fiveam:is (= #x202 (chip8-pc (run-rom '(#x6001))))))

(fiveam:test an-unknown-opcode-does-nothing
  (fiveam:is (equal '(1) (regs (run-rom '(#x0123 #x6001)) 0))))

(fiveam:test font-sprites-are-in-memory
  (let ((machine (run-rom '())))
    (fiveam:is (equal '(#xf0 #x90 #x90 #x90 #xf0) (loop for i below 5 collect (memory-byte machine i))))
    (fiveam:is (equal '(#xf0 #x80 #xf0 #x80 #x80) (loop for i from 75 below 80 collect (memory-byte machine i))))))

;;; Registers and arithmetic

(fiveam:test load-and-add-immediate
  (let ((machine (run-rom '(#x60ff      ; V0 = 255
                            #x7002      ; V0 += 2, wraps, no flag
                            #x61ab      ; V1 = 0xAB
                            #x6f07))))  ; VF = 7
    (fiveam:is (equal '(1 #xab 7) (regs machine 0 1 15)))))

(fiveam:test bitwise-ops
  (let ((machine (run-rom '(#x600c      ; V0 = 0b1100
                            #x610a      ; V1 = 0b1010
                            #x8200      ; V2 = V0
                            #x8211      ; V2 |= V1
                            #x8300 #x8312   ; V3 = V0, V3 &= V1
                            #x8400 #x8413)))) ; V4 = V0, V4 ^= V1
    (fiveam:is (equal '(14 8 6) (regs machine 2 3 4)))))

(fiveam:test add-sets-the-carry
  (fiveam:is (equal '(44 1) (regs (run-rom '(#x60c8 #x6164 #x8014)) 0 15)))
  (fiveam:is (equal '(3 0) (regs (run-rom '(#x6201 #x6302 #x8234)) 2 15))))

(fiveam:test subtract-sets-not-borrow
  (fiveam:is (equal '(2 1) (regs (run-rom '(#x6005 #x6103 #x8015)) 0 15)))   ; 5 - 3
  (fiveam:is (equal '(254 0) (regs (run-rom '(#x6003 #x6105 #x8015)) 0 15))) ; 3 - 5
  (fiveam:is (equal '(2 1) (regs (run-rom '(#x6003 #x6105 #x8017)) 0 15))))  ; V0 = 5 - 3

(fiveam:test shifts-take-the-bit-shifted-out
  (fiveam:is (equal '(2 1) (regs (run-rom '(#x6005 #x8006)) 0 15)))     ; 0b101 >> 1
  (fiveam:is (equal '(2 1) (regs (run-rom '(#x6081 #x800e)) 0 15))))    ; 0x81 << 1

(fiveam:test the-flag-wins-when-vf-is-the-destination
  (fiveam:is (equal '(1) (regs (run-rom '(#x6fc8 #x6064 #x8f04)) 15))))

;;; Control flow

(fiveam:test skips
  (let ((machine (run-rom '(#x6001      ; V0 = 1
                            #x3001      ; skip if V0 == 1
                            #x6105      ; (skipped)
                            #x4001      ; skip if V0 != 1: no skip
                            #x6207      ; V2 = 7
                            #x5340      ; skip if V3 == V4: both 0
                            #x6509      ; (skipped)
                            #x9340      ; skip if V3 != V4: no skip
                            #x6609))))  ; V6 = 9
    (fiveam:is (equal '(0 7 0 9) (regs machine 1 2 5 6)))))

(fiveam:test call-and-return
  (let ((machine (run-rom '(#x2206      ; call 0x206
                            #x6101      ; V1 = 1, after the return
                            #x120a      ; jump to the halt
                            #x6007      ; V0 = 7
                            #x00ee))))  ; return
    (fiveam:is (equal '(7 1) (regs machine 0 1)))))

(fiveam:test jump-with-offset
  (let ((machine (run-rom '(#x6004 #xb208 #x6101 #x6101 #x6101 #x6101))))
    (fiveam:is (equal '(4 0) (regs machine 0 1)))))   ; V0 + 0x208 is the halt

;;; Memory

(fiveam:test bcd-and-index
  (let ((machine (run-rom '(#xa300      ; I = 0x300
                            #x60ea      ; V0 = 234
                            #xf033      ; store 2, 3, 4 at I
                            #x6105 #xf11e)))) ; V1 = 5, I += V1
    (fiveam:is (equal '(2 3 4) (loop for i from #x300 to #x302 collect (memory-byte machine i))))
    (fiveam:is (= #x305 (i-reg machine)))))

(fiveam:test store-and-load-registers-leave-i
  (let ((machine (run-rom '(#x6001 #x6102 #x6203    ; V0-V2 = 1, 2, 3
                            #xa320 #xf255           ; store V0-V2 at 0x320
                            #x6000 #x6100 #x6200    ; clear them
                            #xf265))))              ; load V0-V2
    (fiveam:is (equal '(1 2 3) (loop for i from #x320 to #x322 collect (memory-byte machine i))))
    (fiveam:is (equal '(1 2 3) (regs machine 0 1 2)))
    (fiveam:is (= #x320 (i-reg machine)))))

(fiveam:test font-address
  (fiveam:is (= 45 (i-reg (run-rom '(#x6009 #xf029))))))

;;; Display

(fiveam:test draw-a-font-digit
  (let ((machine (run-rom '(#x6000 #xa000 #xd005))))  
    (fiveam:is (equal '("####...." "#..#...." "#..#...." "#..#...." "####....")
                      (loop for y below 5 collect (row machine y))))
    (fiveam:is (equal "........" (row machine 5)))
    (fiveam:is (= 0 (v-reg machine 15)))))

(fiveam:test drawing-twice-erases-and-sets-vf
  (let ((machine (run-rom '(#x6000 #xa000 #xd005 #xd005))))
    (fiveam:is (equal "........" (row machine 0)))
    (fiveam:is (= 1 (v-reg machine 15)))))

(fiveam:test a-sprite-off-the-byte-boundary
  (let ((machine (run-rom '(#x6006 #x6100 #xa000 #xd015))))   ; digit 0 at x = 6
    (fiveam:is (equal '(0 1 1 1 1 0) (lit machine 0 5 6 7 8 9 10)))
    (fiveam:is (equal '(0 1 0 0 1 0) (lit machine 1 5 6 7 8 9 10)))))

(fiveam:test a-sprite-clips-at-the-right-and-bottom-edges
  (let ((machine (run-rom '(#x603e #x611e #xa000 #xd015))))   ; x = 62, y = 30
    (fiveam:is (equal '(1 1 0) (lit machine 30 62 63 0)))
    (fiveam:is (equal '(1 0 0) (lit machine 31 62 63 0)))))

(fiveam:test a-sprite-starts-at-the-coordinates-mod-the-screen-size
  (let ((machine (run-rom '(#x6043 #x6121 #xa000 #xd015))))   ; (67, 33) is (3, 1)
    (fiveam:is (equal '(1 1 1 1 0) (lit machine 1 3 4 5 6 7)))))

(fiveam:test clear-display
  (let ((machine (run-rom '(#x6000 #xa000 #xd005 #x00e0))))
    (fiveam:is (every (lambda (line) (notany (lambda (c) (char= c #\#)) line)) (display-rows machine)))))

;;; Timers

(fiveam:test timers-count-down-every-ten-instructions
  (let ((machine (run-rom (append '(#x603c #xf015    ; delay = 60
                                    #x6005 #xf018)   ; sound = 5
                                  (make-list 40 :initial-element #x7101)
                                  '(#xf207))))) ; V2 = delay
    (fiveam:is (equal '(56 1) (list (delay-timer machine) (sound-timer machine))))
    (fiveam:is (equal '(56 40) (regs machine 2 1)))))

;;; Keypad

(fiveam:test skip-on-key-state
  (let ((machine (run-rom '(#x6003 #xe09e #x6101 #xe0a1 #x6202) :keys '(3))))
    ;; key 3 is down: EX9E skips V1 = 1, EXA1 does not skip V2 = 2
    (fiveam:is (equal '(0 2) (regs machine 1 2))))
  (let ((machine (run-rom '(#x6003 #xe09e #x6101 #xe0a1 #x6202))))
    (fiveam:is (equal '(1 0) (regs machine 1 2)))))

(fiveam:test waiting-for-a-key-resumes-when-one-is-pressed
  (let ((machine (load-chip8 (rom '(#xf00a #x6107)))))
    (fiveam:is (eq :running (run-chip8 machine :max-steps 5000)))
    (fiveam:is (equal '(0 0) (regs machine 0 1)))
    (key-down machine 5)
    (fiveam:is (eq :halted (run-chip8 machine)))
    (fiveam:is (equal '(5 7) (regs machine 0 1)))))

;;; Random numbers

(defun next-seed (seed)
  (ldb (byte 16 0) (+ (* seed 25173) 13849)))

(fiveam:test random-bytes-are-masked-and-repeatable
  (dolist (seed '(1 7))
    (let ((machine (run-rom '(#xc0ff #xc10f) :seed seed))
          (first (next-seed seed)))
      (fiveam:is (equal (list (ldb (byte 8 8) first)
                              (logand #x0f (ldb (byte 8 8) (next-seed first))))
                        (regs machine 0 1))))))

;;; A whole program: registers, memory and the display.

(fiveam:test add-store-decimal-and-draw-the-digit
  (let ((machine (run-rom '(#x6005      ; V0 = 5
                            #x6107      ; V1 = 7
                            #x8014      ; V0 += V1: 12
                            #xa300      ; I = 0x300
                            #xf033      ; 0, 1, 2 at 0x300
                            #xf029      ; I = the sprite for 12: C
                            #x6200 #x6300
                            #xd235))))  ; draw it at (0, 0)
    (fiveam:is (equal '(12 7 0) (regs machine 0 1 15)))
    (fiveam:is (equal '(0 1 2) (loop for i from #x300 to #x302 collect (memory-byte machine i))))
    (fiveam:is (= 60 (i-reg machine)))
    (fiveam:is (equal '("####...." "#......." "#......." "#......." "####....")
                      (loop for y below 5 collect (row machine y))))))
