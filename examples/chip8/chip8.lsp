;;;; chip8.lsp
;;;; A CHIP-8 interpreter in the lasm source language (docs/language.md).
;;;; host.lisp defines the machine it compiles for; chip8.lisp loads a ROM and
;;;; runs it. See docs/examples.md.

#|
CHIP-8 SPECIFICATION

Machine
  4096 bytes of memory. Programs load at 0x200; the font sprites live at 0x000.
  Registers: V0-VF (8 bits; VF is the flag register), I (12 bits), PC (12 bits).
  A stack of 16 return addresses. Two 8-bit timers, delay and sound, that
  count down at 60 Hz. A 16-key keypad, keys 0-F.
  Every instruction is two bytes, high byte first, and PC advances past it
  before it runs.

Display
  64x32 pixels, one bit each. A row is 8 bytes, the leftmost pixel the top
  bit of the first byte.

Font
  The digits 0-F as 8x5 sprites, 5 bytes each, at 0x000 + 5*digit.

Instructions (NNN address, NN byte, N nibble, X and Y register numbers)
  00E0  clear the display                  8XY0  VX = VY
  00EE  return                             8XY1  VX = VX | VY
  1NNN  PC = NNN                           8XY2  VX = VX & VY
  2NNN  call NNN                           8XY3  VX = VX ^ VY
  3XNN  skip if VX == NN                   8XY4  VX += VY; VF = carry
  4XNN  skip if VX != NN                   8XY5  VX -= VY; VF = 1 if no borrow
  5XY0  skip if VX == VY                   8XY6  VX >>= 1; VF = the bit shifted out
  6XNN  VX = NN                            8XY7  VX = VY - VX; VF = 1 if no borrow
  7XNN  VX += NN, no flag                  8XYE  VX <<= 1; VF = the bit shifted out
  9XY0  skip if VX != VY                   ANNN  I = NNN
  BNNN  PC = NNN + V0                      CXNN  VX = random byte & NN
  DXYN  draw N rows of the sprite at I to (VX, VY); VF = 1 if a lit pixel
        was turned off
  EX9E  skip if key VX is down             EXA1  skip if key VX is up
  FX07  VX = delay                         FX0A  wait for a key, VX = key
  FX15  delay = VX                         FX18  sound = VX
  FX1E  I += VX                            FX29  I = the font sprite for VX
  FX33  store VX as three decimal digits at I, I+1, I+2
  FX55  store V0-VX at I onward            FX65  load V0-VX from I onward
        (I is left unchanged by both)

Choices
  A sprite starts at (VX mod 64, VY mod 32) and is clipped at the right and
  bottom edges. Shifts read VX, not VY. An opcode this list does not name does
  nothing. The program halts on a jump to itself (1NNN at address NNN).
|#

(defconstant steps-per-tick 10)

(defarray memory 2048)                  ; 2048 words, 4096 bytes, one a host cell
(defarray display 128)                  ; 64x32 bits, 256 bytes
(defarray v 16)
(defarray stack 16)
(defarray keys 16)                      ; nonzero while a key is down

;;; An array is part of the image, so the host can set its contents before the
;;; program starts; a global is stored again by the start-up stub.
(defarray seed (1))

(defvar pc #x200)
(defvar ir 0)
(defvar sptr 0)
(defvar delay 0)
(defvar sound 0)
(defvar ticks 0)

(defarray font
  (#xF0 #x90 #x90 #x90 #xF0             ; 0
   #x20 #x60 #x20 #x20 #x70             ; 1
   #xF0 #x10 #xF0 #x80 #xF0             ; 2
   #xF0 #x10 #xF0 #x10 #xF0             ; 3
   #x90 #x90 #xF0 #x10 #x10             ; 4
   #xF0 #x80 #xF0 #x10 #xF0             ; 5
   #xF0 #x80 #xF0 #x90 #xF0             ; 6
   #xF0 #x10 #x20 #x40 #x40             ; 7
   #xF0 #x90 #xF0 #x90 #xF0             ; 8
   #xF0 #x90 #xF0 #x10 #xF0             ; 9
   #xF0 #x90 #xF0 #x90 #x90             ; A
   #xE0 #x90 #xE0 #x90 #xE0             ; B
   #xF0 #x80 #x80 #x80 #xF0             ; C
   #xE0 #x90 #x90 #x90 #xE0             ; D
   #xF0 #x80 #xF0 #x80 #xF0             ; E
   #xF0 #x80 #xF0 #x80 #x80))           ; F

;;; A macro is expanded at compile time and its body is ordinary code, so a
;;; program can grow the forms it wants. `cond` is a chain of `if`s; a clause
;;; whose test is `else` always runs.
(defun-for-syntax cond-clauses (clauses)
  (if (null clauses)
      0
      (let ((test (car (car clauses))) (body (cdr (car clauses))))
        `(if ,(if (eq test 'else) 1 test)
             (progn ,@body)
             ,(cond-clauses (cdr clauses))))))

(defmacro cond (&rest clauses) (cond-clauses clauses))

;;; The fields of an opcode. It may have its top bit set, and the host orders
;;; words as signed, so an opcode is only ever masked and shifted.
(defmacro x-of (op) `(logand (shr ,op 8) 15))
(defmacro y-of (op) `(logand (shr ,op 4) 15))
(defmacro n-of (op) `(logand ,op 15))
(defmacro nn-of (op) `(logand ,op 255))
(defmacro nnn-of (op) `(logand ,op #xFFF))

(defmacro skip-if (test) `(if ,test (set pc (logand (+ pc 2) #xFFF))))

;;; Memory and the display are byte arrays, one byte a host cell. aref-byte and
;;; aset-byte index them a byte at a time.
(defun fetch ()
  (let ((hi (aref-byte memory pc))
        (lo (aref-byte memory (logand (+ pc 1) #xFFF))))
    (set pc (logand (+ pc 2) #xFFF))
    (logior (shl hi 8) lo)))

(defun reset ()
  (let ((i 0))
    (while (< i 80)
      (aset-byte memory i (aref font i))
      (set i (+ i 1)))))

(defun clear-display ()
  (let ((i 0))
    (while (< i 128)
      (aset display i 0)
      (set i (+ i 1)))))

;;; XOR BITS into display byte INDEX; 1 when that turned a lit pixel off.
(defun xor-byte (index bits)
  (let ((old (aref-byte display index)))
    (aset-byte display index (logxor old bits))
    (if (logand old bits) 1 0)))

(defun draw (op)
  (let ((x (logand (aref v (x-of op)) 63))
        (y (logand (aref v (y-of op)) 31))
        (row 0)
        (hit 0))
    (while (< row (n-of op))
      (if (< (+ y row) 32)
          (let ((bits (aref-byte memory (logand (+ ir row) #xFFF)))
                (index (+ (* (+ y row) 8) (shr x 3)))
                (shift (logand x 7)))
            (set hit (logior hit (xor-byte index (shr bits shift))))
            (if (< (shr x 3) 7)
                (set hit (logior hit (xor-byte (+ index 1)
                                               (logand (shl bits (- 8 shift)) 255)))))))
      (set row (+ row 1)))
    (aset v 15 hit)))

(defun op-0 (op)
  (cond ((= op #xE0) (clear-display))
        ((= op #xEE)
         (set sptr (logand (- sptr 1) 15))
         (set pc (aref stack sptr)))))

(defun op-1 (op) (set pc (nnn-of op)))

(defun op-2 (op)
  (aset stack sptr pc)
  (set sptr (logand (+ sptr 1) 15))
  (set pc (nnn-of op)))

(defun op-3 (op) (skip-if (= (aref v (x-of op)) (nn-of op))))
(defun op-4 (op) (skip-if (/= (aref v (x-of op)) (nn-of op))))
(defun op-5 (op) (skip-if (= (aref v (x-of op)) (aref v (y-of op)))))
(defun op-6 (op) (aset v (x-of op) (nn-of op)))
(defun op-7 (op) (aset v (x-of op) (logand (+ (aref v (x-of op)) (nn-of op)) 255)))

;;; The arithmetic group. The result is stored before the flag, so VF as the
;;; destination ends up holding the flag.
(defun op-8 (op)
  (let ((x (x-of op)) (vx (aref v (x-of op))) (vy (aref v (y-of op))) (n (n-of op)))
    (cond ((= n 0) (aset v x vy))
          ((= n 1) (aset v x (logior vx vy)))
          ((= n 2) (aset v x (logand vx vy)))
          ((= n 3) (aset v x (logxor vx vy)))
          ((= n 4) (aset v x (logand (+ vx vy) 255))
                   (aset v 15 (> (+ vx vy) 255)))
          ((= n 5) (aset v x (logand (- vx vy) 255))
                   (aset v 15 (>= vx vy)))
          ((= n 6) (aset v x (shr vx 1))
                   (aset v 15 (logand vx 1)))
          ((= n 7) (aset v x (logand (- vy vx) 255))
                   (aset v 15 (>= vy vx)))
          ((= n 14) (aset v x (logand (shl vx 1) 255))
                    (aset v 15 (shr vx 7))))))

(defun op-9 (op) (skip-if (/= (aref v (x-of op)) (aref v (y-of op)))))
(defun op-a (op) (set ir (nnn-of op)))
(defun op-b (op) (set pc (logand (+ (nnn-of op) (aref v 0)) #xFFF)))

;;; A 16-bit linear congruential generator; the high byte is the most random.
(defun op-c (op)
  (aset seed 0 (logand (+ (* (aref seed 0) 25173) 13849) #xFFFF))
  (aset v (x-of op) (logand (shr (aref seed 0) 8) (nn-of op))))

(defun op-d (op) (draw op))

(defun op-e (op)
  (let ((down (aref keys (logand (aref v (x-of op)) 15))))
    (cond ((= (nn-of op) #x9E) (skip-if down))
          ((= (nn-of op) #xA1) (skip-if (not down))))))

;;; Waits by running the same instruction again until a key is down.
(defun wait-key (x)
  (let ((key 0) (found 0))
    (while (< key 16)
      (if (aref keys key)
          (progn (aset v x key) (set found 1) (set key 16))
          (set key (+ key 1))))
    (if (not found) (set pc (logand (- pc 2) #xFFF)))))

(defun store-bcd (value)
  (aset-byte memory (logand ir #xFFF) (/ value 100))
  (aset-byte memory (logand (+ ir 1) #xFFF) (mod (/ value 10) 10))
  (aset-byte memory (logand (+ ir 2) #xFFF) (mod value 10)))

(defun store-registers (last)
  (let ((i 0))
    (while (<= i last)
      (aset-byte memory (logand (+ ir i) #xFFF) (aref v i))
      (set i (+ i 1)))))

(defun load-registers (last)
  (let ((i 0))
    (while (<= i last)
      (aset v i (aref-byte memory (logand (+ ir i) #xFFF)))
      (set i (+ i 1)))))

(defun op-f (op)
  (let ((x (x-of op)) (nn (nn-of op)))
    (cond ((= nn #x07) (aset v x delay))
          ((= nn #x0A) (wait-key x))
          ((= nn #x15) (set delay (aref v x)))
          ((= nn #x18) (set sound (aref v x)))
          ((= nn #x1E) (set ir (logand (+ ir (aref v x)) #xFFF)))
          ((= nn #x29) (set ir (* (logand (aref v x) 15) 5)))
          ((= nn #x33) (store-bcd (aref v x)))
          ((= nn #x55) (store-registers x))
          ((= nn #x65) (load-registers x)))))

;;; The dispatch table: an array of function values, indexed by an opcode's
;;; top nibble. funcall calls through the address the array holds.
(defarray ops
  ((function op-0) (function op-1) (function op-2) (function op-3)
   (function op-4) (function op-5) (function op-6) (function op-7)
   (function op-8) (function op-9) (function op-a) (function op-b)
   (function op-c) (function op-d) (function op-e) (function op-f)))

;;; Both timers count down once every steps-per-tick instructions.
(defun tick ()
  (set ticks (+ ticks 1))
  (if (>= ticks steps-per-tick)
      (progn
        (set ticks 0)
        (if delay (set delay (- delay 1)))
        (if sound (set sound (- sound 1))))))

;;; Runs until a jump to itself, which leaves PC at that instruction.
(defun main ()
  (reset)
  (let ((running 1))
    (while running
      (let ((start pc) (op (fetch)))
        (if (= op (+ #x1000 start))
            (progn (set pc start) (set running 0))
            (progn
              (funcall (aref ops (shr op 12)) op)
              (tick)))))))
