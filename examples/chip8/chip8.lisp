;;;; chip8.lisp
;;;; Compiles chip8.lsp for the host machine and drives it from Lisp.
;;;; Load it with (asdf:load-system :chip8); see docs/examples.md.

(in-package #:chip8)

;;; assemble-source-file compiles the .lsp program with the backend and
;;; assembles the result. A label is the program's name for a global (`gv`) or an
;;; array (`ar`); see docs/language.md#names.
(defparameter *image*
  (assemble-source-file (asdf:system-relative-pathname :chip8 "chip8.lsp")
                        :backend 'host-lang)
  "The assembled emulator.")

;;; A language word is two host cells, the low one first. CHIP-8 memory and the
;;; display are byte arrays, one byte a cell.
(defun label-address (label)
  (gethash label (assembly-symbols *image*)))

(defun word-at (machine address)
  (logior (mref machine 'ram address) (ash (mref machine 'ram (1+ address)) 8)))

(defun (setf word-at) (value machine address)
  (setf (mref machine 'ram address) (ldb (byte 8 0) value)
        (mref machine 'ram (1+ address)) (ldb (byte 8 8) value))
  value)

(defun global (machine name)
  (word-at machine (label-address (format nil "gv~(~A~)" name))))

(defun (setf global) (value machine name)
  (setf (word-at machine (label-address (format nil "gv~(~A~)" name))) value))

(defun array-word (machine name index)
  (word-at machine (+ (label-address (format nil "ar~(~A~)" name)) (* 2 index))))

(defun (setf array-word) (value machine name index)
  (setf (word-at machine (+ (label-address (format nil "ar~(~A~)" name)) (* 2 index))) value))

(defun array-byte (machine name index)
  (mref machine 'ram (+ (label-address (format nil "ar~(~A~)" name)) index)))

(defun (setf array-byte) (value machine name index)
  (setf (mref machine 'ram (+ (label-address (format nil "ar~(~A~)" name)) index)) value))

(defun load-chip8 (rom &key (seed 1))
  "A machine with the emulator loaded and ROM, a sequence of bytes, at 0x200.
SEED starts the random number generator."
  (let ((machine (make-machine 'host)))
    (load-program machine *image*)
    (setf (sref machine 'sp) #xF000
          (array-word machine 'seed 0) seed)
    (loop for byte across (coerce rom 'vector)
          for address from #x200
          do (setf (array-byte machine 'memory address) byte))
    machine))

(defun run-chip8 (machine &key (max-steps 1000000))
  "Run MACHINE until the ROM halts on a jump to itself, or MAX-STEPS host
instructions pass. A ROM waiting for a key stops at MAX-STEPS; press a key and
call this again to resume. Returns :HALTED or :RUNNING; any other reason the
host stopped is an error."
  (let ((reason (run machine :max-steps max-steps)))
    (case reason
      (:trap :halted)
      (:max-steps :running)
      (t (error "The CHIP-8 host stopped: ~S" reason)))))

(defun key-down (machine key) (setf (array-word machine 'keys key) 1))
(defun key-up (machine key) (setf (array-word machine 'keys key) 0))

(defun v-reg (machine n) (array-word machine 'v n))
(defun i-reg (machine) (global machine 'ir))
(defun chip8-pc (machine) (global machine 'pc))
(defun delay-timer (machine) (global machine 'delay))
(defun sound-timer (machine) (global machine 'sound))
(defun memory-byte (machine address) (array-byte machine 'memory address))

(defun pixel (machine x y)
  "1 when the pixel at column X, row Y is lit."
  (ldb (byte 1 (- 7 (mod x 8))) (array-byte machine 'display (+ (* y 8) (floor x 8)))))

(defun display-rows (machine)
  "The display as 32 strings of 64 characters, # for a lit pixel."
  (loop for y below 32
        collect (coerce (loop for x below 64 collect (if (= 1 (pixel machine x y)) #\# #\.)) 'string)))
