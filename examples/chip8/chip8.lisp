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

(defun label-address (label)
  (gethash label (assembly-symbols *image*)))

(defun global (machine name)
  (mref machine 'ram (label-address (format nil "gv~(~A~)" name))))

(defun (setf global) (value machine name)
  (setf (mref machine 'ram (label-address (format nil "gv~(~A~)" name))) value))

(defun array-cell (machine name index)
  (mref machine 'ram (+ (label-address (format nil "ar~(~A~)" name)) index)))

(defun (setf array-cell) (value machine name index)
  (setf (mref machine 'ram (+ (label-address (format nil "ar~(~A~)" name)) index)) value))

;;; CHIP-8 memory and the display are byte arrays packed two to a host cell,
;;; the even byte in the low half.
(defun packed-byte (machine name index)
  (let ((cell (array-cell machine name (floor index 2))))
    (if (evenp index) (ldb (byte 8 0) cell) (ldb (byte 8 8) cell))))

(defun (setf packed-byte) (value machine name index)
  (let ((cell (array-cell machine name (floor index 2))))
    (setf (array-cell machine name (floor index 2))
          (if (evenp index)
              (dpb value (byte 8 0) cell)
              (dpb value (byte 8 8) cell)))
    value))

(defun load-chip8 (rom &key (seed 1))
  "A machine with the emulator loaded and ROM, a sequence of bytes, at 0x200.
SEED starts the random number generator."
  (let ((machine (make-machine 'host)))
    (load-program machine *image*)
    (setf (sref machine 'sp) #xF000
          (array-cell machine 'seed 0) seed)
    (loop for byte across (coerce rom 'vector)
          for address from #x200
          do (setf (packed-byte machine 'memory address) byte))
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

(defun key-down (machine key) (setf (array-cell machine 'keys key) 1))
(defun key-up (machine key) (setf (array-cell machine 'keys key) 0))

(defun v-reg (machine n) (array-cell machine 'v n))
(defun i-reg (machine) (global machine 'ir))
(defun chip8-pc (machine) (global machine 'pc))
(defun delay-timer (machine) (global machine 'delay))
(defun sound-timer (machine) (global machine 'sound))
(defun memory-byte (machine address) (packed-byte machine 'memory address))

(defun pixel (machine x y)
  "1 when the pixel at column X, row Y is lit."
  (ldb (byte 1 (- 7 (mod x 8))) (packed-byte machine 'display (+ (* y 8) (floor x 8)))))

(defun display-rows (machine)
  "The display as 32 strings of 64 characters, # for a lit pixel."
  (loop for y below 32
        collect (coerce (loop for x below 64 collect (if (= 1 (pixel machine x y)) #\# #\.)) 'string)))
