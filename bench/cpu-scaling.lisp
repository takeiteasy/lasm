;;;; SBCL: sbcl --script bench/cpu-scaling.lisp ../star/star.asd [CPUS] [FRAMES]

(load (merge-pathnames "../examples/boot.lisp" *load-pathname*))
(asdf:load-asd (truename (or (first (uiop:command-line-arguments)) "../star/star.asd")))
(asdf:load-system :star/anima16)

(defun benchmark-cpus (label source count frames &optional verify)
  (let* ((assembly (star/anima16:assemble-anima16 source))
         (machines (loop repeat count
                         collect (let ((machine (lasm:make-machine 'lasm::anima16)))
                                   (lasm:load-program machine assembly)
                                   machine)))
         (samples (make-array frames))
         (steps 0))
    (dolist (machine machines)
      (lasm:run machine :max-steps 100))
    (sb-ext:gc :full t)
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (frame frames)
        (let ((start (get-internal-real-time)))
          (dolist (machine machines)
            (multiple-value-bind (reason executed)
                (lasm:run-for-cycles machine 1667)
              (assert (eq reason :max-cycles))
              (incf steps executed)))
          (setf (aref samples frame)
                (* 1000.0d0 (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second)))))
      (let ((bytes (- (sb-ext:get-bytes-consed) before)))
        (sort samples #'<)
        (format t "~&~A: ~D CPUs, ~D frames, ~D instructions, ~,2F MiB allocated~%"
                label count frames steps (/ bytes 1048576.0))
        (format t "Frame time: mean ~,2F ms, p95 ~,2F ms, max ~,2F ms~%"
                (/ (reduce #'+ samples) frames)
                (aref samples (1- (ceiling (* frames 0.95))))
                (aref samples (1- frames)))))
    (when verify (mapc verify machines))))

(let* ((args (uiop:command-line-arguments))
       (count (parse-integer (or (second args) "100")))
       (frames (parse-integer (or (third args) "60"))))
  (assert (and (plusp count) (plusp frames)))
  (format t "~&Each CPU receives 1,667 cycles per frame (~D frames).~%" frames)
  (benchmark-cpus "Registers"
                  "loop: add a, #1
set PC, #loop" count frames)
  (benchmark-cpus "Memory"
                  "set b, #4096
loop: set [b], #777
add c, [b]
add b, #1
and b, #4095
bor b, #4096
set PC, #loop" count frames
                  (lambda (machine) (assert (= 777 (lasm:mref machine 'lasm::ram 4096)))))
  (benchmark-cpus "Clock interrupts"
                  "set a, #0
set b, #1
hwi #0
set a, #2
set b, #7
hwi #0
ias #handler
loop: add x, #1
set PC, #loop
handler: add y, #1
rfi #0" count frames
                  (lambda (machine) (assert (plusp (lasm:regref machine 'lasm::reg 4))))))
