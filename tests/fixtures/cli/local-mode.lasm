(in-package #:lasm)

(defmachine cli-local-mode
  (register pc :width 16)
  (memory ram :width 8 :addr-width 16))

(defmode cli-leak-probe "(" expr ")")
(defmode (cli-leak-local (:machine cli-local-mode)) "[" expr "]")
