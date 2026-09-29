(in-package #:lasm)

(defdirective ".compile-file-bad" (a b) (set-origin! a) (reserve b))
