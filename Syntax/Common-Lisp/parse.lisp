(cl:in-package #:climacs-syntax-common-lisp)

(defun parse (analyzer-stream)
  (let ((*stack* (list '())))
    (sicl-reader:read analyzer-stream)
    (first (first *stack*))))

