(cl:in-package #:climacs-syntax-common-lisp-test)

(defun random-integer ()
  (random 1000000))

(defun random-character ()
  (code-char (+ 32 (random 96))))
