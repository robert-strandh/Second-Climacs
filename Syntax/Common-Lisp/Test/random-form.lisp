(cl:in-package #:climacs-syntax-common-lisp-test)

(defun random-integer ()
  (random 1000000))

(defun random-character ()
  (code-char (+ 32 (random 96))))

(defun random-symbol ()
  (loop for sym being each external-symbol in '#:common-lisp
	repeat (random 500)
	finally (return sym)))
