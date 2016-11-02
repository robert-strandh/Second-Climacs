(cl:in-package #:climacs-syntax-common-lisp-test)

(defun random-integer ()
  (random 1000000))

(defun random-character ()
  (code-char (+ 32 (random 96))))

(defun random-symbol ()
  (loop for sym being each external-symbol in '#:common-lisp
	repeat (random 500)
	finally (return sym)))

(defun random-string ()
  (with-output-to-string (var)
    (loop repeat (random 10)
	  do (write-char (random-character) var))))

(defun random-form (level)
  (let ((length (random (- 7 level))))
    (if (zerop length)
	(case (random 4)
	  (0 (random-integer))
	  (1 (random-symbol))
	  (2 (random-character))
	  (3 (random-string)))
	(cons (random-symbol)
	      (loop repeat (1- length)
		    collect (random-form (1+ level)))))))

(defun random-forms ()
  (loop repeat (1+ (random 20))
	collect (random-form 0)))

(defun random-buffer-contents (forms)
  (with-output-to-string (var)
    (loop for form in forms
	  do (pprint form var)
	     (write-char #\Newline var))))
