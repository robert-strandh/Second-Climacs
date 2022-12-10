(cl:in-package #:second-climacs-syntax-common-lisp-test)

(defun random-integer ()
  (random 1000000))

(defun random-character ()
  (code-char (+ 32 (random 95))))

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

(defun forms-from-buffer-contents (string)
  (with-input-from-string (var string)
    (loop with eof-value = (gensym)
	  for form = (read var nil eof-value)
	  until (eq form eof-value)
	  collect form)))
