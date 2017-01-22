(cl:in-package #:climacs-syntax-common-lisp)

(defun parse (analyzer)
  (let ((*stack* (list '())))
    (handler-case (sicl-reader:read analyzer)
      (end-of-file () nil))
    (first (first *stack*))))

(defun parse-and-cache (analyzer)
  (push (parse analyzer)
	(prefix (folio analyzer))))

(defun parse-buffer (analyzer)
  (with-accessors ((current-line-number current-line-number)
		   (current-item-number current-item-number)
		   (cache folio))
      analyzer
    (let ((prefix (prefix cache)))
      (if (null prefix)
	  (setf current-line-number 0
		current-item-number 0)
	  (setf current-line-number (end-line (first prefix))
		current-item-number (end-column (first prefix)))))
    (loop until (progn
		  (skip-whitespace analyzer)
		  (or (eof-p analyzer)
		      (and (not (null (suffix cache)))
			   (let* ((pr (first (suffix cache)))
				  (l (start-line pr))
				  (c (start-column pr)))
			     (and (= l current-line-number)
				  (= c current-item-number))))))
	  do (parse-and-cache analyzer)
	     (pop-to-stream-position analyzer))))
