(cl:in-package #:climacs-syntax-common-lisp)

(defun parse (analyzer-stream)
  (let ((*stack* (list '())))
    (handler-case (sicl-reader:read analyzer-stream)
      (end-of-file () nil))
    (first (first *stack*))))

(defun parse-and-cache (analyzer-stream)
  (push (parse analyzer-stream)
	(prefix (folio analyzer-stream))))

(defun parse-buffer (analyzer-stream)
  (with-accessors ((current-line-number current-line-number)
		   (current-item-number current-item-number)
		   (cache folio))
      analyzer-stream
    (let ((prefix (prefix cache)))
      (if (null prefix)
	  (setf current-line-number 0
		current-item-number 0)
	  (setf current-line-number (end-line (first prefix))
		current-item-number (end-column (first prefix)))))
    (loop until (progn
		  (skip-whitespace analyzer-stream)
		  (or (eof-p analyzer-stream)
		      (and (not (null (suffix cache)))
			   (let* ((pr (first (suffix cache)))
				  (l (start-line pr))
				  (c (start-column pr)))
			     (and (= l current-line-number)
				  (= c current-item-number))))))
	  do (parse-and-cache analyzer-stream)
	     (pop-to-stream-position analyzer-stream))))
