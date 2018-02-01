(cl:in-package #:climacs-syntax-common-lisp)

(defun parse-and-cache (analyzer)
  (let ((*stack* (list '()))
        (reader:*preserve-whitespace* t)
        (start-line (current-line-number analyzer))
        (start-column (current-item-number analyzer)))
    (handler-case (reader:read-preserving-whitespace analyzer)
      (end-of-file ()
	nil)
      (error ()
	(setf (first *stack*)
	      (list (make-parse-result 'error-parse-result
		      :max-line-width (compute-max-line-width
				       analyzer
				       start-line
				       (current-line-number analyzer)
				       (first *stack*))
		      :children (make-relative (nreverse (first *stack*))
					       start-line)
		      :start-line start-line
		      :start-column start-column
		      :height (- (current-line-number analyzer) start-line)
		      :end-column (current-item-number analyzer)
		      :relative-p nil)))))
    (loop for parse-result in (reverse (first *stack*))
          do (push-to-prefix (folio analyzer) parse-result))))

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
