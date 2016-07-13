(cl:in-package #:climacs-command)

(defclass command (standard-generic-function)
  ((%types :initarg :types :reader types))
  (:metaclass #.(class-name (class-of (find-class 'standard-generic-function)))))

(defun split-lambda-list (lambda-list)
  (let* ((position-or-nil (position '&optional lambda-list))
	 (position (if (null position-or-nil)
		       (length lambda-list)
		       position-or-nil)))
    (values (subseq lambda-list 0 position)
	    (subseq lambda-list position))))

(defun extract-lambda-lists (lambda-list)
  (multiple-value-bind (required optional)
      (split-lambda-list lambda-list)
    (let ((ordinary-required
	    (loop for req in required
		  collect (if (symbolp req) req (first req))))
	  (gf-optionals
	    (if (null optional)
		'()
		(cons '&optional
		      (loop for opt in (rest optional)
			    collect (if (symbolp opt) opt (first opt)))))))
      (values (loop for req in required
		    collect (if (symbolp req) t (second req)))
	      (append ordinary-required gf-optionals)
	      (append ordinary-required optional)))))
