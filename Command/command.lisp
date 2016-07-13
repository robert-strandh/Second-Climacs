(cl:in-package #:climacs-command)

(defclass command (standard-generic-function)
  ((%lambda-list :initarg :lambda-list :reader lambda-list))
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
    (values (loop for req in required
		  collect (if (symbolp req) t (second req)))
	    (append (loop for req in required
			  collect (if (symbolp req) req (first req)))
		    optional))))
