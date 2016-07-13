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
