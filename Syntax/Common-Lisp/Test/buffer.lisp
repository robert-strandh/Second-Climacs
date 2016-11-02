(cl:in-package #:climacs-syntax-common-lisp-test)

(defclass line ()
  ((%items :initarg :items :accessor items)))

(defclass buffer ()
  ((%lines :initarg :lines :accessor lines)))

(defmethod cluffer:items ((line line) &key &allow-other-keys)
  (items line))

(defun buffer-from-string (string)
  (make-instance 'buffer
    :lines (loop with lines = (split-sequence:split-sequence #\Newline string)
		 for items in lines
		 collect (make-instance 'line
			   :items items))))
