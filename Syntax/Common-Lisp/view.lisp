(cl:in-package #:climacs-syntax-common-lisp)

(defclass common-lisp-view (climacs2-base:view) ())

(defmethod initialize-instance :after ((instance common-lisp-view) &key)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (let ((cache (make-instance 'cache :buffer buffer)))
      (reinitialize-instance instance
			     :cursor cursor
			     :analyzer cache))))
