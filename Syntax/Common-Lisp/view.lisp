(cl:in-package #:climacs-syntax-common-lisp)

(defclass common-lisp-view (climacs2-base:view) ())

(defmethod initialize-instance :after ((instance common-lisp-view) &key)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (let ((analyzer (make-instance 'analyzer :buffer buffer)))
      (reinitialize-instance instance
			     :cursor cursor
			     :analyzer analyzer))))
