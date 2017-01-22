(cl:in-package #:climacs-syntax-common-lisp)

(defclass view (climacs2-base:view) ())

(defmethod initialize-instance :after ((instance view) &key)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (let* ((cache (make-instance 'cache))
           (analyzer (make-instance 'analyzer
                       :folio cache
                       :buffer buffer)))
      (reinitialize-instance instance
                             :cursor cursor
			     :analyzer analyzer))))
