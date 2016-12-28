(cl:in-package #:second-climacs-base)

;;; A FUNDAMENTAL-VIEW contains a NULL-ANALYZER.

(defclass fundamental-view (view) ())

(defmethod initialize-instance :after ((instance fundamental-view) &key)
  (multiple-value-bind (buffer cursor)
      (make-empty-standard-buffer-and-cursor)
    (let ((analyzer (make-instance 'null-analyzer :buffer buffer)))
      (reinitialize-instance instance
			     :cursor cursor
			     :analyzer analyzer))))

  
