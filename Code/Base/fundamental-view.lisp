(cl:in-package #:second-climacs-base)

;;; A FUNDAMENTAL-VIEW contains a NULL-ANALYZER.

(defclass fundamental-view (view) ())

(defmethod initialize-instance :after ((instance fundamental-view) &key)
  (let* ((buffer (make-empty-standard-buffer))
         (cursor (edit:point buffer))
         (analyzer (make-instance 'null-analyzer :buffer buffer)))
    (reinitialize-instance instance :cursor cursor :analyzer analyzer)))
