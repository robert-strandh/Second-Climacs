(cl:in-package #:second-climacs-base)

(defparameter *view-classes* (make-hash-table :test #'equal))

(defun view-class (file-extension)
  (gethash file-extension *view-classes*))

(defun (setf view-class) (view-class file-extension)
  (setf (gethash file-extension *view-classes*) view-class))
