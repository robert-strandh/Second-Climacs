(cl:in-package #:second-climacs-base)

;;; A NULL-ANALYZER is an analyzer that does no analysis.

(defclass null-analyzer (analyzer) ())

(defmethod update-analyzer-from-buffer ((analyzer null-analyzer) buffer)
  (declare (ignorable analyzer) (ignore buffer))
  nil)
