(cl:in-package #:second-climacs-syntax-common-lisp)

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (compute-wad-descriptors cache cursor)
    (declare (ignore current parent previous next))))
