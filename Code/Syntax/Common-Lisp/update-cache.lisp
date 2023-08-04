(cl:in-package #:second-climacs-syntax-common-lisp)

(defun update-cache (analyzer)
  (scavenge (cache analyzer))
  (read-forms analyzer))
