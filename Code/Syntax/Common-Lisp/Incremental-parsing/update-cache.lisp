(cl:in-package #:second-climacs-incremental-parsing)

(defun update-cache (analyzer)
  (scavenge (cache analyzer))
  (read-forms analyzer))
