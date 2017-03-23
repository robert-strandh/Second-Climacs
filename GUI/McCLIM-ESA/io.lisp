(cl:in-package #:second-climacs-clim-base)

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (climacs2-base:fill-buffer-from-stream cursor stream)
    buffer))
