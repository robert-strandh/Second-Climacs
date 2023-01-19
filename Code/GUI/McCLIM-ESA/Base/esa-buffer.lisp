(cl:in-package #:second-climacs-clim-base)

(stealth-mixin:define-stealth-mixin
    buffer (esa-buffer:esa-buffer-mixin)
  base:buffer
  ())

(defmethod base:insert-item :after (cursor item)
  (setf (esa-buffer:needs-saving (base:buffer cursor)) t))

(defmethod base:delete-item :after (cursor)
  (setf (esa-buffer:needs-saving (base:buffer cursor)) t))

(defmethod base:erase-item :after (cursor)
  (setf (esa-buffer:needs-saving (base:buffer cursor)) t))
