(cl:in-package #:climacs-esa-gui)

(defclass buffer (esa-buffer:esa-buffer-mixin)
  ((%cluffer-buffer
    :initarg :cluffer-buffer
    :reader cluffer-buffer)))

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (make-instance 'buffer
    :cluffer-buffer (climacs-basic-emacs:buffer-from-stream stream)))
