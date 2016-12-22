(cl:in-package #:climacs-esa-gui)

(defclass buffer ()
  ((%cluffer-buffer
    :initarg :cluffer-buffer
    :reader cluffer-buffer)
   (%filepath
    :initform nil
    :initarg filepath
    :accessor esa-buffer:filepath)
   (%name
    :initform ""
    :initarg :name
    :accessor esa-utils:name)
   (%needs-saving
    :initform nil
    :initarg :needs-saving
    :accessor esa-buffer:needs-saving)))

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (make-instance 'buffer
    :cluffer-buffer (climacs-basic-emacs:buffer-from-stream stream)))
