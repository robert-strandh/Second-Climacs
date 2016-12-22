(cl:in-package #:climacs-esa-gui)

(defclass buffer (cluffer-standard-buffer:buffer)
  ((%filepath
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
  (let ((initial-line (make-instance 'cluffer-standard-line:closed-line)))
    (make-instance 'buffer
      :initial-line initial-line)))

