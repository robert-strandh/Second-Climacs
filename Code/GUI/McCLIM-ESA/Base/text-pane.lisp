(cl:in-package #:second-climacs-clim-base)

(defparameter *default-text-style*
  (clim:make-text-style :fix :roman 14))

(defclass text-pane (esa:esa-pane-mixin
                     clim:application-pane)
  ((%left-gutter :initarg :left-gutter :reader left-gutter))
  (:default-initargs
   :background clim:+white+
   :text-style *default-text-style*))

(defmethod clim:stream-output-history ((stream text-pane))
  (let ((view (clim:stream-default-view stream)))
    (if (typep view 'climacs-clim-view)
        (output-history (clim:stream-default-view stream))
        (call-next-method))))
