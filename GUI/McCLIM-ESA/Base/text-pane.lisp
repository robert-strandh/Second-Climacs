(cl:in-package #:climacs-esa-gui)

(defclass text-pane (esa:esa-pane-mixin
		     clim:application-pane)
  ()
  (:default-initargs
   :background clim:+white+
   :text-style (clim:make-text-style :fixed nil 14)))

(defmethod clim:stream-output-history ((stream text-pane))
  (let ((view (clim:stream-default-view stream)))
    (if (typep view 'climacs-clim-view)
        (output-history (clim:stream-default-view stream))
        (call-next-method))))
