(cl:in-package #:climacs-esa-gui)

(defclass climacs (clim:standard-application-frame
		   esa:esa-frame-mixin)
  ((%views :initarg :views :reader views)
   (%current-view :initarg :current-view :accessor current-view)))

(defun climacs ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'climacs)))
