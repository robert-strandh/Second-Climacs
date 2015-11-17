(cl:in-package #:climacs-esa-gui)

(defclass climacs (clim:application-frame esa:esa-frame-mixin)
  ((%views :initarg :views :reader views)
   (%current-view :initarg :current-view :accessor current-view)))
