(cl:in-package #:climacs-esa-gui)

(defclass text-pane (clim:application-pane
		     esa:esa-pane-mixin)
  ()
  (:default-initargs
   :background clim:+white+
   :text-style (clim:make-text-style :fixed nil 14)))
