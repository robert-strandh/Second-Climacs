(cl:in-package #:climacs-esa-gui)

(defclass text-pane (esa:esa-pane-mixin
		     clim:application-pane)
  ()
  (:default-initargs
   :background clim:+white+
   :text-style (clim:make-text-style :fixed nil 14)))
