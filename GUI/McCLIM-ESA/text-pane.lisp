(cl:in-package #:climacs-esa-gui)

(defclass text-pane (climi::composite-pane
		     clim:permanent-medium-sheet-output-mixin)
  ((%lines :initarg :lines :accessor lines))
  (:default-initargs
   :background clim:+white+
   :text-style (clim:make-text-style :fixed nil 14)))
