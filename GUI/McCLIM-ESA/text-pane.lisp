(cl:in-package #:climacs-esa-gui)

(defclass text-pane (clim:application-pane
		     esa:esa-pane-mixin)
  ((%lines :initarg :lines :accessor lines))
  (:default-initargs
   :background clim:+white+
   :text-style (clim:make-text-style :fixed nil 14)))

;;; The value of this variable is a fraction of the text height to be
;;; added to that height in order to obtain the distance between two
;;; base lines.
(defparameter *base-line-extra* 0.1)

(defmethod clim:compose-space ((pane text-pane) &key width height)
  (declare (ignore width height))
  (let* ((style (clim:pane-text-style pane))
	 (ascent (ceiling (clim:text-style-ascent style pane)))
	 (descent (ceiling (clim:text-style-descent style pane)))
	 (height (clim:text-style-height style pane))
	 (dist (ceiling (* height (+ 1.0 *base-line-extra*))))
	 (line-count (length (lines pane))))
    (clim:make-space-requirement
     :width 200
     :height (+ (* dist line-count) ascent descent))))
