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

(defmethod clim:handle-repaint ((pane text-pane) region)
  (let* ((style (clim:pane-text-style pane))
	 (ascent (ceiling (clim:text-style-ascent style pane)))
	 (descent (ceiling (clim:text-style-descent style pane)))
	 (height (clim:text-style-height style pane))
	 (dist (ceiling (* height (+ 1.0 *base-line-extra*))))
	 (line-count (length (lines pane))))
    ;; Line i is drawn with its base-line at
    ;; (+ ascent (* dist i))
    ;;
    ;; The first visible line is the first one drawn at a vertical
    ;; position that is greater than or equal to (- top descent).
    ;;
    ;; The last visible line is the last one drawn at a vertical
    ;; position that is less than or equal to (+ bottom ascent).
    (clim:with-bounding-rectangle* (left top right bottom) region
      (declare (ignore left right))
      (let ((first-line (max 0 (floor (/ (- top descent ascent) dist))))
	    (last-line (min (1- line-count) (ceiling (/ bottom dist)))))
	;; FIXME: make sure those lines exist in the vector
	(loop for i from first-line to last-line
	      do (clim:draw-text* pane
				  (aref (lines pane) i)
				  0
				  (+ ascent (* dist i))))))))
