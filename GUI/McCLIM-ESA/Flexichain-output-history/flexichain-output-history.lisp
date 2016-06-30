(cl:in-package #:climacs-flexichain-output-history)

(defclass flexichain-output-history
    (clim:output-record clim:stream-output-history-mixin)
  ((%parent :initarg :parent :reader clim:output-record-parent)
   (%lines :initform (make-instance 'flexichain:flexichain) :reader lines)
   (%prefix-end :initform 0 :accessor prefix-end)
   (%prefix-height :initform 0 :accessor prefix-height)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)))

(defun forward (history)
  (incf (prefix-height history)
	(+ 5 (clim:bounding-rectangle-height
	      (flexichain:element* (lines history) (prefix-end history)))))
  (incf (prefix-end history)))

(defun backward (history)
  (decf (prefix-end history))
  (decf (prefix-height history)
	(+ 5 (clim:bounding-rectangle-height
	      (flexichain:element* (lines history) (prefix-end history))))))

(defun adjust-prefix (history viewport-top)
  ;; If there are lines in the suffix that are entirely above the
  ;; viewport, then move them to the prefix.
  (loop with lines = (lines history)
	until (= (prefix-end history) (flexichain:nb-elements lines))
	while (<= (+ (prefix-height history)
		     (clim:bounding-rectangle-height
		      (flexichain:element* lines (prefix-end history))))
		  viewport-top)
	do (forward history))
  ;; If there are lines in the prefix that are not entirely above
  ;; the viewport, then move them to the suffix.
  (loop until (zerop (prefix-end history))
	while (> (prefix-height history) viewport-top)
	do (backward history)))

(defmethod clim:replay-output-record
    ((record flexichain-output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle* (clim:pane-viewport-region stream))
    (clim:medium-clear-area (clim:sheet-medium stream)
			    left top right bottom)
    (adjust-prefix record top)
    (loop with lines = (lines record)
	  with length = (flexichain:nb-elements lines)
	  for i from (prefix-end record) below length
	  for line = (flexichain:element* lines i)
	  for height = (clim:bounding-rectangle-height line)
	  for y = (+ (prefix-height record) 5)  then (+ y height 5)
	  while (< y bottom)
	  do (setf (clim:output-record-position line) (values 0 y))
	     (clim:replay-output-record line stream region))))

(defmethod clim:bounding-rectangle* ((history flexichain-output-history))
  (values 0 0 (width history) (height history)))

(defun insert (history record index)
  (when (> (prefix-end history) index)
    (incf (prefix-end history))
    (incf (prefix-height history)
	  (clim:bounding-rectangle-height record)))
  (flexichain:insert* (lines history) index record))

(defun delete (history index)
  (when (> (prefix-end history) index)
    (decf (prefix-height history)
	  (clim:bounding-rectangle-height
	   (flexichain:element* (lines history) index)))
    (decf (prefix-end history)))
  (flexichain:delete* (lines history) index))
