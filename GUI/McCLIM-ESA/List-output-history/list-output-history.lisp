(cl:in-package #:climacs-list-output-history)

(defclass line ()
  ((%contents :initarg :contents :reader contents)
   (%record :initarg :record :reader record)))

(defclass list-output-history
    (clim:output-record clim:stream-output-history-mixin)
  ((%parent :initarg :parent :reader clim:output-record-parent)
   (%lines :initform (list nil) :reader lines)
   (%time-stamp :initform nil :accessor time-stamp)
   (%buffer :initarg :buffer :reader buffer)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)))

(defmethod clim:replay-output-record
    ((record list-output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle* (clim:pane-viewport-region stream))
    (clim:medium-clear-area (clim:sheet-medium stream)
			    left top right bottom))
  (loop for record in (cdr (lines record))
	for height = (clim:bounding-rectangle-height record)
	for y = 0 then (+ y height 5)
	do (setf (clim:output-record-position record) (values 0 y))
	   (clim:replay-output-record record stream region)))

(defmethod clim:bounding-rectangle* ((history list-output-history))
  (values 0 0 (width history) (height history)))
