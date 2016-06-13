(cl:in-package #:climacs-list-output-history)

(defclass list-output-history
    (clim:output-record clim:stream-output-history-mixin)
  ((%parent :initarg :parent :reader clim:output-record-parent)
   (%lines :initform (list nil) :reader lines)
   (%time-stamp :initform nil :accessor time-stamp)
   (%buffer :initarg :buffer :reader buffer)))

(defmethod clim:replay-output-record
    ((record list-output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (loop for record in (cdr (lines record))
	for height = (clim:bounding-rectangle-height record)
	for y = 0 then (+ y height 5)
	do (setf (clim:output-record-position record) (values 0 y))
	   (clim:replay-output-record record stream region)))
