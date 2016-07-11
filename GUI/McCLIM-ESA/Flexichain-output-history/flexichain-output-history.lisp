(cl:in-package #:climacs-flexichain-output-history)

(defclass flexichain-output-history
    (clim:output-record clim:stream-output-history-mixin)
  ((%parent :initarg :parent :reader clim:output-record-parent)
   (%lines :initform (make-instance 'flexichain:standard-flexichain)
	   :reader lines)
   (%prefix-end :initform 0 :accessor prefix-end)
   (%prefix-height :initform 0 :accessor prefix-height)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)))

(defun forward (history)
  (incf (prefix-height history)
	(clim:bounding-rectangle-height
	 (flexichain:element* (lines history) (prefix-end history))))
  (incf (prefix-end history)))

(defun backward (history)
  (decf (prefix-end history))
  (decf (prefix-height history)
	(clim:bounding-rectangle-height
	 (flexichain:element* (lines history) (prefix-end history)))))

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
	  for y = (prefix-height record)  then (+ y height)
	  for height = (clim:bounding-rectangle-height line)
	  while (< y bottom)
	  do (format *trace-output* "height: ~a y ~a~%" height y)
	  do (setf (clim:output-record-position line) (values 0 y))
	     (clim:replay-output-record line stream region))))

(defmethod clim:bounding-rectangle* ((history flexichain-output-history))
  (values 0 0 (width history) (height history)))

(defun insert (history record index)
  (when (> (prefix-end history) index)
    (incf (prefix-end history))
    (incf (prefix-height history)
	  (clim:bounding-rectangle-height record)))
  (incf (height history)
	(clim:bounding-rectangle-height record))
  (let ((width (clim:bounding-rectangle-width record)))
    (when (> width (width history))
      (setf (width history) width)))
  (flexichain:insert* (lines history) index record))

(defun recompute-width (history)
  (setf (width history)
	(loop with lines = (lines history)
	      for i from 0 below (flexichain:nb-elements (lines history))
	      for record = (flexichain:element* lines i)
	      maximize (clim:bounding-rectangle-width record))))

(defun delete (history index)
  (let ((existing (flexichain:element* (lines history) index)))
    (when (> (prefix-end history) index)
      (decf (prefix-height history)
	    (clim:bounding-rectangle-height existing))
      (decf (prefix-end history)))
    (decf (height history)
	  (clim:bounding-rectangle-height existing))
    (flexichain:delete* (lines history) index)
    (when (= (clim:bounding-rectangle-width existing) (width history))
      (recompute-width history))))

(defun replace (history record index)
  (let ((existing (flexichain:element* (lines history) index)))
    (when (> (prefix-end history) index)
      (incf (prefix-height history)
	    (- (clim:bounding-rectangle-height record)
	       (clim:bounding-rectangle-height existing))))
    (incf (height history)
	  (- (clim:bounding-rectangle-height record)
	     (clim:bounding-rectangle-height existing)))
    (setf (flexichain:element* (lines history) index) record)
    (if (> (clim:bounding-rectangle-width record)
	   (clim:bounding-rectangle-width existing))
	(when (> (clim:bounding-rectangle-width record) (width history))
	  (setf (width history)
		(clim:bounding-rectangle-width record)))
	(when (= (clim:bounding-rectangle-width existing) (width history))
	  (recompute-width history)))))

(defmethod clim:map-over-output-records-containing-position
    (function
     (history flexichain-output-history)
     x y
     &optional
       x-offset
       y-offset
     &rest function-args)
  (declare (ignore x-offset y-offset))
  ;; For now, loop over all the records.  To do this better, do a
  ;; binary search.
  (loop with lines = (lines history )
	for index from 0 below (flexichain:nb-elements lines)
	for record = (flexichain:element* lines index)
	when (clim:region-contains-position-p record x y)
	  do (apply function record function-args)))

(defun change-space-requirements (output-history)
  (clim:change-space-requirements
   (clim:output-record-parent output-history)
   :width (width output-history)
   :height (height output-history)))
