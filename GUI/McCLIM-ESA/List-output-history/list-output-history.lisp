(cl:in-package #:climacs-list-output-history)

(defclass line ()
  ((%contents :initarg :contents :reader contents)
   (%record :initarg :record :reader record)))

(defclass list-output-history
    (clim:output-record clim:stream-output-history-mixin)
  ((%parent :initarg :parent :reader clim:output-record-parent)
   (%prefix :initform '() :accessor prefix)
   (%prefix-length :initform 0 :accessor prefix-length)
   (%prefix-height :initform 0 :accessor prefix-height)
   (%suffix :initform '() :accessor suffix)
   (%suffix-length :initform 0 :accessor suffix-length)
   (%time-stamp :initform nil :accessor time-stamp)
   (%buffer :initarg :buffer :reader buffer)
   (%width :initform 0 :accessor width)
   (%height :initform 0 :accessor height)))

(defun adjust-prefix-and-suffix (history viewport-top)
  ;; If there are lines in the suffix that are entirely above the
  ;; viewport, then move them to the prefix.
  (loop until (null (suffix history))
	while (<= (+ (prefix-height history)
		     (clim:bounding-rectangle-height
		      (record (contents (car (suffix history))))))
		  viewport-top)
	do (push (pop (suffix history)) (prefix history))
	   (incf (prefix-height history)
		 (+ 5 (clim:bounding-rectangle-height
		       (record (contents (car (prefix history)))))))
	   (incf (prefix-length history))
	   (decf (suffix-length history)))
  ;; If there are lines in the prefix that are not entirely above
  ;; the viewport, then move them to the suffix.
  (loop until (null (prefix history))
	while (> (prefix-height history) viewport-top)
	do (push (pop (prefix history)) (suffix history))
	   (decf (prefix-height history)
		 (+ 5 (clim:bounding-rectangle-height
		       (record (contents (car (suffix history)))))))
	   (decf (prefix-length history))
	   (incf (suffix-length history))))

(defmethod clim:replay-output-record
    ((record list-output-history) stream &optional region x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (multiple-value-bind (left top right bottom)
      (clim:bounding-rectangle* (clim:pane-viewport-region stream))
    (clim:medium-clear-area (clim:sheet-medium stream)
			    left top right bottom)
    (adjust-prefix-and-suffix record top)
    (loop for line in (suffix record)
	  for line-record = (record line)
	  for height = (clim:bounding-rectangle-height line-record)
	  for y = (+ (prefix-height record) 5)  then (+ y height 5)
	  while (< y bottom)
	  do (setf (clim:output-record-position record) (values 0 y))
	     (clim:replay-output-record record stream region))))

(defmethod clim:bounding-rectangle* ((history list-output-history))
  (values 0 0 (width history) (height history)))
