(cl:in-package #:clim-simple-editor-record)

(defclass record (clim:stream-output-history-mixin)
  (;; This slot contains the splay tree that represents the sequence
   ;; of child output records (lines).  If there are no children, then
   ;; this slot contains NIL.
   (%contents :initform nil :initarg :contents :accessor contents)))

(defmethod clim:bounding-rectangle* ((record record))
  (if (null (contents record))
      (values 0 0 0 0)
      (clim:bounding-rectangle* (contents record))))

(defmethod clim:bounding-rectangle-width ((record record))
  (multiple-value-bind (x-min y-min x-max y-max)
      (clim:bounding-rectangle* record)
    (declare (ignore y-min y-max))
    (- x-max x-min)))

(defmethod clim:bounding-rectangle-height ((record record))
  (multiple-value-bind (x-min y-min x-max y-max)
      (clim:bounding-rectangle* record)
    (declare (ignore x-min x-max))
    (- y-max y-min)))

(defmethod clim:bounding-rectangle-max-x ((record record))
  (nth-value 2 (clim:bounding-rectangle* record)))

(defmethod clim:clear-output-record ((record record))
  (setf (contents record) nil))

(defmethod clim:replay-output-record ((record record) stream
				      &optional
					(region clim:+everywhere+)
					x-offset y-offset)
  (declare (ignore x-offset y-offset))
  (unless (or (null (contents record))
	      (eq region clim:+nowhere+))
    (clim:replay-output-record (contents record) stream region)))

;;; The CLIM II specification requires the position of an output
;;; record to be relative to the stream in which it is located, rather
;;; than to its parent.  We interpret that to mean that the following
;;; generic functions accept or return stream-relative coordinates:
;;;
;;;  - output-record-position
;;;  - (setf* output-record-position)
;;;  - output-record-start-cursor-position
;;;  - (setf* output-record-start-cursor-position)
;;;  - output-record-end-cursor-position
;;;  - (setf* output-record-end-cursor-position)
;;;  - replay-output-record
;;;  - output-record-hit-detection-rectangle*
;;;  - output-record-refined-position-test
;;;
;;; Clearly, we do not want to store stream-relative positions in the
;;; output records for individual lines, because then we would have to
;;; update all following such records when one is inserted or deleted.
;;; We get around this problem by requiring that the kind of child
;;; output records that can be contained in this top-level record be
;;; subclasses of RELATIVE-COORDINATES-OUTPUT-RECORD-MIXIN.
;;;
;;; In order for this scheme to work, we have to rely on CLIM not
;;; caching the position of our relative output records, because those
;;; positions may change as a result of editing operations, and
;;; without (SETF* OUTPUT-RECORD-POSITION) being called.

;;; We make this class a MIXIN class so that it can be used to define
;;; subclasses of existing CLIM output record classes such as
;;; TEXT-OUTPUT-RECORD, thereby overriding the default behavior of the
;;; parent class.
(defclass relative-coordinates-output-record-mixin ()
  ((%dx :initarg :dx :accessor dx)
   (%dy :initarg :dy :accessor dy)))

(defmethod clim:output-record-position
    ((record relative-coordinates-output-record-mixin))
  (multiple-value-bind (x y)
      (clim:output-record-position (clim:output-record-parent record))
    (values (+ x (dx record)) (+ y (dy record)))))
