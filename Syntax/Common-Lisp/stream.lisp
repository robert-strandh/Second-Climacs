(cl:in-package #:climacs-syntax-common-lisp)

(defclass analyzer-stream ()
  ((%analyzer :initarg :analyzer :reader analyzer)
   (%current-line :initform 0 :accessor current-line)
   (%current-column :initform 0 :accessor current-column)))

(defun eof-p (analyzer-stream)
  (let* ((lines (lines (analyzer analyzer-stream)))
	 (count (flexichain:nb-elements lines)))
    (and (= (current-line analyzer-stream) (1- count))
	 (= (current-column analyzer-stream)
	    (let ((last-line (flexichain:element* lines (1- count))))
	      (length (contents last-line)))))))

(defmethod trivial-gray-streams:stream-read-char ((stream analyzer-stream))
  (if (eof-p stream)
      nil
      (with-accessors ((analyzer analyzer)
		       (current-line current-line)
		       (current-column current-column))
	  stream
	(let* ((lines (lines analyzer))
	       (line (flexichain:element* lines current-line))
	       (contents (contents line))
	       (length (length contents)))
	  (if (= length current-column)
	      (prog1 #\Newline
		(incf current-line)
		(setf current-column 0))
	      (prog1 (aref contents current-column)
		(incf current-column)))))))

(defmethod trivial-gray-streams:stream-unread-char
    ((stream analyzer-stream) character)
  (declare (ignore character))
  (with-accessors ((analyzer analyzer)
		   (current-line current-line)
		   (current-column current-column))
      stream
    (if (zerop current-column)
	(let* ((lines (lines analyzer))
	       (line (flexichain:element* lines (1- current-line))))
	  (decf current-line)
	  (setf current-column (length (contents line))))
	(decf current-column))))
