(defpackage #:climacs-fundamental
  (:use #:common-lisp)
  (:export))

(in-package #:climacs-fundamental)

(defclass view () ())

;;; For now, do this in a simple but inefficient way by keeping a
;;; simple list of the lines of the buffer.
(defclass fundamental-view (view)
  ((%buffer :initarg :buffer :reader buffer)
   (%update-time :initform -1 :accessor update-time)
   (%lines :initform '() :accessor lines)))

(defgeneric update (view))

(defmethod update ((view fundamental-view))
  (let ((lines (lines view))
	(reversed-lines '()))
    (flet ((sync (line)
	     (loop for old-line = (pop lines)
		   until (eq line old-line))
	     (push line reversed-lines))
	   (create (line)
	     (push line reversed-lines))
	   (update (line)
	     ;; For now don't do anything particular with
	     ;; updated lines.  Just synchronize.
	     (loop for old-line = (pop lines)
		   until (eq line old-line))
	     (push line reversed-lines))
	   (skip (count)
	     (loop repeat count
		   do (push (pop lines) reversed-lines))))
      (climacs-buffer:update (buffer view)
			     (update-time view)
			     #'sync #'skip #'update #'create))
    (setf (lines view) (nreverse reversed-lines))
    (setf (update-time view)
	  (incf (climacs-buffer:current-time (buffer view))))))

	     
