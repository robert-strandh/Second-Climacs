(cl:in-package #:climacs-esa-gui)

;;; A FUNDAMENTAL-VIEW contains a NULL-ANALYZER.

(defclass fundamental-view (view) ())

;;; Since the FUNDAMENTAL-VIEW contains a NULL-ANALYZER, the method on
;;; UPDATE-VIEW-FROM-ANALYZER specialized to the NULL-ANALYZER calls
;;; UPDATE-VIEW-FROM-ANALYZER recursively with the buffer replacing
;;; the analyzer.  That is why the following method is specialized to
;;; the STANDARD-BUFFER, and not to a subclass of ANALYZER.

(defmethod climacs2-base:update-view-from-analyzer
    ((view fundamental-view) (buffer climacs2-base:standard-buffer))
  (let ((index 0)
	(pane (clim:output-record-parent (history view))))
    (flet ((delete-line (index)
	     (flexichain:delete* (lines view) index)
	     (climacs-flexichain-output-history:delete (history view) index)))
      (flet ((delete-lines-until-line (line)
	       (loop for entry = (flexichain:element* (lines view) index)
		     until (eq line (car entry))
		     do (delete-line index))))
	(flet ((skip (n)
		 (incf index n))
	       (modify (line)
		 (delete-lines-until-line line)
		 (let ((entry (flexichain:element* (lines view) index)))
		   (setf (cdr entry)
			 (cluffer:items (car entry)))
		   (climacs-flexichain-output-history:replace
		    (history view)
		    (clim:with-output-to-output-record (pane)
		      (format pane "~a" (coerce (cdr entry) 'string)))
		    index))
		 (incf index))
	       (sync (line)
		 (delete-lines-until-line line)
		 (incf index))
	       (create (line)
		 (let ((entry (cons line (cluffer:items line))))
		   (flexichain:insert* (lines view) index entry)
		   (climacs-flexichain-output-history:insert
		    (history view)
		    (clim:with-output-to-output-record (pane)
		      (format pane "~a" (coerce (cdr entry) 'string)))
		    index))
		 (incf index)))
	  (setf (timestamp view)
		(cluffer:update (climacs2-base:cluffer-buffer buffer)
				(timestamp view)
				#'sync #'skip #'modify #'create)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAKE-EMPTY-FUNDAMENTAL-VIEW.
;;;
;;; This function creates an empty, hidden, fundamental view.

(defun make-empty-fundamental-view ()
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (let ((analyzer (make-instance 'climacs2-base:null-analyzer
		      :buffer buffer)))
      (make-instance 'fundamental-view
	:history nil
	:cursor cursor
	:analyzer analyzer))))
