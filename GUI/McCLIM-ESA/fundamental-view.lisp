(cl:in-package #:climacs-esa-gui)

;;; A FUNDAMENTAL-VIEW contains a NULL-ANALYZER.

(defclass fundamental-view (climacs2-base:view)
  (;; This slot contains the timestamp corresponding to the
   ;; last time this view was updated from the Cluffer buffer.
   (%timestamp :initform nil :accessor timestamp)
   ;; This slot contains a flexichain of CONS cells.  The CAR of each
   ;; cell is a Cluffer line object.  The CDR is a vector containing
   ;; the contents of the Cluffer line object as it was when the
   ;; contents was asked for.
   (%lines :initform (make-instance 'flexichain:standard-flexichain))
   ;; This slot contains a flexichain-output-history object.
   (%history :initarg :history :reader history)))

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
	     (loop until (eq line (car (lines view)))
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
		      (format pane "~a" (cdr entry)))
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
		      (format pane "~a" (cdr entry)))
		    index))
		 (incf index)))
	  (setf (timestamp view)
		(cluffer:update (climacs2-base:cluffer-buffer buffer)
				(timestamp view)
				#'sync #'skip #'modify #'create)))))))
