(cl:in-package #:climacs-esa-gui)

;;; This class is is a subclass of CLIMACS-CLIM-VIEW.  An instance of
;;; this class is created in order to display a Climacs view that is
;;; an instance of CLIMACS2-BASE:FUNDAMENTAL-VIEW.

(defclass fundamental-view (climacs-clim-view)
  ((%previous-cursor-line-number
    :initform -1
    :accessor previous-cursor-line-number)
   (%previous-cursor-column-number
    :initform 0
    :accessor previous-cursor-column-number)))

(defmethod climacs-clim-view-class ((view climacs2-base:fundamental-view))
  (find-class 'fundamental-view))

(defmethod command-table ((view  climacs2-base:fundamental-view))
  (clim:find-command-table 'fundamental-table))

(defun make-output-record (items pane cursor-column)
  (clim:with-output-to-output-record (pane)
    (format pane "~a" (coerce items 'string))))

;;; Since the FUNDAMENTAL-VIEW contains a NULL-ANALYZER, the method on
;;; UPDATE-VIEW-FROM-ANALYZER specialized to the NULL-ANALYZER calls
;;; UPDATE-VIEW-FROM-ANALYZER recursively with the buffer replacing
;;; the analyzer.  That is why the following method is specialized to
;;; the STANDARD-BUFFER, and not to a subclass of ANALYZER.

(defmethod climacs2-base:update-view-from-analyzer
    ((view climacs2-base:fundamental-view)
     (pane text-pane)
     (buffer climacs2-base:standard-buffer))
  (let* ((index 0)
	 (climacs-clim-view (clim:stream-default-view pane))
	 (cursor (climacs2-base:cursor view))
	 (cursor-line-number (cluffer:line-number cursor))
	 (cursor-column-number (cluffer:cursor-position cursor))
	 (lines (lines climacs-clim-view))
	 (history (clim:stream-output-history pane))
	 (previous-cursor-line-number (previous-cursor-line-number view))
	 (previous-cursor-column-number (previous-cursor-column-number view)))
    (flet ((delete-line (index)
	     (unless (null previous-cursor-line-number)
	       (if (= previous-cursor-line-number index)
		   (setf previous-cursor-line-number nil)
		   (decf previous-cursor-line-number)))
	     (flexichain:delete* lines index)
	     (climacs-flexichain-output-history:delete history index)))
      (flet ((delete-lines-until-line (line)
	       (loop for entry = (flexichain:element* lines index)
		     until (eq line (car entry))
		     do (delete-line index))))
	(flet ((skip (n)
		 (cond  ((or (< previous-cursor-line-number index)
			     (>= previous-cursor-line-number (+ index n)))
			 ;; We do not have to process the line in which
			 ;; the cursor was previously located, but we may
			 ;; have to process the line in which the cursor
			 ;; is now located.
			 (unless (>= cursor-line-number (+ index n))
			   ;; We must re-create the output record of
			   ;; the line in which the cursor is now
			   ;; located.
			   (let ((entry (flexichain:element*
					 history
					 cursor-line-number)))
			     (climacs-flexichain-output-history:replace
			      history
			      (make-output-record (cdr entry)
						  pane
						  cursor-column-number)
			       cursor-line-number))))
			;; We may have to process the line in which
			;; the cursor was previously located.
			((>= cursor-line-number (+ index n))
			 ;; The cursor is now in a line beyond the
			 ;; block we are skipping, so we only have to
			 ;; process the line in which the cursor was
			 ;; previously located.
			 (let ((entry (flexichain:element*
				       history
				       previous-cursor-line-number)))
			   (climacs-flexichain-output-history:replace
			    history
			    (make-output-record (cdr entry) pane nil)
			    previous-cursor-line-number)))
			;; Both the line in which the cursor was
			;; previously located, and the line in which
			;; the cursor is now located are inside the
			;; block we are skipping.  So we may have to
			;; process both those lines.
			((= cursor-line-number previous-cursor-line-number)
			 ;; We have to process at most one line.
			 (unless (= cursor-column-number
				    previous-cursor-column-number)
			   ;; We have to process the line.
			   (let ((entry (flexichain:element*
					 history
					 cursor-line-number)))
			     (climacs-flexichain-output-history:replace
			      history
			      (make-output-record (cdr entry)
						  pane
						  cursor-column-number)
			       cursor-line-number))))
			(t ;; The cursor is in a different line from
			   ;; before.
			 (let ((entry (flexichain:element*
				       history
				       previous-cursor-line-number)))
			   (climacs-flexichain-output-history:replace
			    history
			    (make-output-record (cdr entry) pane nil)
			    previous-cursor-line-number)
			   (let ((entry (flexichain:element*
					 history
					 cursor-line-number)))
			     (climacs-flexichain-output-history:replace
			      history
			      (make-output-record (cdr entry)
						  pane
						  cursor-column-number)
			      cursor-line-number)))))
		 (incf index n))
	       (modify (line)
		 (delete-lines-until-line line)
		 (let ((entry (flexichain:element* lines index))
		       (column (if (= index cursor-line-number)
				   cursor-column-number
				   nil)))
		   (setf (cdr entry)
			 (cluffer:items (car entry)))
		   (climacs-flexichain-output-history:replace
		    history
		    (make-output-record (cdr entry) pane column)
		    index))
		 (incf index))
	       (sync (line)
		 (delete-lines-until-line line)
		 (incf index))
	       (create (line)
		 (let ((entry (cons line (cluffer:items line)))
		       (column (if (= index cursor-line-number)
				   cursor-column-number
				   nil)))
		   (flexichain:insert* lines index entry)
		   (climacs-flexichain-output-history:insert
		    history
		    (make-output-record (cdr entry) pane column)
		    index))
		 (unless (< previous-cursor-line-number index)
		   (incf previous-cursor-line-number))
		 (incf index)))
	  (setf (timestamp climacs-clim-view)
		(cluffer:update (climacs2-base:cluffer-buffer buffer)
				(timestamp climacs-clim-view)
				#'sync #'skip #'modify #'create))
	  (setf (previous-cursor-line-number view) cursor-line-number)
	  (setf (previous-cursor-column-number view) cursor-column-number))))))
