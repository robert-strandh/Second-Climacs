(cl:in-package #:climacs-esa-gui)

;;; At the moment, this file contains code that is very similar to
;;; that of the fundamental view.  The reason is that we have not yet
;;; figured out how to use the parse results as a basis for the
;;; rendering.

(defclass common-lisp-view (climacs-clim-view)
  ((%previous-cursor-line-number
    :initform -1
    :accessor previous-cursor-line-number)
   (%previous-cursor-column-number
    :initform 0
    :accessor previous-cursor-column-number)))

(defmethod climacs-clim-view-class
    ((view climacs-syntax-common-lisp:view))
  (find-class 'common-lisp-view))

(defun draw-interval (pane line-number contents start-column end-column)
  (let* ((text-style (clim:medium-text-style pane))
	 (text-height (clim:text-style-height text-style pane))
	 (text-width (clim:text-style-width text-style pane))
	 (y (* line-number text-height))
	 (x (* start-column text-width)))
    (clim:draw-text* pane contents x y :start start-column :end end-column)))

(defun draw-area (pane
		  lines
		  start-line-number
		  start-column-number
		  end-line-number
		  end-column-number)
  (if (= start-line-number end-line-number)
      (let ((contents (cdr (flexichain:element* lines start-line-number))))
	(draw-interval pane
		       start-line-number
		       contents
		       start-column-number
		       end-column-number))
      (progn (let ((first (cdr (flexichain:element* lines start-line-number))))
	       (draw-interval pane
			      start-line-number
			      first
			      start-column-number
			      (length first)))
	     (let ((last (cdr (flexichain:element* lines end-line-number))))
	       (draw-interval pane
			      end-line-number
			      last
			      0
			      end-column-number))
	     (loop for line-number from (1+ start-line-number)
		     to (1- end-line-number)
		   for contents = (cdr (flexichain:element* lines line-number))
		   do (draw-interval pane
				     line-number
				     contents
				     0
				     (length contents))))))

(defmethod command-table
    ((view  climacs-syntax-common-lisp:view))
  (clim:find-command-table 'common-lisp-table))

(stealth-mixin:define-stealth-mixin
    presentation
    (clim:standard-presentation)
  climacs-syntax-common-lisp:parse-result
  (;; This slot contains the x-coordinate of the left border of the
   ;; presentation.  There might be buffer objects to the left of this
   ;; border, for instance if characters that are read are not part of
   ;; the output from the reader.  It typically won't happen, but it
   ;; might.  Either way, we only use the value of this slot for
   ;; highlighting, not for replaying.
   (%x-min :initform 0 :accessor x-min)
   ;; Similarly, this slot contains the x-coordinate of the right
   ;; border of the presentation.  There might be buffer objects to
   ;; the right of this border, but the value of this slot is only
   ;; used for highlighting.
   (%x-max :initform 0 :accessor x-max)
   ;; This slot contains the x-coordinate of the right edge of the
   ;; rightmost buffer object between the start and the end of the
   ;; parse result.  The value of this slot is used to determine the
   ;; total width of the pane that displays the buffer contents.
   (%width :initform 0 :accessor width)
   ;; This slot contains the height of the parse result.  The value of
   ;; this slot is used to determine the total height of the pane that
   ;; displays the buffer contents.
   (%height :initform 0 :accessor height)
   ;; This slot contains the maximum of all the values of all the
   ;; WIDTH slots in the parse results on either the prefix or the
   ;; suffix.  It is recomputed whenever a parse result is pushed to
   ;; one of those lists.
   (%max-width :initform 0 :accessor max-width)
   ;; This slot contains the total height of all the parse results on
   ;; either the prefix or the suffix.
   (%total-height :initform 0 :accessor total-height)
   ;; This slot contains the y-coordinate of the beginning of this
   ;; parse result.  It is computed when the parse result is pushed
   ;; onto the prefix, and it is only valid when the parse result is
   ;; an element of the prefix.  Thus, the output records that are
   ;; replayed, must all be on the prefix.
   (%top :initform 0 :accessor top))
  (:default-initargs :single-box t))

(defun update-cache (view pane analyzer)
  (declare (ignore view pane))
  (let* ((cache (climacs-syntax-common-lisp:folio analyzer))
	 (climacs-buffer (climacs2-base:buffer analyzer))
	 (cluffer-buffer (climacs2-base:cluffer-buffer climacs-buffer)))
    (climacs-syntax-common-lisp:scavenge cache cluffer-buffer)
    (climacs-syntax-common-lisp:read-forms analyzer)))

(defmethod climacs2-base:update-view-from-analyzer
    ((view climacs-syntax-common-lisp:view)
     (pane text-pane)
     (analyzer climacs-syntax-common-lisp:analyzer))
  (update-cache view pane analyzer)
  (let* ((buffer (climacs2-base:buffer analyzer))
	 (index 0)
         (climacs-clim-view (clim:stream-default-view pane))
         (cursor (climacs2-base:cursor view))
         (cursor-line-number (cluffer:line-number cursor))
         (cursor-column-number (cluffer:cursor-position cursor))
         (lines (lines climacs-clim-view))
         (history (clim:stream-output-history pane))
         (previous-cursor-line-number (previous-cursor-line-number
                                       climacs-clim-view))
         (previous-cursor-column-number (previous-cursor-column-number
                                         climacs-clim-view)))
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
                                         lines
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
                                       lines
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
                                         lines
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
                                       lines
                                       previous-cursor-line-number)))
                           (climacs-flexichain-output-history:replace
                            history
                            (make-output-record (cdr entry) pane nil)
                            previous-cursor-line-number)
                           (let ((entry (flexichain:element*
                                         lines
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
	  (loop until (= (flexichain:nb-elements lines) index)
		do (delete-line index))
          (setf (previous-cursor-line-number climacs-clim-view)
                cursor-line-number)
          (setf (previous-cursor-column-number climacs-clim-view)
                cursor-column-number))))))
