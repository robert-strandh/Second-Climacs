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

(defmethod command-table
    ((view  climacs-syntax-common-lisp:view))
  (clim:find-command-table 'common-lisp-table))

(defun make-output-record (items pane cursor-column-number)
  (let ((string (coerce items 'string)))
    (clim:with-output-to-output-record (pane)
      (if (null cursor-column-number)
          (format pane "~a" string)
          (let ((prefix (subseq string 0 cursor-column-number))
                (suffix (subseq string cursor-column-number)))
            (format pane "~a" prefix)
            (clim:with-room-for-graphics (pane :move-cursor nil)
              (clim:draw-rectangle* pane 0 -5 3 10
                                    :filled t
                                    :ink clim:+blue+))
            (clim:stream-increment-cursor-position pane 3 0)
            (format pane "~a" suffix))))))

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
