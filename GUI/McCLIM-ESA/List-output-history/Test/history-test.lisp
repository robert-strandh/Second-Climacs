(cl:in-package #:climacs-list-output-history-test)

(clim:define-application-frame history-test ()
  ()
  (:panes
   (app :application :width 200 :height 300 :display-time nil)
   (int :interactor :width 200 :height 100))
  (:layouts
   (default (clim:vertically () app int))))

(defun history-test ()
  (clim:run-frame-top-level (clim:make-application-frame 'history-test)))

(define-history-test-command (quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-history-test-command (inspect :name t) ()
  (clouseau:inspector
   (clim:stream-output-history
    (clim:find-pane-named clim:*application-frame* 'app))))

;;; Replace the existing top-level output record in the application
;;; pane by an instance of our new one.
(define-history-test-command (com-replace :name t) ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'app)))
    (reinitialize-instance
     pane
     :output-record
     (make-instance 'climacs-list-output-history:list-output-history
       :stream pane))))

(define-history-test-command (com-insert :name t)
    ((place 'integer) (text 'string))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'app))
	 (history (clim:stream-output-history pane))
	 (lines (climacs-list-output-history:lines history))
	 (prev lines)
	 (y 0)
	 new-record
	 height)
    (loop repeat place
	  do (setf prev (cdr prev))
	     (incf y (+ (clim:bounding-rectangle-height (car prev)) 5)))
    (setf (clim:stream-current-output-record pane) history)
    (setf new-record
	  (clim:with-new-output-record (pane)
	    (format pane "~a" text)))
    (setf (clim:output-record-position new-record)
	  (values 0 y))
    (setf height (clim:bounding-rectangle-height new-record))
    (loop for record in (cdr prev)
	  do (multiple-value-bind (x y) (clim:output-record-position record)
	       (setf (clim:output-record-position record)
		     (values x (+ y height 5)))))
    (clim:replay-output-record history pane)
    (push new-record (cdr prev))))
