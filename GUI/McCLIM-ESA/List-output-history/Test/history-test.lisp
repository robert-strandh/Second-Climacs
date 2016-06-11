(cl:in-package #:climacs-list-output-history-test)

(clim:define-application-frame history-test ()
  ()
  (:panes
   (app :application :width 200 :height 300)
   (int :interactor :width 200 :height 100))
  (:layouts
   (default (clim:vertically () app int))))

(defun history-test ()
  (clim:run-frame-top-level (clim:make-application-frame 'history-test)))

(define-history-test-command (quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-history-test-command (inspect :name t) ()
  (clueless:inspect clim:*application-frame*))

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
	 (x 0)
	 (new-record (clim:with-output-to-output-record (pane)
		       (format pane "~a" text)))
	 (height (clim:bounding-rectangle-height new-record)))
    (loop repeat place
	  do (setf prev (cdr prev))
	     (incf x (+ (clim:bounding-rectangle-height (car prev)) 5)))
    (loop for record in (cdr prev)
	  do (multiple-value-bind (x y) (clim:output-record-position record)
	       (setf (clim:output-record-position record)
		     (values (+ x height 5) y))))
    (setf (clim:output-record-position new-record)
	  (values (+ x height) 0))
    (clim:add-output-record new-record history)
    (push new-record (cdr prev))))
