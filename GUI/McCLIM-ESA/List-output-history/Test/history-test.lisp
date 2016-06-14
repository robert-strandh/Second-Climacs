(cl:in-package #:climacs-list-output-history-test)

(clim:define-application-frame history-test ()
  ()
  (:panes
   (app :application
	:width 200
	:height 300
	:display-time nil
	:scroll-bars t)
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
    (setf (clim:stream-recording-p pane) nil)
    (change-class (clim:stream-output-history pane)
		  'climacs-list-output-history:list-output-history
		  :parent pane)))

(define-history-test-command (com-insert :name t)
    ((place 'integer) (text 'string))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'app))
	 (history (clim:stream-output-history pane))
	 (buffer (climacs-list-output-history:buffer history))
	 (line (cluffer:find-line buffer place))
	 (ignore (cluffer:split-line-at-position line 0))
	 (new-line (cluffer:find-line buffer place)))
    (declare (ignore ignore))
    (loop for char across text
	  for pos from 0
	  do (cluffer:insert-item-at-position new-line char pos))
    (climacs-list-output-history::update history)))
