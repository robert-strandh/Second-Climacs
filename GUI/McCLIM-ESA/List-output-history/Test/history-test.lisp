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
