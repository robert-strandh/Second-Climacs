(cl:in-package #:clim-simple-editor-record-test)

(clim:define-application-frame record-test ()
  ()
  (:panes
   (app :application :width 200 :height 300)
   (int :interactor :width 200 :height 100))
  (:layouts
   (default (clim:vertically () app int))))

(defun record-test ()
  (clim:run-frame-top-level (clim:make-application-frame 'record-test)))

(define-record-test-command (quit :name t) ()
  (clim:frame-exit clim:*application-frame*))

(define-record-test-command (inspect :name t) ()
  (clueless:inspect clim:*application-frame*))

(define-record-test-command (com-trace :name t) ()
  (format *trace-output* "~%++++++++++++++++++++ trace~%")
  (trace clim:replay-output-record))

(define-record-test-command (com-untrace :name t) ()
  (format *trace-output* "~%-------------------- untrace~%")
  (untrace))

;;; Replace the existing top-level output record in the application
;;; pane by an instance of our new one.
(define-record-test-command (com-replace :name t) ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'app)))
    (reinitialize-instance
     pane
     :output-record (make-instance 'clim-simple-editor-record:record))))

(define-record-test-command (com-insert :name t)
    ((line-number 'integer) (text 'string))
  (declare (ignore line-number text)))
