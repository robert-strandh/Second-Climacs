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
