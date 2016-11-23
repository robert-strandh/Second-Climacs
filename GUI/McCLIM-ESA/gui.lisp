(cl:in-package #:climacs-esa-gui)

(defclass example-info-pane (esa:info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :display-function 'display-info
   :incremental-redisplay t))

(defclass example-minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20))

(clim:define-application-frame climacs (clim:standard-application-frame
					esa:esa-frame-mixin)
  ((%views :initarg :views :reader views)
   (%current-view :initarg :current-view :accessor current-view))
  (:panes
   (window (let* ((my-pane (clim:make-pane 'text-pane
					   :lines '()
					   :width 900 :height 400
					   :display-function 'display-my-pane
					   :command-table 'esa:global-esa-table))
		  (my-info-pane (clim:make-pane 'example-info-pane
						:master-pane my-pane
						:width 900)))
	     (setf (esa:windows clim:*application-frame*) (list my-pane))
	     (clim:vertically ()
	       (clim:scrolling ()
		 my-pane)
	       my-info-pane)))
   (minibuffer (clim:make-pane 'example-minibuffer-pane :width 900)))
  (:layouts
   (default (clim:vertically (:scroll-bars nil)
	      window
	      minibuffer)))
  (:top-level (esa:esa-top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame pane))
  nil)

(defun climacs ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'climacs)))
