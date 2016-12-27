(cl:in-package #:climacs-esa-gui)

(defclass info-pane (esa:info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :display-function 'display-info
   :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (clim:pane-name (esa:master-pane pane))))

(defclass minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20))

(clim:define-command-table global-climacs-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table
   ascii-insert-table))

(defun make-climacs-pane ()
  (let ((view (make-instance 'climacs-clim-view :climacs-view nil)))
    (clim:make-pane 'text-pane
		    :name 'stuff
		    :default-view view
		    :width 900 :height 400
		    :display-time nil
		    :command-table 'global-climacs-table)))

(defun pane-has-attached-view-p (pane)
  (not (null (climacs-view (clim:stream-default-view pane)))))

(defun detach-view (pane)
  (assert (pane-has-attached-view-p pane))
  (climacs2-base:hide-view (climacs-view (clim:stream-default-view pane)))
  (setf (climacs-view (clim:stream-default-view pane)) nil))

(clim:define-application-frame climacs (esa:esa-frame-mixin
					clim:standard-application-frame)
  ((%views :initarg :views :reader views)
   (%buffers :initarg :buffers :reader esa:buffers)
   (%current-view :initarg :current-view :accessor current-view))
  (:panes
   (window (let* ((my-pane (make-climacs-pane))
		  (my-info-pane (clim:make-pane 'info-pane
						:master-pane my-pane
						:width 900)))
	     (setf (clim:stream-recording-p my-pane) nil)
	     (setf (clim:stream-end-of-line-action my-pane) :allow)
	     (change-class
	      (clim:stream-output-history my-pane)
	      'climacs-flexichain-output-history:flexichain-output-history
	      :parent my-pane)
	     (setf (esa:windows clim:*application-frame*) (list my-pane))
	     (clim:vertically ()
	       (clim:scrolling ()
		 my-pane)
	       my-info-pane)))
   (minibuffer (clim:make-pane 'minibuffer-pane :width 900)))
  (:layouts
   (default (clim:vertically (:scroll-bars nil)
	      window
	      minibuffer)))
  (:top-level (esa:esa-top-level)))

(defun climacs ()
  (clim:run-frame-top-level
   (clim:make-application-frame
    'climacs
    :views '()
    :buffers '())))

(clim:define-command (com-inspect :name t :command-table global-climacs-table) ()
  (clouseau:inspector clim:*application-frame*))

(defmethod clim:frame-standard-input ((frame climacs))
  (clim:find-pane-named frame 'minibuffer))

(defmethod clim:redisplay-frame-panes :after ((frame climacs) &key force-p)
  (declare (ignore force-p))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'stuff))
	 (history (clim:stream-output-history pane)))
    (climacs-flexichain-output-history:change-space-requirements history)
    (clim:replay history pane)))
