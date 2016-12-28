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
  (clim:make-pane 'text-pane
		  :name 'stuff
		  :default-view clim:+textual-view+
		  :width 900 :height 400
		  :display-time nil
		  :command-table 'global-climacs-table))

(defun pane-has-attached-view-p (pane)
  (typep (clim:stream-default-view pane) 'climacs-clim-view))

(defun detach-view (pane)
  (assert (pane-has-attached-view-p pane))
  (climacs2-base:hide-view (climacs-view (clim:stream-default-view pane)))
  (setf (clim:stream-default-view pane) clim:+textual-view+))

(defun attach-view (pane view)
  (assert (not (pane-has-attached-view-p pane)))
  (climacs2-base:expose-view view pane)
  (setf (clim:stream-default-view pane)
	(make-climacs-clim-view view)))

(clim:define-application-frame climacs (esa:esa-frame-mixin
					clim:standard-application-frame
					climacs2-base:application)
  ((%buffers :initarg :buffers :reader esa:buffers)
   (%current-view :initarg :current-view :accessor current-view))
  (:panes
   (window (let* ((my-pane (make-climacs-pane))
		  (my-info-pane (clim:make-pane 'info-pane
						:master-pane my-pane
						:width 900))
		  (view (make-instance 'climacs2-base:fundamental-view)))
	     (setf (clim:stream-recording-p my-pane) nil)
	     (setf (clim:stream-end-of-line-action my-pane) :allow)
	     (change-class
	      (clim:stream-output-history my-pane)
	      'climacs-flexichain-output-history:flexichain-output-history
	      :parent my-pane)
	     ;; Unfortunately, the ESA top-level accesses the slot
	     ;; named WINDOWS directly (using WITH-SLOTS) rather than
	     ;; using the accessor, so we must initialize this slot
	     ;; by using the slot writer provided by ESA.
	     (setf (esa:windows clim:*application-frame*) (list my-pane))
	     (attach-view my-pane view)
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

(defmethod clim:adopt-frame :after (frame-manager (frame climacs))
  (declare (ignore frame-manager))
  (let* ((pane (clim:find-pane-named frame 'stuff))
	 (clim-view (clim:stream-default-view pane)))
    (push (climacs-view clim-view)
	  (climacs2-base:views frame))))

(defmethod esa:windows ((esa climacs))
  (loop for view in (climacs2-base:views esa)
	for window = (climacs2-base:window view)
	unless (null window)
	  collect window))

(defmethod esa:buffers ((esa climacs))
  (remove-duplicates (loop for view in (climacs2-base:views esa)
			   for analyzer = (climacs2-base:analyzer view)
			   collect (climacs2-base:buffer analyzer))
		     :test #'eq))

(defun climacs ()
  (let ((frame (clim:make-application-frame
		'climacs
		:views '()
		:buffers '())))
    (clim:run-frame-top-level frame)))

(clim:define-command (com-inspect :name t :command-table global-climacs-table) ()
  (clouseau:inspector clim:*application-frame*))

(defmethod clim:frame-standard-input ((frame climacs))
  (clim:find-pane-named frame 'minibuffer))

(defmethod clim:redisplay-frame-panes :after ((frame climacs) &key force-p)
  (declare (ignore force-p))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'stuff))
	 (clim-view (clim:stream-default-view pane))
	 (history (clim:stream-output-history pane)))
    (climacs2-base:update-view (climacs-view clim-view))
    (climacs-flexichain-output-history:change-space-requirements history)
    (clim:replay history pane)))
