(cl:in-package #:climacs-gui)

(defun status-line ()
  (let* ((background-color
	   (clim3-gadgets:background-color clim3-gadgets:*theme*))
	 (background (clim3:opaque background-color))
	 (style (clim3:text-style :free :fixed :bold 12))
	 (color (clim3:make-color 0.0 0.0 0.0))
	 ;; Put in a text for testing purposes.
	 (text (clim3-text:text "-:**- climacs.lisp      0% (0,0) (Common Lisp)"
			   style color))
	 (thing (clim3:bboard* text))
	 (height (clim3:vframe 20 20 20)))
    (setf (clim3:depth background) 10)
    (clim3:pile* thing background height)))

(defun minibuffer ()
  (let* ((background-color (clim3:make-color 1.0 1.0 1.0))
	 (background (clim3:opaque background-color))
	 (thing (clim3:wrap))
	 (height (clim3:vframe 20 20 20)))
    (clim3:pile* thing background height)))

(defclass climacs (clim3:application)
  ((%current-view :initarg :current-view :accessor clim3:current-view)
   (%zones :initarg :zones :accessor zones)))

(defmethod clim3:command-loop-iteration :around ((application climacs) view)
  (let ((climacs-commands:*point* (climacs-view:cursor view)))
    (call-next-method))
  ;; FIXME: update every analyzer, not only that of the current view.
  (climacs-analyzer-fundamental:update (climacs-view:analyzer view))
  ;; FIXME: update every visible view, not only the current view.
  (let ((show (climacs-view:show view)))
    (unless (null show)
      (climacs-show:update show))))

(defun climacs ()
  (let* ((wrap (clim3:wrap))
	 (scroll (clim3:scroll wrap))
	 (scrollbar (clim3-gadgets:vscrollbar scroll))
	 (margin-color (clim3:make-color 0.9 0.9 0.95))
	 (lmargin (clim3:pile* (clim3:opaque margin-color) (clim3:hbrick 10)))
	 (rmargin (clim3:pile* (clim3:opaque margin-color) (clim3:hbrick 10)))
	 (hbox (clim3:hbox* scrollbar lmargin scroll rmargin))
	 (size (clim3:brick 800 1000))
	 (tooltip (clim3:bboard*))
	 (status (status-line))
	 (minibuffer (minibuffer))
	 (buffer (with-open-file (stream "test-input" :direction :input)
		   (climacs-basic-emacs:buffer-from-stream stream)))
	 (analyzer (make-instance 'climacs-analyzer-fundamental:fundamental-analyzer
		     :buffer buffer))
	 (cursor (let* ((line0 (climacs-buffer:find-line buffer 0))
			(cursor (climacs-buffer:make-right-sticky-cursor)))
		   (climacs-buffer:attach-cursor cursor line0 0)
		   cursor))
	 (show (climacs-show:make-show analyzer cursor wrap))
	 (command-processor
	   (climacs-commands:make-fundamental-command-processor))
	 (view (make-instance 'climacs-view:view
		 :analyzer analyzer
		 :show show
		 :command-key-processor command-processor
		 :cursor cursor))
	 (info (clim3:vbox* hbox status minibuffer))
	 (all (clim3:pile* tooltip info size))
	 (clim3:*application* (make-instance 'climacs
				:current-view view
				:zones all)))
    (climacs-analyzer-fundamental:update analyzer)
    (climacs-show:update show)
    ;; (clueless:inspect all)
    (let ((clim3:*port* (clim3:make-port :clx-framebuffer)))
      (clim3:connect all clim3:*port*)
      (catch :quit
	(clim3:command-loop))
      (clim3:disconnect all clim3:*port*))))
	

