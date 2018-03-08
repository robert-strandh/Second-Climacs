(cl:in-package #:second-climacs-clim-base)

(defclass info-pane (esa:info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :display-function 'display-info
   :incremental-redisplay t))

(defun display-info (frame info-pane)
  (let* ((pane (esa:esa-current-window frame))
         (clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view clim-view)))
    (format info-pane
            " ~a (~a)"
            (esa-utils:name (esa:current-buffer))
            (view-name climacs-view))))

(defclass minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20))

(defun make-climacs-pane ()
  (clim:make-pane 'text-pane
                  :name 'stuff
                  :default-view clim:+textual-view+
                  :width 900 :height 900
                  :display-time nil))

(defun pane-has-attached-view-p (pane)
  (typep (clim:stream-default-view pane) 'climacs-clim-view))

(defun detach-view (pane)
  (assert (pane-has-attached-view-p pane))
  (climacs2-base:hide-view (climacs-view (clim:stream-default-view pane)))
  (setf (clim:stream-default-view pane) clim:+textual-view+))

(defun attach-view (pane view)
  (assert (not (pane-has-attached-view-p pane)))
  (climacs2-base:expose-view view pane)
  (let ((climacs-clim-view (make-climacs-clim-view view)))
    (setf (clim:output-record-parent (output-history climacs-clim-view))
          pane)
    (setf (clim:stream-default-view pane) climacs-clim-view)))

(clim:define-application-frame climacs (esa:esa-frame-mixin
                                        clim:standard-application-frame
                                        climacs2-base:application)
  ()
  (:panes
   (window
    (multiple-value-bind (buffer cursor)
        (climacs2-base:make-empty-standard-buffer-and-cursor)
      (setf (esa-buffer:filepath buffer)
            (first (directory ".")))
      (let* ((my-pane (make-climacs-pane))
             (my-info-pane (clim:make-pane 'info-pane
                                           :master-pane my-pane
                                           :width 900))
             ;; (view (make-instance 'climacs-syntax-fundamental:view
             ;;        :buffer buffer :cursor cursor))
             (view (make-instance 'climacs-syntax-common-lisp:view
                     :buffer buffer :cursor cursor)))
        (setf (clim:stream-recording-p my-pane) nil)
        (setf (clim:stream-end-of-line-action my-pane) :allow)
        ;; Unfortunately, the ESA top-level accesses the slot
        ;; named WINDOWS directly (using WITH-SLOTS) rather than
        ;; using the accessor, so we must initialize this slot
        ;; by using the slot writer provided by ESA.
        (setf (esa:windows clim:*application-frame*) (list my-pane))
        (attach-view my-pane view)
        (clim:vertically ()
          (clim:scrolling (:height 700)
            my-pane)
          my-info-pane))))
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

(defmethod clim:frame-standard-input ((frame climacs))
  (clim:find-pane-named frame 'minibuffer))

(defmethod clim:redisplay-frame-panes :after ((frame climacs) &key force-p)
  (declare (ignore force-p))
  (let* ((pane (clim:find-pane-named clim:*application-frame* 'stuff))
         (clim-view (clim:stream-default-view pane)))
    (update-view pane clim-view)))

(defmethod clim:execute-frame-command :around ((frame climacs) command)
  (handler-case
      (progn
        (call-next-method)
        ;; This is probably wrong.  All windows may be concerned.
        (let ((pane (esa:current-window)))
          (second-climacs-clim-view-common-lisp:move-viewport-to-cursor pane)))
    (cluffer:cluffer-error (condition)
      (esa:display-message "~a" condition))))

(defmethod esa:find-applicable-command-table ((frame climacs))
  (let* ((pane (esa:esa-current-window frame))
         (climacs-clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view climacs-clim-view)))
    (command-table climacs-view)))
