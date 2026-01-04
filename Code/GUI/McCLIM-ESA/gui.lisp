(cl:in-package #:second-climacs-clim-base)

(defparameter *info-pane-text-style*
  (clim:make-text-style :fix :roman 14))

(defclass info-pane (info:info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :text-style *info-pane-text-style*
   :display-function 'display-info
   :incremental-redisplay t))

(defun display-info (frame info-pane)
  (let* ((pane (esa:esa-current-window frame))
         (clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view clim-view)))
    (format info-pane
            " ~a (~a)"
            (esa-utils:name (esa:current-buffer))
            (base:view-name climacs-view))))

(defparameter *minibuffer-pane-text-style*
  (clim:make-text-style :fix :roman 14))

(defclass minibuffer-pane (esa:minibuffer-pane)
  ()
  (:default-initargs
   :text-style *minibuffer-pane-text-style*
   :height 20 :max-height 20 :min-height 20))

(defun make-gutter-pane ()
  (clim:make-pane 'clim:application-pane
                  :background clim:+gray+
                  :max-width 12
                  :width 12
                  :display-time nil))

(defun make-climacs-pane (gutter)
  (clim:make-pane 'text-pane
                  :name 'stuff
                  :left-gutter gutter
                  :default-view clim:+textual-view+
                  :width 900 :height 900
                  :display-time nil))

(defun pane-has-attached-view-p (pane)
  (typep (clim:stream-default-view pane) 'climacs-clim-view))

(defun detach-view (pane)
  (assert (pane-has-attached-view-p pane))
  (base:hide-view (climacs-view (clim:stream-default-view pane)))
  (setf (clim:stream-default-view pane) clim:+textual-view+))

(defun attach-view (pane view)
  (assert (not (pane-has-attached-view-p pane)))
  (base:expose-view view pane)
  (let* ((climacs-clim-view (make-climacs-clim-view view))
         (history (output-history climacs-clim-view)))
    (reinitialize-instance history :stream pane)
    (setf (clim:stream-default-view pane) climacs-clim-view)))

;;; We define our own HRACK-PANE as a subclass of the one provided by
;;; CLIM.  We do this so that we can define an :AFTER method on
;;; CLIM:NOTE-SHEET-TRANSFORMATION-CHANGED, specialized to this new
;;; class.  This :AFTER method is used to detect scrolling, and we
;;; then check whether the cursor is outside the viewport, in which
;;; case we move the cursor.
(defclass hrack-pane (clim:hrack-pane) ())

(clim:define-application-frame climacs (esa:esa-frame-mixin
                                        clim:standard-application-frame
                                        base:application)
  ()
  (:menu-bar nil)
  (:panes
   (window
    (let ((buffer (base::make-empty-standard-buffer)))
      (setf (esa-buffer:filepath buffer) (first (directory ".")))
      (let* ((gutter (make-gutter-pane))
             (my-pane (make-climacs-pane gutter))
             (my-info-pane (clim:make-pane 'info-pane
                                           :master-pane my-pane
                                           :width       900))
             (view (make-instance 'cl-syntax:view :buffer buffer)))
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
            (clim:make-pane 'hrack-pane
                            :contents (list gutter my-pane)))
          my-info-pane))))
   (minibuffer (clim:make-pane 'minibuffer-pane :width 900)))
  (:layouts
   (default (clim:vertically ()
              window
              minibuffer)))
  (:top-level (esa:esa-top-level)))

(defmethod clim:note-sheet-transformation-changed :after
    ((pane hrack-pane))
  (let ((pane (esa:current-window)))
    (move-cursor-to-viewport pane)))

(defmethod clim:adopt-frame :after (frame-manager (frame climacs))
  (declare (ignore frame-manager))
  (let* ((pane (clim:find-pane-named frame 'stuff))
         (clim-view (clim:stream-default-view pane)))
    (push (climacs-view clim-view)
          (base:views frame))))

(defmethod esa:windows ((esa climacs))
  (loop for view in (base:views esa)
        for window = (base:window view)
        unless (null window)
          collect window))

(defmethod esa:buffers ((esa climacs))
  (remove-duplicates (loop for view in (base:views esa)
                           for analyzer = (base:analyzer view)
                           collect (base:buffer analyzer))
                     :test #'eq))

(defun climacs (&key new-process (process-name "Climacs"))
  (with-open-file (stream
                   (uiop:xdg-config-home "second-climacs" "init.lisp")
                   :if-does-not-exist nil)
    (unless (null stream)
      (load stream)))
  (let ((frame (clim:make-application-frame 'climacs)))
    (flet ((run () (clim:run-frame-top-level frame)))
      (if new-process
          (clim-sys:make-process #'run :name process-name)
          (run)))))

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
          (move-viewport-to-cursor pane)))
    ((or cluffer:cluffer-error
         text.editing:editing-condition
         base:climacs-error)
        (condition)
      (mini:display-message "~a" condition))))

(defmethod esa:find-applicable-command-table ((frame climacs))
  (let* ((pane (esa:esa-current-window frame))
         (climacs-clim-view (clim:stream-default-view pane))
         (climacs-view (climacs-view climacs-clim-view)))
    (command-table climacs-view)))

(defmethod clim:frame-exit :around ((frame climacs))
  (with-current-cursor (cursor)
    (let ((buffer (cluffer:buffer cursor)))
      (when (and (esa-buffer:needs-saving buffer)
                 (handler-case
                     (clim:accept 'boolean
                                  :prompt (format nil "Save buffer?"))
                   (error () (progn (clim:beep)
                                    (mini:display-message "Invalid answer")
                                    (return-from clim:frame-exit nil)))))
        (esa-io:save-buffer buffer))))
  (call-next-method))
