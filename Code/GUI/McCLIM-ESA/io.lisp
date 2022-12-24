(cl:in-package #:second-climacs-clim-base)

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (multiple-value-bind (buffer cursor)
      (base:make-empty-standard-buffer-and-cursor)
    (base:fill-buffer-from-stream cursor stream)
    buffer))

(defmethod esa-buffer:frame-save-buffer-to-stream
    ((frame climacs) (buffer base:standard-buffer) stream)
  (let* ((cluffer-buffer (base:cluffer-buffer buffer))
         (class 'cluffer-standard-line:right-sticky-cursor)
         (cursor (make-instance class))
         (first-line (cluffer:find-line cluffer-buffer 0)))
    (cluffer:attach-cursor cursor first-line)
    (loop until (cluffer:end-of-buffer-p cursor)
          do (write-char (cluffer-emacs:item-after-cursor cursor) stream)
             (cluffer-emacs:forward-item cursor))))

(defmethod esa-buffer:frame-make-new-buffer ((frame climacs) &key)
  (multiple-value-bind (buffer cursor)
      (base:make-empty-standard-buffer-and-cursor)
    (declare (ignore cursor))
    buffer))

(defun file-name-extension (file-path)
  (let* ((namestring (namestring file-path))
         (dot-position (position #\. namestring :from-end t)))
    (if (null dot-position)
        nil
        (subseq namestring (1+ dot-position)))))

(defmethod esa-io:frame-find-file :around ((frame climacs) file-path)
  (let* ((extension (file-name-extension file-path))
         (view-class (base:view-class extension))
         (buffer (call-next-method))
         (cluffer-buffer (base:cluffer-buffer buffer))
         (first-line (cluffer:find-line cluffer-buffer 0))
         (window (esa:current-window)))
    (when (null view-class)
      (setf view-class 'fundamental-syntax:view))
    (let* ((cursor (make-instance 'base:standard-cursor
                     :buffer buffer))
           (view (make-instance view-class :buffer buffer :cursor cursor)))
      (cluffer:attach-cursor cursor first-line)
      (push view (base:views frame))
      (detach-view window)
      (attach-view window view))))

;;; For some reason, write-file gives a string rather than a pathname
;;; to the default method of this generic function.
(defmethod esa-io:check-buffer-writability
    (application-frame (pathname string) (buffer base:standard-buffer))
  t)
