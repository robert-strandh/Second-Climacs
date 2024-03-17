(cl:in-package #:second-climacs-clim-base)

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (let* ((buffer (base:make-empty-standard-buffer))
         (point (edit:point buffer)))
    (base:fill-buffer-from-stream point stream)
    buffer))

(defmethod esa-buffer:frame-save-buffer-to-stream
    ((frame climacs) (buffer base:standard-buffer) stream)
  (edit:with-temporary-cursor (cursor buffer)
    (loop until (cluffer:end-of-buffer-p cursor)
          do (write-char (edit::item-after-cursor* cursor) stream)
             (edit::move-item-forward cursor))))

(defmethod esa-buffer:frame-make-new-buffer ((frame climacs) &key)
  (base:make-empty-standard-buffer))

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
         (window (esa:current-window)))
    (clim:window-clear (left-gutter window))
    (when (null view-class)
      (setf view-class 'fundamental-syntax:view))
    (let ((view (make-instance view-class :buffer buffer)))
      (push view (base:views frame))
      (detach-view window)
      (attach-view window view))))

;;; For some reason, write-file gives a string rather than a pathname
;;; to the default method of this generic function.
(defmethod esa-io:check-buffer-writability
    (application-frame (pathname string) (buffer base:standard-buffer))
  t)
