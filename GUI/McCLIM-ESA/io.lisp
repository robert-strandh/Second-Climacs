(cl:in-package #:second-climacs-clim-base)

(defmethod esa-buffer:frame-make-buffer-from-stream ((frame climacs) stream)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (climacs2-base:fill-buffer-from-stream cursor stream)
    buffer))

(defmethod esa-buffer:frame-save-buffer-to-stream
    ((frame climacs) (buffer climacs2-base:standard-buffer) stream)
  (let* ((cluffer-buffer (climacs2-base:cluffer-buffer buffer))
         (class 'cluffer-standard-line:right-sticky-cursor)
         (cursor (make-instance class))
         (first-line (cluffer:find-line cluffer-buffer 0)))
    (cluffer:attach-cursor cursor first-line)
    (loop until (cluffer:end-of-buffer-p cursor)
          do (write-char (cluffer-emacs:item-after-cursor cursor) stream)
             (cluffer-emacs:forward-item cursor))))

(defmethod esa-buffer:frame-make-new-buffer ((frame climacs) &key)
  (multiple-value-bind (buffer cursor)
      (climacs2-base:make-empty-standard-buffer-and-cursor)
    (declare (ignore cursor))
    buffer))

(defun file-name-extension (file-path)
  (let* ((namestring (namestring file-path))
         (dot-position (position #\. namestring :from-end t)))
    (if (null dot-position)
        nil
        (subseq namestring (1+ dot-position)))))
