(cl:in-package #:second-climacs-clim-base)

(defun current-view ()
  (let ((clim-view (clim:stream-default-view (esa:current-window))))
    (climacs-view clim-view)))

(defun current-cursor ()
  (edit:point (base:site (current-view))))

(defun current-buffer ()
  (cluffer:buffer (current-cursor)))

(defmacro with-current-cursor ((cursor-var) &body body)
  `(let ((,cursor-var (current-cursor)))
     ,@body))
