(cl:in-package #:second-climacs-clim-base)

(defmacro with-current-cursor ((cursor-var) &body body)
  (let ((clim-view-var (gensym))
	(climacs-view-var (gensym)))
    `(let* ((,clim-view-var (clim:stream-default-view (esa:current-window)))
	    (,climacs-view-var (climacs-view ,clim-view-var))
	    (,cursor-var (climacs2-base:cursor ,climacs-view-var)))
       ,@body)))

(defmacro with-current-cursor-and-view ((cursor-var view-var) &body body)
  (let ((clim-view-var (gensym)))
    `(let* ((,clim-view-var (clim:stream-default-view (esa:current-window)))
	    (,view-var (climacs-view ,clim-view-var))
	    (,cursor-var (climacs2-base:cursor ,view-var)))
       ,@body)))

