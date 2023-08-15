(cl:in-package #:second-climacs-clim-view-common-lisp)

(defmacro with-current-cursor-and-cache
    ((cursor-variable cache-variable) &body body)
  (let ((view-variable (gensym))
        (climacs-view-variable (gensym))
        (analyzer-variable (gensym)))
    `(let* ((,view-variable (clim:stream-default-view (esa:current-window)))
            (,climacs-view-variable (clim-base:climacs-view ,view-variable))
            (,analyzer-variable (base:analyzer ,climacs-view-variable))
            (,cache-variable (ip:cache ,analyzer-variable)))
       (base:update-view ,climacs-view-variable)
       (clim-base:with-current-cursor (,cursor-variable)
         ,@body))))
