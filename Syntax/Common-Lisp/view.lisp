(cl:in-package #:climacs-syntax-common-lisp)

(defclass view (climacs2-base:view) ())

(setf (second-climacs-base:view-class "lisp") 'view)

(defmethod initialize-instance :after ((instance view) &key buffer cursor)
  (let* ((cache (make-instance 'cache))
         (analyzer (make-instance 'analyzer
                     :folio cache
                     :buffer buffer)))
    (reinitialize-instance instance
                           :cursor cursor
                           :analyzer analyzer)))

(defmethod second-climacs-clim-base:view-name ((view view))
  "Common Lisp")
