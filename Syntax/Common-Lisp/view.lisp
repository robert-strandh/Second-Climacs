(cl:in-package #:climacs-syntax-common-lisp)

(defclass view (base:view) ())

(setf (base:view-class "lisp") 'view)

(defmethod initialize-instance :after ((instance view) &key buffer cursor)
  (let* ((cache (make-instance 'cache))
         (analyzer (make-instance 'analyzer
                     :folio cache
                     :buffer buffer)))
    (reinitialize-instance instance
                           :cursor cursor
                           :analyzer analyzer)))

(defmethod clim-base:view-name ((view view))
  "Common Lisp")
