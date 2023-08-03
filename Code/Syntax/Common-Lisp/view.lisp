(cl:in-package #:second-climacs-syntax-common-lisp)

(defclass view (base:view) ())

(setf (base:view-class "lisp") 'view)
(setf (base:view-class "asd") 'view)

(defmethod initialize-instance :after ((instance view) &key buffer cursor)
  (let* ((cache (make-instance 'cache))
         (analyzer (make-instance 'analyzer
                     :cache cache
                     :folio cache
                     :buffer buffer)))
    (reinitialize-instance instance
                           :cursor cursor
                           :analyzer analyzer)))

(defmethod base:view-name ((view view))
  "Common Lisp")
