(cl:in-package #:second-climacs-syntax-fundamental)

(defclass view (base:view) ())

(defmethod initialize-instance :after ((instance view) &key buffer cursor)
  (let ((analyzer (make-instance 'analyzer :buffer buffer)))
    (reinitialize-instance instance
                           :cursor cursor
                           :analyzer analyzer)))

(defmethod base:view-name ((view view))
  "Fundamental")
