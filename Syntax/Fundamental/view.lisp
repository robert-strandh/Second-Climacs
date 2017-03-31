(cl:in-package #:climacs-syntax-fundamental)

(defclass view (climacs2-base:view) ())

(defmethod initialize-instance :after ((instance view) &key buffer cursor)
  (let ((analyzer (make-instance 'analyzer :buffer buffer)))
    (reinitialize-instance instance
                           :cursor cursor
                           :analyzer analyzer)))

(defmethod second-climacs-clim-base:view-name ((view view))
  "Fundamental")
