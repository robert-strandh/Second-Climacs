(cl:in-package #:climacs-esa-gui)

(defgeneric view-name (view)
  (:method (view) "Unknown"))

(defmethod view-name ((view climacs2-base:fundamental-view))
  "Fundamental")

(defmethod view-name ((view common-lisp-view))
  "Common Lisp")
