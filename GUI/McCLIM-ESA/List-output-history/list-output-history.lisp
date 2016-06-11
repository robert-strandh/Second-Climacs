(cl:in-package #:climacs-list-output-history)

(defclass list-output-history (clim:standard-tree-output-history)
  ((%lines :initform (list) :reader lines)
   (%time-stamp :initform nil :accessor time-stamp)
   (%buffer :initarg :buffer :reader buffer)))
