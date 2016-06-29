(cl:in-package #:climacs-flexichain-view)

(defclass view ()
  ((%pane :initarg :pane :reader pane)
   (%lines :initarg :lines :reader lines)))
