(cl:in-package #:climacs-syntax-fundamental)

(defclass cache ()
  ((%prefix :initform () :accessor prefix)
   (%suffix :initform () :accessor suffix)))

   
