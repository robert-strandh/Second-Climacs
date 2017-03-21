(cl:in-package #:climacs-syntax-fundamental)

(defclass entry ()
  ((%line :initarg :line :reader line)
   (%contents :initarg :contents :reader contents)))

(defclass cache ()
  ((%prefix :initform () :accessor prefix)
   (%suffix :initform () :accessor suffix)))
