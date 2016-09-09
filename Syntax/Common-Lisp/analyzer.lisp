(cl:in-package #:climacs-syntax-common-lisp)

(defclass analyzer ()
  ((%prefix :initform '() :accessor prefix)
   (%suffix :initform '() :accessor suffix)
   (%residue :initform '() :accessor residue)
   (%worklist :initform '() :accessor worklist)))
