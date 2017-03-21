(cl:in-package #:climacs-syntax-fundamental)

(defclass entry ()
  (;; This slot contains the Cluffer line object so that we can
   ;; find it when we process a SYNC operation.
   (%line :initarg :line :reader line)
   ;; This slot contains the contents of the line as it was when we
   ;; last updated the cache.
   (%contents :initarg :contents :reader contents)
   ;; This slot contains the number of entries in the list (prefix or
   ;; suffix) that this entry is a member of.
   (%list-length :initarg list-length accessor list-length)))

(defclass cache ()
  ((%prefix :initform () :accessor prefix)
   (%suffix :initform () :accessor suffix)))
