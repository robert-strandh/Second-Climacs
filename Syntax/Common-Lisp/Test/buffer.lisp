(cl:in-package #:climacs-syntax-common-lisp-test)

(defclass line ()
  ((%items :initarg :items :accessor items)))

(defclass buffer ()
  ((%lines :initarg :lines :accessor lines)))

(defmethod cluffer:items ((line line) &key &allow-other-keys)
  (items line))
