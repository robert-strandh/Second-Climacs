(cl:in-package #:climacs-syntax-common-lisp)

(defclass token ()
  ((%characters :initarg :characters :reader characters)))

(defmethod print-object ((object token) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (characters object))))
