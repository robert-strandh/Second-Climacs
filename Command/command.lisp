(cl:in-package #:climacs-command)

(defclass command (standard-generic-function)
  ((%lambda-list :initarg :lambda-list :reader lambda-list))
  (:metaclass #.(class-name (class-of (find-class 'standard-generic-function)))))
