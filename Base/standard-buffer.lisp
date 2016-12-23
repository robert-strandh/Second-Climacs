(cl:in-package #:second-climacs-base)

;;; A STANDARD-BUFFER is a buffer that is restricted to contain a
;;; Cluffer buffer.

(defclass standard-buffer (buffer)
  ((%cluffer-buffer :initarg :cluffer-buffer :reader cluffer-buffer)))
