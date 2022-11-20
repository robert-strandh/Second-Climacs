(cl:in-package #:second-climacs-base)

;;; For now, the kill ring is an adjustable vector used as a LIFO.  An
;;; element of the kill ring is a region of items.  Such a region is
;;; also represented as an adjustable vector where each element is a
;;; line of items.  Finally, each line is also an adjustable vector of
;;; buffer items.

(defparameter *kill-ring* (make-array 0 :adjustable t :fill-pointer t))

;;; Add a new empty element to the kill ring.
(defun add-kill-ring-element ()
  (vector-push-extend (make-array 0 :adjustable t :fill-pointer t)
                      *kill-ring*))
