(cl:in-package #:second-climacs-base)

;;; For now, the kill ring is an adjustable vector used as a LIFO.  An
;;; element of the kill ring is a region of items.  Such a region is
;;; also represented as an adjustable vector where each element is a
;;; line of items.  Finally, each line is also an adjustable vector of
;;; buffer items.

(defun make-empty-adjustable-vector ()
  (make-array 0 :adjustable t :fill-pointer t))

(defparameter *kill-ring* (make-empty-adjustable-vector))

;;; Add a new empty element to the kill ring.
(defun add-kill-ring-element ()
  (vector-push-extend (make-empty-adjustable-vector) *kill-ring*))

;;; Find the last element of a vector with fill pointer, i.e., the
;;; element immediately preceding the fill pointer.
(defun last-element (vector)
  (aref vector (1- (fill-pointer vector))))

;;; Add a new empty line to the end of the last element of the kill
;;; ring.
(defun add-kill-ring-line ()
  (let ((element (last-element *kill-ring*)))
    (vector-push-extend (make-empty-adjustable-vector) element)))