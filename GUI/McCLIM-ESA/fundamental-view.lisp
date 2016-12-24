(cl:in-package #:climacs-esa-gui)

;;; A FUNDAMENTAL-VIEW contains a NULL-ANALYZER.

(defclass fundamental-view (climacs2-base:view)
  (;; This slot contains the timestamp corresponding to the
   ;; last time this view was updated from the Cluffer buffer.
   (%timestamp :initform nil :accessor timestamp)
   ;; This slot contains a flexichain of CONS cells.  The CAR of each
   ;; cell is a Cluffer line object.  The CDR is a vector containing
   ;; the contents of the Cluffer line object as it was when the
   ;; contents was asked for.
   (%lines :initform (make-instance 'flexichain:standard-flexichain))
   ;; This slot contains a flexichain-output-history object.
   (%history :initarg :history :reader history)))
