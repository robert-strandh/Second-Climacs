(cl:in-package #:second-climacs-syntax-common-lisp)

;;; The LINES slot contains a flexichain of lines, where each line
;;; is a vector of items.
(defclass flexichain-folio (folio)
  ((%lines :initform (make-instance 'flx:standard-flexichain)
           :initarg :lines
           :reader lines)))

(defmethod line-count ((folio flexichain-folio))
  (flx:nb-elements (lines folio)))

(defmethod line-length ((folio flexichain-folio) line-number)
  (length (flx:element* (lines folio) line-number)))

(defmethod item ((folio flexichain-folio) line-number item-number)
  (aref (flx:element* (lines folio) line-number) item-number))
