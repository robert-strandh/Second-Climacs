(cl:in-package #:second-climacs-syntax-common-lisp)

;;; The CONTENTS slot contains a flexichain of lines, where each line
;;; is a vector of items.
(defclass flexichain-folio (folio)
  ((%contents :initform (make-instance 'flx:standard-flexichain)
              :initarg :contents
              :reader contents)))

(defmethod line-count ((folio flexichain-folio))
  (flx:nb-elements (contents folio)))

(defmethod line-length ((folio flexichain-folio) line-number)
  (length (flx:element* (contents folio) line-number)))

(defmethod line-contents ((folio flexichain-folio) line-number)
  (flx:element* (contents folio) line-number))

(defmethod item ((folio flexichain-folio) line-number item-number)
  (aref (flx:element* (contents folio) line-number) item-number))
