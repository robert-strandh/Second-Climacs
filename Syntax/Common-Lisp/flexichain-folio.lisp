(cl:in-package #:climacs-syntax-common-lisp)

(defclass flexichain-folio (folio)
  ((%contents :initarg :contents :reader contents)))

(defmethod line-count ((folio flexichain-folio))
  (flexichain:nb-elements (contents folio)))

(defmethod line-length ((folio flexichain-folio) line-number)
  (length (flexichain:element* (contents folio) line-number)))

(defmethod item ((folio flexichain-folio) line-number item-number)
  (aref (flexichain:element* (contents folio) line-number) item-number))

  
