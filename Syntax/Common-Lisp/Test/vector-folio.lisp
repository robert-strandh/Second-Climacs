(cl:in-package #:climacs-syntax-common-lisp-test)

;;; The CONTENTS slot contains a vector of lines, where each line is a
;;; vector of items.
(defclass vector-folio (climacs-syntax-common-lisp:folio)
  ((%contents :initarg :contents :accessor contents)))

(defmethod line-count ((folio flexichain-folio))
  (length (contents folio)))

(defmethod line-length ((folio flexichain-folio) line-number)
  (length (faref (contents folio) line-number)))

(defmethod item ((folio flexichain-folio) line-number item-number)
  (aref (aref (contents folio) line-number) item-number))
