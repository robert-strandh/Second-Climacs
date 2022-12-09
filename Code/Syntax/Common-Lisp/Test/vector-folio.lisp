(cl:in-package #:climacs-syntax-common-lisp-test)

;;; The CONTENTS slot contains a vector of lines, where each line is a
;;; vector of items.
(defclass vector-folio (cl-syntax:folio)
  ((%contents :initarg :contents :accessor contents)))

(defmethod cl-syntax:line-count
    ((folio vector-folio))
  (length (contents folio)))

(defmethod cl-syntax:line-length
    ((folio vector-folio) line-number)
  (length (aref (contents folio) line-number)))

(defmethod cl-syntax:item
    ((folio vector-folio) line-number item-number)
  (aref (aref (contents folio) line-number) item-number))
