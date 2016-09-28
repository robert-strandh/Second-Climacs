(cl:in-package #:climacs-syntax-common-lisp-test)

;;; The CONTENTS slot contains a vector of lines, where each line is a
;;; vector of items.
(defclass vector-folio (climacs-syntax-common-lisp:folio)
  ((%contents :initarg :contents :accessor contents)))

(defmethod climacs-syntax-common-lisp:line-count
    ((folio vector-folio))
  (length (contents folio)))

(defmethod climacs-syntax-common-lisp:line-length
    ((folio vector-folio) line-number)
  (length (aref (contents folio) line-number)))

(defmethod climacs-syntax-common-lisp:item
    ((folio vector-folio) line-number item-number)
  (aref (aref (contents folio) line-number) item-number))
