(cl:in-package #:climacs-syntax-common-lisp)

(defclass folio () ())

(defgeneric line-count (folio))

(defgeneric line-length (folio line-number))

(defgeneric line-items (folio line-number))

(defgeneric item (folio line-number column-number))
