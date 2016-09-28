(cl:in-package #:climacs-syntax-common-lisp)

(defgeneric next-position (folio line-number item-number))

(defmethod next-position ((folio folio) line-number item-number)
  (if (= (line-length folio line-number) item-number)
      (values (1+ line-number) 0)
      (values line-number (1+ column-number))))

(defgeneric previous-position (folio line-number item-number))

(defmethod previous-position ((folio folio) line-number item-number)
  (if (zerop item-number)
      (values 0 (line-length folio (1- line-number)))
      (values line-number (1- item-number))))
