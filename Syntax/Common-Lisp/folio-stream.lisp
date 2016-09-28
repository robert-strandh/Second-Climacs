(cl:in-package #:climacs-syntax-common-lisp)

;;; A folio stream is a stream that uses a folio as a source for the
;;; items to return as a result of reading.
(defclass folio-stream ()
  ((%folio :initarg :folio :reader folio)
   (%current-line-number :initform 0 :accessor current-line-number)
   (%current-item-number :initform 0 :accessor current-item-number)))

(defgeneric next-position (folio line-number item-number))

(defmethod next-position ((folio folio) line-number item-number)
  (if (= (line-length folio line-number) item-number)
      (values (1+ line-number) 0)
      (values line-number (1+ item-number))))

(defgeneric previous-position (folio line-number item-number))

(defmethod previous-position ((folio folio) line-number item-number)
  (if (zerop item-number)
      (values 0 (line-length folio (1- line-number)))
      (values line-number (1- item-number))))
