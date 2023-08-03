(cl:in-package #:second-climacs-syntax-common-lisp)

(defclass flexichain-folio (folio)
  ((%lines :initform (make-instance 'flx:standard-flexichain)
           :initarg :lines
           :reader lines)))
