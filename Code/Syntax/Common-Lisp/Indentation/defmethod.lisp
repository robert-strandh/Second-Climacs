(cl:in-package #:second-climacs-syntax-common-lisp)

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defmethod))) client)
  (let ((indentation-units (compute-indentation-units (children wad))))
    (declare (ignore indentation-units))))
