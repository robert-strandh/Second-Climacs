(cl:in-package #:second-climacs-syntax-common-lisp)

(defparameter *indentation-computers*
  '((:bindings . compute-binding-indentations)
    (:form . comput-child-indentations)
    (:forms . compute-form-indentations)
    (:ordinary-body . comput-ordinary-body-indentations)
    (:function-body . comput-function-body-indentations)))

(defmacro define-indentation (operator lambda-list)
  (let ((package-name (if (symbolp operator)
                          (cl:package-name (cl:symbol-package operator))
                          (first operator)))
        (symbol-name (if (symbolp operator)
                          (cl:symbol-name operator)
                          (second operator))))
  `(defmethod compute-sub-form-indentations
       (wad (pawn (eql (intern-pawn ,package-name ,symbol-name))) client)
     (let ((arguments (split-wads (rest (children wad)))))
       (unless (null arguments)
         ,lambda-list)))))
