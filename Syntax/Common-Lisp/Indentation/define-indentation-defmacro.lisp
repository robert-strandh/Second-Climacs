(cl:in-package #:climacs-syntax-common-lisp)

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
