(cl:in-package #:second-climacs-syntax-common-lisp)

(defun compute-indentation-destruturing-bind-etc (wad client)
  (let* ((arguments (rest (children wad)))
         (lambda-list-indentation (+ (start-column wad) 6))
         (values-form-indentation (+ (start-column wad) 4))
         (body-indentation (+ (start-column wad) 2))
         (remaining (compute-distinguished-indentation
                     arguments
                     lambda-list-indentation
                     (lambda (w)
                       (indent-lambda-list w client))))
         (body-wads (compute-distinguished-indentation
                     remaining
                     values-form-indentation
                     (lambda (w)
                       (compute-child-indentations w client)))))
    (indent-body body-indentation body-wads client)))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:destructuring-bind))) client)
  (compute-indentation-destruturing-bind-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-bind))) client)
  (compute-indentation-destruturing-bind-etc wad client))
