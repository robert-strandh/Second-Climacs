(cl:in-package #:second-climacs-syntax-common-lisp)

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defun))) client)
  (let* ((arguments (rest (children wad)))
         (name-indentation (+ (start-column wad) 6))
         (lambda-list-indentation (+ (start-column wad) 4))
         (body-indentation (+ (start-column wad) 2))
         (remaining (compute-distinguished-indentation
                     arguments name-indentation #'identity))
         (body-wads (compute-distinguished-indentation
                     remaining
                     lambda-list-indentation
                     (lambda (w)
                       (indent-lambda-list w client)))))
    (indent-body body-indentation body-wads client)))
