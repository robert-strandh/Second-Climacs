(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-situations (wad)
  (let ((children (children wad)))
    (unless (null children)
      (let ((column (start-column (first children))))
        (loop for child in (rest children)
              unless (zerop (start-line child))
                do (setf (indentation child) column))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:eval-when))) client)
  (let* ((fun #'indent-situations)
         (arguments (rest (children wad)))
         (indentation (+ (start-column wad) 4))
         (body-wads (compute-distinguished-indentation
                     arguments indentation fun)))
    (indent-body (+ (start-column wad) 2) body-wads client)))
