(cl:in-package #:second-climacs-syntax-common-lisp)

(defun indent-situations (wad)
  (let ((children (children wad)))
    (unless (null children)
      (let ((column (start-column (first children))))
        (loop for child in (rest children)
              unless (zerop (start-line child))
                do (setf (indentation child) column))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:eval-when))) client)
  (compute-indentation-single-distinguished
   wad
   #'indent-situations
   (lambda (indentation wads)
     (indent-body indentation wads client))))
