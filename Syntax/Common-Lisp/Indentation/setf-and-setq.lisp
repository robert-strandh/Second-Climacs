(cl:in-package #:climacs-syntax-common-lisp)

(defun compute-setf-and-setq-indentation (wad client)
  (let ((children (children wad)))
    (unless (null children)
      (destructuring-bind (first-child . remaining-children) (rest children)
        (unless (zerop (start-line first-child))
          (setf (indentation first-child) (+ (start-column wad) 2)))
        (loop with first-child-column = (start-column first-child)
              with additional-indentation = 2
              for child in remaining-children
              unless (zerop (start-line child))
                do (setf (indentation child)
                         (+ first-child-column additional-indentation))
              when (typep child 'expression-wad)
                do (setf additional-indentation
                         (- 2 additional-indentation))
                   (compute-child-indentations child client))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setf))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setq))) client)
  (compute-setf-and-setq-indentation wad client))
