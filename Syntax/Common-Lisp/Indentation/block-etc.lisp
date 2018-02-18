(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-block-etc (wad client)
  (let ((arguments (rest (children wad))))
    (unless (null arguments)
      (let* ((argument (pop arguments))
             (distinguished-indentation (start-column argument))
             (body-indentation (+ (start-column wad) 2)))
        (flet ((maybe-set-indentation (wad-to-indent column)
                 (unless (or (zerop (start-line wad-to-indent))
                             (eq wad-to-indent (second (children wad))))
                   (setf (indentation wad-to-indent) column))))
          (unless (zerop (start-line argument))
            (setf (indentation argument) (+ (start-column wad) 4)))
          (tagbody
             (if (typep argument 'expression-wad)
                 (go distinguished-wad)
                 (go preceding-distinguished))
           preceding-distinguished
             ;; ARGUMENT is a NON-EXPRESSION-WAD preceding distinguished.
             (maybe-set-indentation argument distinguished-indentation)
             (if (null arguments)
                 (go out)
                 (progn (setf argument (pop arguments))
                        (if (typep argument 'expression-wad)
                            (go distinguished-wad)
                            (go preceding-distinguished))))
           distinguished-wad
             (maybe-set-indentation argument distinguished-indentation)
             (if (null arguments)
                 (go out)
                 (progn (setf argument (pop arguments))
                        (go body-wads)))
           body-wads
             (maybe-set-indentation argument body-indentation)
             (when (typep argument 'expression-wad)
               (compute-child-indentations argument client))
             (if (null arguments)
                 (go out)
                 (progn (setf argument (pop arguments))
                        (go body-wads)))
           out))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:block))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:return-from))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:the))) client)
  (indent-block-etc wad client))