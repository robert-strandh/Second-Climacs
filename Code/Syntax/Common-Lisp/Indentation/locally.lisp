(cl:in-package #:second-climacs-syntax-common-lisp)

(defun indent-locally (wad client)
  (let ((arguments (rest (children wad))))
    (unless (null arguments)
      (let* ((argument (pop arguments))
             (declaration-indentation (start-column argument))
             (body-indentation (+ (start-column wad) 2)))
        (flet ((maybe-set-indentation (wad-to-indent column)
                 (unless (or (zerop (start-line wad-to-indent))
                             (eq wad-to-indent (second (children wad))))
                   (setf (indentation wad-to-indent) column))))
          (unless (zerop (start-line argument))
            (setf (indentation argument) (+ (start-column wad) 4)))
          (tagbody
             (if (typep argument 'expression-wad)
                 (go declaration-wads)
                 (go preceding-declarations))
           preceding-declarations
             ;; ARGUMENT is a NON-EXPRESSION-WAD preceding declaration.
             (maybe-set-indentation argument declaration-indentation)
             (if (null arguments)
                 (go out)
                 (progn (setf argument (pop arguments))
                        (if (typep argument 'expression-wad)
                            (go declaration-wads)
                            (go preceding-declarations))))
           declaration-wads
             (maybe-set-indentation argument declaration-indentation)
             (if (null arguments)
                 (go out)
                 (progn (setf argument (pop arguments))
                        (if (first-child-wad-represents-symbol-p
                             argument 'declare)
                            (go declaration-wads)
                            (go body-wads))))
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
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:locally))) client)
  (indent-locally wad client))
