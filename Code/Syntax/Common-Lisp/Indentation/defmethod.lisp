(cl:in-package #:second-climacs-syntax-common-lisp)

(defun compute-defmethod-indentations (indentation-units client)
  (let ((indentations (list 1))
        (current-unit '())
        (remaining-units indentation-units)
        (current-wad nil)
        (seen-expression-wad-p nil))
    (flet ((next ()
             (setf current-wad nil)
             (loop until (typep current-wad 'expression-wad)
                   do (when (null current-unit)
                        (setf seen-expression-wad-p nil)
                        (if (null remaining-units)
                            (return-from compute-defmethod-indentations
                              (reverse indentations))
                            (setf current-unit
                                  (pop remaining-units))))
                      (setf current-wad (pop current-unit))))
           (maybe-assign-indentation (indentation next-default)
             (unless seen-expression-wad-p
               (setf seen-expression-wad-p t)
               (setf (first indentations) indentation)
               (push next-default indentations))))
      (tagbody
         (next)
         ;; The current wad must be the symbol DEFMETHOD, or else we
         ;; wouldn't be here.
         (maybe-assign-indentation 1 6)
         (next)
         ;; If it exists, the current wad represents the function
         ;; name.  It is not the purpose of the indentation code to
         ;; check the validity of that name, so we just skip it.
         (maybe-assign-indentation 6 4)
         (next)
         ;; The current wad may represent a method qualifier or the
         ;; lambda list.
       method-qualifier-or-lambda-list
         (unless (or (consp (expression current-wad))
                     (wad-represents-symbol-p current-wad nil))
           (maybe-assign-indentation 4 4)
           (next)
           (go method-qualifier-or-lambda-list))
         ;; The current wad is the lambda list.
         (maybe-assign-indentation 4 2)
         (indent-lambda-list current-wad client)
         (next)
       declaration-or-documentation-or-form
         (when (and (consp (expression current-wad))
                    (wad-represents-symbol-p
                     (first (children current-wad))
                     'declare))
           (maybe-assign-indentation 3 2)
           (next)
           (go declaration-or-documentation-or-form))
       documentation-or-form
         (when (stringp (expression current-wad))
           (maybe-assign-indentation 3 2)
           (next))
       form 
         (maybe-assign-indentation 2 2)
         (next)
         (compute-child-indentations current-wad client)
         (go form)))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defmethod))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-defmethod-indentations indentation-units client)))
    (loop for indentation-unit in indentation-units
          for indentation in indentations
          do (assign-indentation-of-wads-in-unit
              indentation-unit indentation))))
