(cl:in-package #:second-climacs-syntax-common-lisp)

(defun wad-represents-lambda-list-keyword-p (wad)
  (loop for keyword in lambda-list-keywords
        thereis (wad-represents-symbol-p wad keyword)))

(defun compute-lambda-list-indentations (indentation-units client)
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
                            (return-from compute-lambda-list-indentations
                              (reverse indentations))
                            (setf current-unit
                                  (pop remaining-units))))
                      (setf current-wad (pop current-unit))))
           (maybe-assign-indentation (indentation next-default)
             (unless seen-expression-wad-p
               (setf seen-expression-wad-p t)
               (setf (first indentations) indentation)
               (push next-default indentations))))
      (macrolet ((go-accordingly (lambda-list-keyword)
                   `(when (wad-represents-symbol-p
                           current-wad ',lambda-list-keyword)
                      (next)
                      (if (wad-represents-lambda-list-keyword-p
                           current-wad)
                          (go lambda-list-keyword)
                          (go ,lambda-list-keyword)))))
        (tagbody
           (next)
         could-be-anything
           (maybe-assign-indentation 1 1)
           ;; As long as the current wad does not represent a
           ;; lambda-list keywod symbol, we consider it to be a
           ;; required parameter.  Of course, that means that a
           ;; required parameter can be just about any expression, but
           ;; it is not the role of the indentation logic to detect
           ;; such issues.
           (unless (wad-represents-lambda-list-keyword-p current-wad)
             (next)
             (go could-be-anything))
         lambda-list-keyword
           ;; Come here when we have a lambda-list keyword.  The
           ;; indentation code should not care too much about the
           ;; order between the lambda-list keywords.  But it should
           ;; recognize the kind of keyword so that it can indent the
           ;; keyword parameter accordingly.  So what we do is that we
           ;; recognize that we are in a particular group of
           ;; parameters according to the lambda-list keyword that
           ;; precedes it, but we do not check the order nor the
           ;; number of such groups.
           (maybe-assign-indentation 3 1)
           (go-accordingly &optional)
           (go-accordingly &rest)
           (go-accordingly &body)
           (go-accordingly &whole)
           (go-accordingly &environment)
           (go-accordingly &key)
           (go-accordingly &allow-other-keys)
           (go-accordingly &aux)
         unknown
           ;; Come here when we have an unknown lambda-list keyword.
           ;; We do not process the possible parameters that might
           ;; follow, because we don't know their nature.  But we
           ;; indent them as members of the group.
           (if (wad-represents-lambda-list-keyword-p current-wad)
               (go lambda-list-keyword)
               (progn (maybe-assign-indentation 3 3)
                      (next)
                      (go unknown)))
         &optional
           ;; Come here when the current wad ought to represent an
           ;; optional parameter.  We try to determine whether we have
           ;; an optional form to indent.
           (when (wad-represents-lambda-list-keyword-p current-wad)
             (go lambda-list-keyword))
           (maybe-assign-indentation 3 3)
           (when (typep current-wad 'expression-wad)
             (let* ((expression (expression current-wad))
                    (children (children current-wad)))
               (when (consp expression)
                 ;; We try to find an expression wad that
                 ;; follows the first child
                 (let ((init-form-wad
                         (find-if (lambda (x)
                                    (typep x 'expression-wad))
                                  (rest children))))
                   (unless (null init-form-wad)
                     (compute-child-indentations
                      init-form-wad client))))))
           (next)
           (go &optional)
         &rest
         &body
         &whole
         &environment
         &key
         &allow-other-keys
         &aux)))))

(defgeneric indent-lambda-list (wad client))

;;; For now, we don't do anything sophisticated about lambda lists.  
(defmethod indent-lambda-list (wad client)
  (indent-simple-list wad))
