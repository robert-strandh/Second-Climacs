(cl:in-package #:second-climacs-syntax-common-lisp)

(defun defmethod-determine-role (wads)
  (let ((remaining wads))
    (flet ((skip-no-expression-wads ()
             (loop until (null remaining)
                   while (typep (first remaining)
                                'no-expression-wad)
                   do (pop remaining))))
      (tagbody
         (skip-no-expression-wads)
         ;; The first expression wad is the symbol DEFMETHOD, or else
         ;; we wouldn't be here.
         (pop remaining)
         (skip-no-expression-wads)
         ;; If it exists, the next wad represents the function name.
         ;; It is not the purpose of the indentation code to check the
         ;; validity of that name, so we just skip it.
         (when (null remaining)
           (return-from defmethod-determine-role
             :generic-function-name))
         (pop remaining)
         (skip-no-expression-wads)
         ;; The next wad may represent a method qualifier or the
         ;; lambda list.
       method-qualifier-or-lambda-list
         (when (null remaining)
           ;; We don't know what it is, so we let the caller decide.
           (return-from defmethod-determine-role
             :method-qualifier-or-lambda-list))
         (let* ((wad (pop remaining)))
           ;; If this wad represents a list, i.e., it is either a CONS
           ;; or the symbol NIL, then it is the lambda list.  If not,
           ;; it is a method qualifier.
           (unless (or (consp (expression wad))
                       (wad-represents-symbol-p wad nil))
             (skip-no-expression-wads)
             (go method-qualifier-or-lambda-list)))
         ;; Come here when we have seen a lambda list.  For now, we
         ;; don't check for declarations and documentation as
         ;; distinguished from body forms, so if we have seen the
         ;; lambda list, we are in the body.
         (return-from defmethod-determine-role
           :body)))))

(defun assign-indentation-of-wads-in-unit
    (indentation-unit indentation)
  ;; Always assign indentation of the first wad in the unit.
  (setf (indentation (first indentation-unit)) indentation)
  (loop for (wad1 wad2) on indentation-unit
        until (null wad2)
        when (wads-are-on-different-lines-p wad1 wad2)
          do (setf (indentation wad2) indentation)))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:defmethod))) client)
  (let ((indentation-units (compute-indentation-units (children wad))))
    ;; This is horribly inefficient.  I am looking for a better
    ;; technique.
    (loop with length = (length indentation-units)
          for indentation-unit in (rest indentation-units)
          for i from 1 below length
          for prefix = (subseq indentation-units 0 i)
          for wads = (apply #'append prefix)
          for role = (defmethod-determine-role wads)
          for indentation
            = (ecase role
                (:generic-function-name 6)
                (:method-qualifier-or-lambda-list 4)
                (:body 2))
          do (assign-indentation-of-wads-in-unit
              indentation-unit indentation))))
