(cl:in-package #:second-climacs-syntax-common-lisp)

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a form.  It could be an atomic wad, in
;;; which case it should not have its indentation computed at all.  Or
;;; it could be a compound wad, but with an unknown operator, in which
;;; case we compute the indentation as if the form is a function call.
(defmethod compute-form-indentation ((wad expression-wad) (pawn null) client)
  (if (simple-form-p wad)
      (let ((first-child (first (children wad))))
        (if (and (typep first-child 'expression-wad)
                 (typep (expression first-child) 'ip:symbol-token))
            (let* ((token (expression first-child))
                   (pawn (find-pawn (package-name token)
                                    (ip:name token))))
              (if (null pawn)
                  (indent-default-function-call wad client)
                  (compute-form-indentation wad pawn client)))
            (indent-default-function-call wad client)))
      (indent-default-function-call wad client)))

;;; This method is applicable when we are given either something that
;;; is not a pawn, or a pawn that has no method associated with it. So
;;; we indent the form as a function call.
(defmethod compute-form-indentation (wad pawn client)
  (indent-default-function-call wad client))

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-form-indentation-method (pawn automaton)
  `(defmethod compute-form-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))
