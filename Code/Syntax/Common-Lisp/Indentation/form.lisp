(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the form.  Instead, we compute the
;;; indentation of the sub-expressions of that expression.
(defgeneric compute-form-indentation (wad pawn client))

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
                 (typep (expression first-child) 'symbol-token))
            (let* ((token (expression first-child))
                   (pawn (find-pawn (package-name token)
                                    (name token))))
              (if (null pawn)
                  (indent-default-function-call wad client)
                  (compute-form-indentation wad pawn client)))
            (indent-default-function-call wad client)))
      (indent-default-function-call wad client)))

(define-indentation-automaton compute-function-call-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 3)
     ;; We indent the operator as if it were a form.  That way, we can
     ;; capture the case where the operator is a lambda expression.
     (compute-form-indentation current-wad nil client)
     (next)
   argument
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go argument)))

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
