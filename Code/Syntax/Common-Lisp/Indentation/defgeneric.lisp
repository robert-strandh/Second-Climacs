(cl:in-package #:second-climacs-syntax-common-lisp)

(defgeneric compute-defgeneric-option-indentation (wad pawn client))

(defmethod compute-defgeneric-option-indentation (wad pawn client)
  nil)

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a defgeneric option.  It could be an
;;; atomic wad, in which case it should not have its indentation
;;; computed at all.  Or it could be a compound wad, but with an
;;; unknown defgeneric-option name, in which case we also do not
;;; compute its indentation.
(defmethod compute-defgeneric-option-indentation (wad (pawn null) client)
  (when (typep wad 'ip:cst-wad)
    (let ((expression (cst:raw wad)))
      (when (and (consp expression)
                 (not (null (first expression))))
        (compute-defgeneric-option-indentation
         wad (first expression) client)))))

;;; This method is applicable when we are given a pawn, but there is
;;; no more specific method applicable, meaning we have not defined a
;;; method for this particular pawn.  So we do nothing.
(defmethod compute-defgeneric-option-indentation (wad (pawn pawn) client)
  nil)

(define-indentation-automaton compute-defgeneric-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the function name.
     (maybe-assign-indentation 6 4)
     (next)
     ;; The current wad ought to be the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
   option-or-method-description
     (next)
     (maybe-assign-indentation 2 2)
     (compute-defgeneric-option-indentation current-wad nil client)
     (go option-or-method-description)))

(define-form-indentation-method
    ('#:common-lisp '#:defgeneric) compute-defgeneric-indentations)

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-defgeneric-option-indentation-method (pawn automaton)
  `(defmethod compute-defgeneric-option-specifier-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))

(define-indentation-automaton compute-method-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol :METHOD, or else we
     ;; wouldn't be here.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad may represent a method qualifier or the
     ;; lambda list.
   method-qualifier-or-lambda-list
     (unless (or (consp (ip:expression current-wad))
                 (wad-represents-symbol-p current-wad nil))
       (maybe-assign-indentation 4 4)
       (next)
       (go method-qualifier-or-lambda-list))
     ;; The current wad is the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
   declaration-or-documentation-or-form
     (when (and (consp (ip:expression current-wad))
                (wad-represents-symbol-p
                 (first (ip:children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-documentation-or-form))
   documentation-or-form
     (when (stringp (ip:expression current-wad))
       (maybe-assign-indentation 3 2)
       (next))
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))


(define-defgeneric-option-indentation-method
    ('#:keyword '#:method) compute-method-indentations)
