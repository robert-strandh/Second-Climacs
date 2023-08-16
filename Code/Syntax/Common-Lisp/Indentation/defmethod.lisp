(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-defmethod-indentations
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

(define-form-indentation-method
    ('#:common-lisp '#:defmethod) compute-defmethod-indentations)
