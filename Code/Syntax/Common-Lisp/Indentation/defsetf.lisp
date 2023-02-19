(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-defsetf-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the name of the access function.
     (maybe-assign-indentation 6 4)
     (next)
     ;; If the current wad represents a CONS, then we have the long
     ;; form.  Otherwise we have the short form.
     (when (atom (expression current-wad))
       (go short-form))
     ;; The current wad ought to be the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
     ;; The current wad ought to be a list of store variables.
     (compute-list-indentation current-wad client (constantly nil))
     (next)
   declaration-or-documentation-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-documentation-or-form))
   documentation-or-form
     (when (stringp (expression current-wad))
       (maybe-assign-indentation 3 2)
       (next))
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)
   short-form
     ;; The current wad ought to be the update function.
     (maybe-assign-indentation 4 2)
     (next)
     ;; If we come here, it probably means the optional documentation
     ;; was given.
     (maybe-assign-indentation 2 2)))

(define-form-indentation-method
    ('#:common-lisp '#:defsetf) compute-defsetf-indentations)
