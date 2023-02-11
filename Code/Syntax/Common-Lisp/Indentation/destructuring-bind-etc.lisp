(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-destructuring-bind-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the lambda list.
     (maybe-assign-indentation 6 4)
     (compute-lambda-list-indentation current-wad client)
     (next)
     ;; The current wad ought to be the distinguished form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
   declaration-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-form))
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:destructuring-bind)
  compute-destructuring-bind-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:multiple-value-bind)
  compute-destructuring-bind-indentations)
