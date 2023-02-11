(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-prog2-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the first form to be indented a
     ;; bit more.
     (maybe-assign-indentation 4 4)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to be the second form to be indented a
     ;; bit more.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
   form
     ;; We should now be among the remaining forms.
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:prog2) compute-prog2-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:progv) compute-prog2-indentations)
