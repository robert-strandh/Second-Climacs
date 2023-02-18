(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-define-modify-macro-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the name.
     (maybe-assign-indentation 6 4)
     (next)
     ;; The current wad ought to be the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
     ;; The current wad ought to be the function symbol.
     (maybe-assign-indentation 2 2)
     (next)
     ;; Come here if the optional documentation is given
     (maybe-assign-indentation 2 2)))

(define-form-indentation-method
    ('#:common-lisp '#:define-modify-macro)
  compute-define-modify-macro-indentations)
