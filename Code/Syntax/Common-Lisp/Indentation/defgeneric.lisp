(cl:in-package #:second-climacs-syntax-common-lisp)

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
     (go option-or-method-description)))

(define-form-indentation-method
    ('#:common-lisp '#:defgeneric) compute-defgeneric-indentations)
