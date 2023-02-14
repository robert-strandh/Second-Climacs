(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-check-type-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the place.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to represent the type specifier.
     (maybe-assign-indentation 2 2)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     ;; If we come here, it's because the optional string is present.
     (maybe-assign-indentation 2 2)))

(define-form-indentation-method
    ('#:common-lisp '#:check-type) compute-check-type-indentations)
