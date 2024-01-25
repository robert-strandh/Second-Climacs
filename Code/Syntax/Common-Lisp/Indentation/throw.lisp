(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-throw-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the tag which is a form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to be the result form.
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)))

(define-form-indentation-method
    ('#:common-lisp '#:catch) compute-throw-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:throw) compute-throw-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:return-from) compute-throw-indentations)
