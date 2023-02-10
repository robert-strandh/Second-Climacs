(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-prog1-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the first form to be indented a
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
    ('#:common-lisp '#:prog1) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:multiple-value-prog1) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:multiple-value-call) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:catch) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:throw) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:unwind-protect) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:when) compute-prog1-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:unless) compute-prog1-indentations)
