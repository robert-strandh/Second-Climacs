(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-assert-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 8)
     (next)
     ;; The current wad ought to represent the test form.
     (maybe-assign-indentation 8 6)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to represent the list of places.
     (maybe-assign-indentation 6 4)
     (compute-list-indentation
      current-wad client
      (lambda (wad client)
        (compute-form-indentation wad nil client)))
     (next)
     ;; The current wad ought to represent the datum form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:assert) compute-assert-indentations)
