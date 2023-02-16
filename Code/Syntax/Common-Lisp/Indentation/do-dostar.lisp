(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-do-indentations
  (tagbody
     (next)
     ;; The current wad represents the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad represents the list of variable bindings.
     (maybe-assign-indentation 4 4)
     (compute-list-indentation
      current-wad client #'compute-do-binding-indentation)
     (next)
     ;; The current wad represents the end test.
     (maybe-assign-indentation 4 4)
     ;; The end test is just a list of forms.
     (compute-list-indentation
      current-wad client
      (lambda (wad client)
        (compute-form-indentation wad nil client)))
     (next)
   tag-or-form
     (if (atom (expression current-wad))
         ;; We have a label.
         (maybe-assign-indentation 2 4)
         ;; We have a form.
         (progn
           (maybe-assign-indentation 4 4)
           (compute-form-indentation current-wad nil client)))
     (next)
     (go tag-or-form)))

(define-form-indentation-method
    ('#:common-lisp '#:do) compute-do-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:do*) compute-do-indentations)
