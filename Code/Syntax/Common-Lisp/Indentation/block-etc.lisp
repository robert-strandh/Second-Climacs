(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-block-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the block name.  We don't do
     ;; anything in particular with it.
     (maybe-assign-indentation 4 2)
     (next)
   form
     ;; We should now be among the remaining forms.
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:block) compute-block-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:multiple-value-return-from) compute-block-indentations)
