(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-the-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be a type specifier.
     (maybe-assign-indentation 4 2)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; There should be just one form, but we indent any extraneous
     ;; expressions as forms.
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:the) compute-the-indentations)
