(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-eval-when-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list of situations.  We
     ;; think it is unlikely that situations will be separated by
     ;; newlines, so we do nothing special for this list.
     (maybe-assign-indentation 4 2)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:eval-when) compute-eval-when-indentations)
