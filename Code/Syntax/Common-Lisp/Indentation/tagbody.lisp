(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-tagbody-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
   tag-or-form
     (if (atom (cst:raw current-wad))
         ;; We have a label.
         (maybe-assign-indentation 2 4)
         ;; We have a form.
         (progn
           (maybe-assign-indentation 4 4)
           (compute-form-indentation current-wad nil client)))
     (next)
     (go tag-or-form)))

(define-form-indentation-method
    ('#:common-lisp '#:tagbody) compute-tagbody-indentations)
