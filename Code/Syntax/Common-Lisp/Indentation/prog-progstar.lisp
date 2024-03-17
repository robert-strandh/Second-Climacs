(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-prog-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list of bindings.
     (maybe-assign-indentation 4 2)
     (compute-bindings-indentation current-wad client)
     (next)
   declaration-or-tag-or-statement
     (when (and (consp (cst:raw current-wad))
                (wad-represents-symbol-p
                 (first (ip:children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-tag-or-statement))
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
    ('#:common-lisp '#:prog) compute-prog-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:prog*) compute-prog-indentations)
