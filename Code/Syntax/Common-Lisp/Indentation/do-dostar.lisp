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
      current-wad client
      (lambda (wad client)
        ;; Do we need to check whether WAD is atomic?
        (compute-list-indentation
         wad client
         (lambda (wad client)
           (compute-form-indentation wad nil client)))))
     (next)
     ;; The current wad represents the end test.
     (maybe-assign-indentation 4 4)
     ;; The end test is just a list of forms.
     (compute-list-indentation
      current-wad client
      (lambda (wad client)
        (compute-form-indentation wad nil client)))
     (next)
   tag-or-form-or-declaration
     (when (and (consp (cst:raw current-wad))
                (wad-represents-symbol-p
                 (first (ip:children current-wad))
                 'declare))
       (maybe-assign-indentation 3 4)
       (compute-declare-indentation current-wad client)
       (next)
       (go tag-or-form-or-declaration))
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
    ('#:common-lisp '#:do) compute-do-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:do*) compute-do-indentations)
