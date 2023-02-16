(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-dolist-indentations
  (tagbody
     (next)
     ;; The current wad represents the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad represents the variable binding.
     (maybe-assign-indentation 4 4)
     ;; Do we need to check whether WAD is atomic?
     (compute-list-indentation
      current-wad client
      (lambda (wad client)
        (compute-form-indentation wad nil client)))
     (next)
   tag-or-form-or-declaration
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 4)
       (compute-declare-indentation current-wad client)
       (next)
       (go tag-or-form-or-declaration))
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
    ('#:common-lisp '#:dolist) compute-dolist-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:dotimes) compute-dolist-indentations)
