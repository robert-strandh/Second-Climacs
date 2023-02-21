(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Compute the indentations for a single LET binding.
(define-indentation-automaton compute-handler-bind-binding-indentations
  (tagbody
     (next)
     ;; The current wad represents a type specifier.
     (maybe-assign-indentation 1 3)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     ;; The current wad represents a form.
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)))

;;; Compute the indentation of a single LET binding.
(defun compute-handler-bind-binding-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-handler-bind-binding-indentations))

;;; Compute the indentation of a list of LET bindings.
(defun compute-handler-bind-bindings-indentation (wad client)
  (compute-list-indentation
   wad client #'compute-handler-bind-binding-indentation))

(define-indentation-automaton compute-handler-bind-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list of bindings.
     (maybe-assign-indentation 4 2)
     (compute-handler-bind-bindings-indentation current-wad client)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:handler-bind) compute-handler-bind-indentations)
