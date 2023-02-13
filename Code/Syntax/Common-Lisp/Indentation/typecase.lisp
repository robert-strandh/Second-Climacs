(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-typecase-clause-indentations
  (tagbody
     (next)
     ;; The current wad should represent the type specifier.
     (maybe-assign-indentation 1 3)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
   form
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

;;; Compute the indentation of a single TYPECASE clause.
(defun compute-typecase-clause-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-typecase-clause-indentations))

(defun compute-typecase-clauses-indentation (wad client)
  (compute-list-indentation
   wad client #'compute-typecase-clause-indentation))

(define-indentation-automaton compute-typecase-indentations
  (tagbody
     (next)
     ;; The current wad represents the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad should represent the keyform or keyplace.
     ;; We indent it as a form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad should represent the list of clauses.
     (maybe-assign-indentation 2 2)
     (compute-typecase-clauses-indentation current-wad client)))

(define-form-indentation-method
    ('#:common-lisp '#:typecase) compute-typecase-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:ctypecase) compute-typecase-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:etypecase) compute-typecase-indentations)
