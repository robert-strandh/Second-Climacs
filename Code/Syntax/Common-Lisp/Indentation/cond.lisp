(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-cond-clause-indentations
  (tagbody
     (next)
     ;; The current wad should represent the test form.
     (maybe-assign-indentation 1 3)
     (compute-form-indentation current-wad nil client)
     (next)
   form
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

;;; Compute the indentation of a single COND clause.
(defun compute-cond-clause-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-cond-clause-indentations))

(defun compute-cond-clauses-indentation (wad client)
  (compute-list-indentation
   wad client #'compute-cond-clause-indentation))

(define-indentation-automaton compute-cond-indentations
  (tagbody
     (next)
     ;; The current wad represents the operator.
     (maybe-assign-indentation 1 2)
     (next)
     ;; The current wad should represent the list of clauses.
     (maybe-assign-indentation 2 2)
     (compute-cond-clauses-indentation current-wad client)))

(define-form-indentation-method
    ('#:common-lisp '#:cond) compute-cond-indentations)
