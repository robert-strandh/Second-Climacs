(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-case-clause-indentations
  (tagbody
     (next)
     ;; The current wad should represent the keys or OTHERWISE or T
     ;; for an OTHERWISE clause.
     (maybe-assign-indentation 1 3)
     ;; We indent it as a list of arbitrary objects.
     (compute-list-indentation current-wad client (constantly nil))
     (next)
   form
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

;;; Compute the indentation of a single CASE clause.
(defun compute-case-clause-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-case-clause-indentations))

(defun compute-case-clauses-indentation (wad client)
  (compute-list-indentation
   wad client #'compute-case-clause-indentation))

(define-indentation-automaton compute-case-indentations
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
     (compute-case-clauses-indentation current-wad client)))

(define-form-indentation-method
    ('#:common-lisp '#:case) compute-case-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:ccase) compute-case-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:ecase) compute-case-indentations)
