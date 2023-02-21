(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Compute the indentations for a single LET binding.
(define-indentation-automaton compute-handler-case-clause-indentations
  (tagbody
     (next)
     ;; The current wad represents a type specifier (if this clause is
     ;; an error clause) or the symbol :NO-ERROR if it is a no-error
     ;; clauase.  But we can indent :NO-ERROR as if it is a type
     ;; specifier, so we avoid a special case here.
     (maybe-assign-indentation 1 5)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     ;;; The current wad ought to represent something that looks like
     ;;; a lambda list, so we indent it as such.
     (maybe-assign-indentation 5 3)
     (compute-lambda-list-indentation current-wad client)
     (next)
   declaration-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 4 3)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-form))
   form
     (maybe-assign-indentation 3 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

;;; Compute the indentation of a single LET binding.
(defun compute-handler-case-clause-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-handler-case-clause-indentations))

(define-indentation-automaton compute-handler-case-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the expression.
     (maybe-assign-indentation 4 2)
     (next)
   clause
     (maybe-assign-indentation 2 2)
     (compute-handler-case-clause-indentation current-wad client)
     (next)
     (go clause)))

(define-form-indentation-method
    ('#:common-lisp '#:handler-case) compute-handler-case-indentations)
