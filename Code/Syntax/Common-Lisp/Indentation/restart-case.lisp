(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Compute the indentations for a single LET binding.
(define-indentation-automaton compute-restart-case-clause-indentations
  (tagbody
     (next)
     ;; The current wad represents the case name.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad represent the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
   option-or-declaration-or-form
     (let ((expression (cst:raw current-wad)))
       (when (member expression '(:interactive :report :test) :test #'eq)
         ;; We have an option.
         (maybe-assign-indentation 2 4)
         (next)
         ;; The current wad represents the expression associated with
         ;; the option.  But we can compute the indentation as if it
         ;; is a form, because it is either an atom or a lambda
         ;; expression.
         (maybe-assign-indentation 4 2)
         (compute-form-indentation current-wad nil client)
         (next)
         (go option-or-declaration-or-form)))
   declaration-or-form
     (when (and (consp (cst:raw current-wad))
                (wad-represents-symbol-p
                 (first (ip:children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-form))
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

;;; Compute the indentation of a single LET binding.
(defun compute-restart-case-clause-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-restart-case-clause-indentations))

(define-indentation-automaton compute-restart-case-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the restartable form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
   clause
     (maybe-assign-indentation 2 2)
     (compute-restart-case-clause-indentation current-wad client)
     (next)
     (go clause)))

(define-form-indentation-method
    ('#:common-lisp '#:restart-case) compute-restart-case-indentations)
