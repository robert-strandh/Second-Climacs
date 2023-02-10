(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Compute the indentations for a single LET binding.
(define-indentation-automaton compute-binding-indentations
  (tagbody
     (next)
     ;; The current wad represents the variable introduced by the
     ;; binding.  But it is not the role of the indentation code
     ;; to verify the nature of this wad.  Here, we just assume
     ;; that there are no sub-indentation to be computed in this
     ;; wad.
     (maybe-assign-indentation 3 1)
     (next)
     ;; Come here when the current wad represents the initialization
     ;; form for the variable being bound.  This expression should be
     ;; a form, so we compute the indentation that way.
     (maybe-assign-indentation 1 3)
     (compute-form-indentation current-wad nil client)
     (next)
   extraneous-stuff
     ;; We shouldn't really be here, because if we are, there was
     ;; more than one child expression wad of the binding, so the
     ;; binding is malformed.  So it doesn't really matter what we
     ;; do.
     (maybe-assign-indentation 1 1)
     (next)
     (go extraneous-stuff)))

(defun compute-binding-indentation (wad client)
  (compute-and-assign-indentations client wad compute-binding-indentations))

;;; Compute the indentations for a list of LET bindings. 
(define-indentation-automaton compute-bindings-indentations
  (tagbody
     (next)
   binding
     (maybe-assign-indentation 1 1)
     (compute-binding-indentation current-wad client)
     (next)
     (go binding)))

(defun compute-bindings-indentation (wad client)
  (compute-and-assign-indentations client wad compute-bindings-indentations))

(define-indentation-automaton compute-let-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list of bindings.
     (maybe-assign-indentation 4 2)
     (compute-bindings-indentation current-wad client)
     (next)
   declaration-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-form))
   form
     (maybe-assign-indentation 2 2)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:let) compute-let-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:let*) compute-let-indentations)
