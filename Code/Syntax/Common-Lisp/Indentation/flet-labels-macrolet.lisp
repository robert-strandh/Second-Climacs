(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-local-function-indentations
  (tagbody 
     (next)
     ;; The current wad ought to be the name.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the lambda list.
     (maybe-assign-indentation 4 2)
     (compute-lambda-list-indentation current-wad client)
     (next)
   declaration-or-documentation-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-documentation-or-form))
   documentation-or-form
     (when (stringp (expression current-wad))
       (maybe-assign-indentation 3 2)
       (next))
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(defun compute-local-function-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-local-function-indentations))

(define-indentation-automaton compute-local-functions-indentations
  (tagbody
     (next)
   local-function
     (maybe-assign-indentation 1 1)
     (compute-local-function-indentation current-wad client)
     (next)
     (go local-function)))

(defun compute-local-functions-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-local-functions-indentations))

(define-indentation-automaton compute-flet-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the list of local function
     ;; definitions.
     (maybe-assign-indentation 4 2)
     (compute-local-functions-indentation current-wad client)
     (next)
   declaration-or-documentation-or-form
     (when (and (consp (expression current-wad))
                (wad-represents-symbol-p
                 (first (children current-wad))
                 'declare))
       (maybe-assign-indentation 3 2)
       (compute-declare-indentation current-wad client)
       (next)
       (go declaration-or-documentation-or-form))
   documentation-or-form
     (when (stringp (expression current-wad))
       (maybe-assign-indentation 3 2)
       (next))
   form
     (maybe-assign-indentation 2 2)
     (compute-child-indentations current-wad client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:flet) compute-flet-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:labels) compute-flet-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:macrolet) compute-flet-indentations)
