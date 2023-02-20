(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-defun-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the name.
     (maybe-assign-indentation 6 4)
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

(define-form-indentation-method
    ('#:common-lisp '#:defun) compute-defun-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:defmacro) compute-defun-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:define-compiler-macro) compute-defun-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:deftype) compute-defun-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:define-setf-expander) compute-defun-indentations)
