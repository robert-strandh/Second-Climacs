(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton
    compute-with-input-from-string-arguments-indentations
  (tagbody
     (next)
     ;; The current wad ought to be the variable name.
     (maybe-assign-indentation 1 1)
     (next)
     ;; The current wad ought to be a form.
     (maybe-assign-indentation 1 1)
     (compute-form-indentation current-wad nil client)
     (next)
   keyword-argument
     (maybe-assign-indentation 1 3)
     (compute-form-indentation current-wad nil client)
     (next)
     (maybe-assign-indentation 3 1)
     (compute-form-indentation current-wad nil client)
     (next)
     (go keyword-argument)))

(defun compute-with-input-from-string-arguments-indentation
    (wad client)
  (compute-and-assign-indentations
   client wad compute-with-input-from-string-arguments-indentations))

(define-indentation-automaton compute-with-input-from-string-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the distinguished arguments.
     (maybe-assign-indentation 4 2)
     (compute-with-input-from-string-arguments-indentation current-wad client)
     (next)
   declaration-or-form
     (when (and (consp (ip:expression current-wad))
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

(define-form-indentation-method
    ('#:common-lisp '#:with-input-from-tring)
  compute-with-input-from-string-indentations)

;;; WITH-OUTPUT-TO-STRING can probably be indented the same way as
;;; WITH-INPUT-FROM-STRING.
(define-form-indentation-method
    ('#:common-lisp '#:with-output-to-tring)
  compute-with-input-from-string-indentations)
