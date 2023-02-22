(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton
    compute-print-unreadable-object-options-indentations
  (tagbody
     (next)
     ;; The current wad ought to be the object form.
     (maybe-assign-indentation 1 1)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to be the stream form.
     (maybe-assign-indentation 1 1)
     (compute-form-indentation current-wad nil client)
     (next)
   keyword-value-pair
     ;; The current wad ought to be a keyword form.
     (maybe-assign-indentation 1 3)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to be a value form.
     (maybe-assign-indentation 3 1)
     (compute-form-indentation current-wad nil client)
     (next)
     (go keyword-value-pair)))

(defun compute-print-unreadable-object-options-indentation (wad client)
  (compute-and-assign-indentations
   client wad compute-print-unreadable-object-options-indentations))

(define-indentation-automaton
    compute-print-unreadable-object-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list options.
     (maybe-assign-indentation 4 2)
     (compute-print-unreadable-object-options-indentation current-wad client)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:print-unreadable-object)
  compute-print-unreadable-object-indentations)
