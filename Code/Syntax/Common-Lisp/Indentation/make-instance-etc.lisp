(cl:in-package #:second-climacs-syntax-common-lisp)

;;; We indent MAKE-INSTANCE in a special way, because we can then
;;; often both save horizontal space and make the form more readable.
;;; The first argument to MAKE-INSTANCE (i.e. the CLASS form) is
;;; indented by 6 columns in case it is not on the same line as the
;;; MAKE-INSTANCE symbol.  Each keyword form is indented by 2
;;; positions compared to the entire form and each value form is
;;; indented by 4 positions.  That way, if there are many long keyword
;;; arguments, it is possible to put the keyword form on one line and
;;; the value form on the next one.  Here is an example:
;;;
;;; (make-instance <class>
;;;   :first-keyword
;;;     (cond ...
;;;           ...
;;;           ...)
;;;   :second-keyword
;;;     (case ...
;;;           ...))

(define-indentation-automaton compute-make-instance-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the form that computes the class
     ;; or the instance argument.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
   initarg
     ;; The current wad ought to be a form that computes an
     ;; initialization argument.
     (maybe-assign-indentation 2 4)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; The current wad ought to be a form that computes the value of
     ;; the initialization argument.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go initarg)))

(define-form-indentation-method
    ('#:common-lisp '#:make-instance)
  compute-make-instance-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:reinitialize-instance)
  compute-make-instance-indentations)
