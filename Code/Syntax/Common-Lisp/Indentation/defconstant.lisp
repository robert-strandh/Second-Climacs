(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-defconstant-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the name.
     (maybe-assign-indentation 4 2)
     (next)
     ;; The current wad ought to represent the initial value.
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     ;; If we come here, it's because the optional documentation is
     ;; present.
     (maybe-assign-indentation 2 2)))

(define-form-indentation-method
    ('#:common-lisp '#:defconstant) compute-defconstant-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:defvar) compute-defconstant-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:defparameter) compute-defconstant-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:define-symbol-macro) compute-defconstant-indentations)
