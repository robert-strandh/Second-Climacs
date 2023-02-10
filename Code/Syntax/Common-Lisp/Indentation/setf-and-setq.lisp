(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-setq-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 2)
     (next)
   place
     ;; We indent a place as a form, because in general, it can take
     ;; the shape of a form.
     (maybe-assign-indentation 2 4)
     (compute-form-indentation current-wad nil client)
     (next)
   form
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go place)))

(define-form-indentation-method
    ('#:common-lisp '#:setq) compute-setq-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:setf) compute-setq-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:psetq) compute-setq-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:psetf) compute-setq-indentations)
