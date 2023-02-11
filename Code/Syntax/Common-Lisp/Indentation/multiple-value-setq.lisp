(cl:in-package #:second-climacs-syntax-common-lisp)

;;; MULTIPLE-VALUE-SETQ does not have a body, only a single form.  But
;;; we must do something reasonable even if the programmer got the
;;; syntax wrong, so we indent as if several forms were allowed.

(define-indentation-automaton compute-multiple-value-setq-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to represent the list of variables.  We
     ;; think it is unlikely that the variables will be separated by
     ;; newlines, so we do nothing special for this list.
     (maybe-assign-indentation 4 2)
     (next)
   form
     (maybe-assign-indentation 2 2)
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:multiple-value-setq)
  compute-multiple-value-setq-indentations)
