(cl:in-package #:second-climacs-syntax-common-lisp)

(define-indentation-automaton compute-with-slots-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 6)
     (next)
     ;; The current wad ought to be the list of entries.  We don't
     ;; think it is likely that an entry is going to be split across
     ;; lines, so we don't indent an entry in any particular way.
     (maybe-assign-indentation 6 4)
     (compute-list-indentation current-wad client (constantly nil))
     (next)
     ;; The current wad ought to be the instance form.
     (maybe-assign-indentation 4 2)
     (compute-form-indentation current-wad nil client)
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
     (compute-form-indentation current-wad nil client)
     (next)
     (go form)))

(define-form-indentation-method
    ('#:common-lisp '#:with-slots) compute-with-slots-indentations)

(define-form-indentation-method
    ('#:common-lisp '#:with-accessors) compute-with-slots-indentations)
