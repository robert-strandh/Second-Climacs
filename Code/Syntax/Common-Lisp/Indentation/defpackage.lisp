(cl:in-package #:second-climacs-syntax-common-lisp)

(defgeneric compute-defpackage-option-indentation (wad pawn client))

(defmethod compute-defpackage-option-indentation (wad pawn client)
  nil)

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a defpackage option.  It could be an
;;; atomic wad, in which case it should not have its indentation
;;; computed at all.  Or it could be a compound wad, but with an
;;; unknown defpackage-option name, in which case we also do not
;;; compute its indentation.
;;; FIXME: we need to access the pawn.
(defmethod compute-defpackage-option-indentation
    ((wad expression-wad) (pawn null) client)
  (when (simple-form-p wad)
    (let ((first-child (first (children wad))))
      (when (and (typep first-child 'expression-wad)
                 (typep (expression first-child) 'symbol-token))
        (let* ((token (expression first-child))
               (pawn (find-pawn (package-name token) (name token))))
          (unless (null pawn)
            (compute-defpackage-option-indentation wad pawn client)))))))

;;; This method is applicable when we are given either something that
;;; is not a pawn, or a pawn that has no method associated with it.
;;; So we do nothing.
(defmethod compute-defpackage-option-indentation (wad pawn client)
  nil)

(define-indentation-automaton compute-defpackage-indentations
  (tagbody
     (next)
     ;; The current wad is the operator.
     (maybe-assign-indentation 1 4)
     (next)
     ;; The current wad ought to be the class name.
     (maybe-assign-indentation 6 4)
     (next)
     ;; The current wad ought to be the list of superclasses.
     (maybe-assign-indentation 4 2)
     (compute-list-indentation current-wad client (constantly nil))
     (next)
     ;; The current wad ought to be the list of slot specifiers.
     (maybe-assign-indentation 2 4)
     (compute-slot-specifiers-indentation current-wad client)
     (next)
   class-option
     (maybe-assign-indentation 4 4)
     (compute-defpackage-option-indentation current-wad nil client)
     (next)
     (go class-option)))

(define-form-indentation-method
    ('#:common-lisp '#:defpackage)  compute-defpackage-indentations)

(define-indentation-automaton compute-shadow-indentations
  (tagbody
     (next)
     ;; The current wad is the option name.
     (maybe-assign-indentation 1 3)
     (next)
   symbol-name
     ;; The current wad ought to be a string designator.
     (maybe-assign-indentation 3 3)
     (next)
     (go symbol-name)))

(define-indentation-automaton compute-shadowing-import-from-indentations
  (tagbody
     (next)
     ;; The current wad is the option name.
     (maybe-assign-indentation 1 5)
     (next)
     ;; The current wad ought to be a package designator.
     (maybe-assign-indentation 5 3)
     (next)
   symbol-name
     ;; The current wad ought to be a string designator.
     (maybe-assign-indentation 3 3)
     (next)
     (go symbol-name)))

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-defpackage-option-indentation-method (pawn automaton)
  `(defmethod compute-defpackage-option-specifier-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))

(define-defpackage-option-indentation-method
    ('#:keyword '#:shadow) compute-shadow-indentations)

(define-defpackage-option-indentation-method
    ('#:keyword '#:shadowing-import-from)
  compute-shadowing-import-from-indentations)

(define-defpackage-option-indentation-method
    ('#:keyword '#:export) compute-shadow-indentations)

(define-defpackage-option-indentation-method
    ('#:keyword '#:intern) compute-shadow-indentations)
