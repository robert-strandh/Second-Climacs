(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the type specifier.  Instead, we
;;; compute the indentation of the sub-expressions of that expression.
(defgeneric compute-type-specifier-indentation (wad pawn client))

;;; This method is applicable when the caller specifies NIL for the
;;; pawn, meaning that we do not know the nature of the wad att all,
;;; only that it ought to be a type specifier.  It could be an atomic
;;; wad, in which case it should not have its indentation computed at
;;; all.  Or it could be a compound wad, but with an unknown type
;;; identifier, in which case we also do not compute its indentation.
(defmethod compute-type-specifier-indentation (wad (pawn null) client)
  (when (typep wad 'ip:expression-wad)
    (let ((expression (ip:expression wad)))
      (when (consp expression)
        (compute-type-specifier-indentation
         wad (first expression) client)))))

;;; This method is applicable when we are given a pawn, but there is
;;; no more specific method applicable, meaning we have not defined a
;;; method for this particular pawn.  So we do nothing.
(defmethod compute-type-specifier-indentation (wad (pawn pawn) client)
  nil)

;;; This method is applicable when we are not given a pawn. So we do
;;; nothing.
(defmethod compute-type-specifier-indentation (wad pawn client)
  nil)

;;; This macro is used to define a typical indentation method that
;;; computes indentation units and calls an automaton function.
(defmacro define-type-specifier-indentation-method (pawn automaton)
  `(defmethod compute-type-specifier-indentation
       (wad (pawn (eql (intern-pawn ,@pawn))) client)
     (compute-and-assign-indentations client wad ,automaton)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type specifiers AND, OR, NOT, CONS.
;;;
;;; While some of these type specifiers take a bounded number of
;;; arguments, we still treat them as if they can take an arbitrary
;;; number, because it is not the purpose of the indentation code to
;;; detect syntax violations like this.

(define-indentation-automaton compute-type-and-etc-indentations
  (tagbody
     (next)
     ;; The current wad must be the type identifier.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The remaining wads represent type specifiers.
   type-specifier
     (maybe-assign-indentation 3 3)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     (go type-specifier)))

(define-type-specifier-indentation-method
    ('#:common-lisp '#:and) compute-type-and-etc-indentations)

(define-type-specifier-indentation-method
    ('#:common-lisp '#:or) compute-type-and-etc-indentations)

(define-type-specifier-indentation-method
    ('#:common-lisp '#:not) compute-type-and-etc-indentations)

(define-type-specifier-indentation-method
    ('#:common-lisp '#:cons) compute-type-and-etc-indentations)
