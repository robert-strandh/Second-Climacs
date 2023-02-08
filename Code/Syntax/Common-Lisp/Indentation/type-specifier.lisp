(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the type specifier.  Instead, we
;;; compute the indentation of the sub-expressions of that expression.
(defgeneric compute-type-specifier-indentation (wad pawn client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AND and OR type specifiers.

(define-indentation-automaton compute-type-and/or-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol AND or the symbol OR, or
     ;; else we would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The remaining wads represent type specifiers.
   type-specifier
     (maybe-assign-indentation 3 3)
     (compute-type-specifier-indentation current-wad nil client)
     (next)
     (go type-specifier)))

(defmethod compute-type-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:and))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-ignore-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))

(defmethod compute-type-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:or))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-ignore-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))
