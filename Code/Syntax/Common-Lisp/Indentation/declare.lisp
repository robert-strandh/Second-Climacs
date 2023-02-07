(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the declaration specifier.  In
;;; stead, we compute the indentation of the sub-expressions of that
;;; expression.
(defgeneric compute-declaration-specifier-indentation
    (wad pawn client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DECLARATION declaration specifier.

(define-indentation-automaton compute-declaration-indentations
  (tagbody
     (next)
     ;; The current wad must be the symbol DECLARATION, or else we
     ;; would not be here.
     (maybe-assign-indentation 1 3)
     (next)
     ;; The current wad represents a name.
   name
     (maybe-assign-indentation 3 3)
     (next)
     (go name)))

(defun compute-declaration-indentations (indentation-units client)
  (declare (ignore indentation-units client)))

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:declaration))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-declaration-indentations indentation-units client)))
    (assign-indentation-of-wads-in-units indentation-units indentations)))
