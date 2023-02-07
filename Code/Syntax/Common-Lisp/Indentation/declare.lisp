(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the declaration specifier.  In
;;; stead, we compute the indentation of the sub-expressions of that
;;; expression.
(defgeneric compute-declaration-specifier-indentation
    (wad pawn client))

(defun compute-declaration-indentations (indentation-units client)
  (declare (ignore indentation-units client)))

(defmethod compute-declaration-specifier-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:declaration))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-declaration-indentations indentation-units client)))
    (loop for indentation-unit in indentation-units
          for indentation in indentations
          do (assign-indentation-of-wads-in-unit
              indentation-unit indentation))))
