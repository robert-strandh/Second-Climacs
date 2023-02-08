(cl:in-package #:second-climacs-syntax-common-lisp)

;;; As usual, we don't really compute the indentation of the
;;; expression itself, in this case the type specifier.  Instead, we
;;; compute the indentation of the sub-expressions of that expression.
(defgeneric compute-type-specifier-indentation
    (wad pawn client))
