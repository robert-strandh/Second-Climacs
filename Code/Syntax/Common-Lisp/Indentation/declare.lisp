(cl:in-package #:second-climacs-syntax-common-lisp)

;;; This generic function is analogous to
;;; COMPUTE-SUB-FORM-INDENTATIONS, but instead of handling special
;;; forms, it handles declaration specifiers.
(defgeneric compute-declaration-specifier-argument-indentations
    (wad pawn client))

(defun compute-declaration-indentations (indentation-units client)
  (declare (ignore indentation-units client)))

(defmethod compute-declaration-specifier-argument-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:declaration))) client)
  (let* ((indentation-units
           (compute-indentation-units (children wad)))
         (indentations
           (compute-declaration-indentations indentation-units client)))
    (loop for indentation-unit in indentation-units
          for indentation in indentations
          do (assign-indentation-of-wads-in-unit
              indentation-unit indentation))))
