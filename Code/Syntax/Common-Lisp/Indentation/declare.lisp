(cl:in-package #:second-climacs-syntax-common-lisp)

;;; This generic function is analogous to
;;; COMPUTE-SUB-FORM-INDENTATIONS, but instead of handling special
;;; forms, it handles declaration specifiers.
(defgeneric compute-declaration-specifier-argument-indentations
    (wad pawn client))
