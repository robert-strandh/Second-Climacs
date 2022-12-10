(cl:in-package #:second-climacs-syntax-common-lisp)

(defgeneric indent-lambda-list (wad client))

;;; For now, we don't do anything sophisticated about lambda lists.  
(defmethod indent-lambda-list (wad client)
  (indent-simple-list wad))
