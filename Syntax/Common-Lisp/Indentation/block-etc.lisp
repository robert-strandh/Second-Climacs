(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-block-etc (wad client)
  (compute-indentation-single-distinguished
   wad
   #'identity
   (lambda (indentation wads)
     (indent-body indentation wads client))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:block))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:return-from))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:the))) client)
  (indent-block-etc wad client))
