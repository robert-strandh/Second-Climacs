(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-block-etc (wad client)
  (let ((arguments (compute-distinguished-indentation wad 4 #'identity)))
    (indent-body (+ (start-column wad) 2) arguments client)))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:block))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:return-from))) client)
  (indent-block-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:the))) client)
  (indent-block-etc wad client))
