(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-prog1-etc (wad client)
  (compute-indentation-single-distinguished
   wad
   (lambda (wad) (compute-child-indentations wad client))
   (lambda (indentation wads)
     (indent-body indentation wads client))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:prog1))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-prog1))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:multiple-value-call))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:catch))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:throw))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:unwind-protect))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:when))) client)
  (indent-prog1-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:unless))) client)
  (indent-prog1-etc wad client))
