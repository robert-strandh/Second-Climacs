(cl:in-package #:climacs-syntax-common-lisp)

(defun indent-prog1-etc (wad client)
  (let* ((fun (lambda (wad) (compute-child-indentations wad client)))
         (arguments (compute-distinguished-indentation wad 4 fun)))
    (loop with body-indentation = (+ (start-column wad) 2)
          for argument in arguments
          unless (zerop (start-line argument))
            do (setf (indentation argument) body-indentation))))

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
