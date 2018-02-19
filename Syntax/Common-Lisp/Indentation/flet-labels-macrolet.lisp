(cl:in-package #:climacs-syntax-common-lisp)

(defgeneric indent-local-function-definition (wad client))

(defmethod indent-local-function-definition (wad client)
  (let* ((fun (lambda (w)
                (indent-lambda-list w client)))
         ;; We start by computing the indentation for the the
         ;; lambda list.
         (body-wads (compute-distinguished-indentation wad 4 fun)))
    (indent-body (+ (start-column wad) 2) body-wads client)))

(defgeneric indent-local-function-definitions (wad client))

(defmethod indent-local-function-definitions (wad client)
  (indent-list wad
               (lambda (w)
                 (indent-local-function-definition w client))))

(defun indent-flet-etc (wad client)
  (let* ((fun (lambda (w)
                (indent-local-function-definitions w client)))
         (arguments (compute-distinguished-indentation wad 4 fun)))
    (indent-body (+ (start-column wad) 2) arguments client)))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:flet))) client)
  (indent-flet-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:labels))) client)
  (indent-flet-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:macrolet))) client)
  (indent-flet-etc wad client))
