(cl:in-package #:second-climacs-syntax-common-lisp)

(defgeneric indent-local-function-definition (wad client))

(defmethod indent-local-function-definition (wad client)
  (when (and (typep wad 'expression-wad)
             (consp (children wad)))
    (let* ((fun (lambda (w)
                  (indent-lambda-list w client)))
           (arguments (rest (children wad)))
           (indentation (+ (start-column wad) 4))
           ;; We start by computing the indentation for the the
           ;; lambda list.
           (body-wads (compute-distinguished-indentation
                       arguments indentation fun)))
      (indent-body (+ (start-column wad) 2) body-wads client))))

(defgeneric indent-local-function-definitions (wad client))

(defmethod indent-local-function-definitions (wad client)
  (when (and (typep wad 'expression-wad)
             (consp (children wad)))
    (indent-list wad
                 (lambda (w)
                   (indent-local-function-definition w client)))))

(defun indent-flet-etc (wad client)
  (compute-indentation-single-distinguished
   wad
   (lambda (wad) (indent-local-function-definitions wad client))
   (lambda (indentation wads)
     (indent-body indentation wads client))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:flet))) client)
  (indent-flet-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:labels))) client)
  (indent-flet-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:macrolet))) client)
  (indent-flet-etc wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:lambda))) client)
  (indent-flet-etc wad client))
