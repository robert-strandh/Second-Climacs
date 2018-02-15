(cl:in-package #:climacs-syntax-common-lisp)

;;; Compute the indentation for a wad representing a single LET or
;;; LET* binding.
(defgeneric compute-binding-indentation (wad client))

(defmethod compute-binding-indentation (wad client)
  (let ((expression (expression wad)))
    (when (and (consp expression)
               (proper-list-p expression))
      (let* ((children (children wad))
             (first-child (first children))
             (start-column (start-column first-child))
             (remaining-children (rest children)))
        ;; We indent every remaining child as the first one, plus 2
        ;; columns.
        (loop for child in remaining-children
              unless (zerop (start-line child))
                do (setf (indentation child) (+ start-column 2))
              do (compute-child-indentations child client))))))

;;; Compute the indentation for a wad representing a list of LET or
;;; LET* bindings.
(defgeneric compute-binding-indentations (wad client))

(defmethod compute-binding-indentations (wad client)
  (let ((expression (expression wad)))
    (when (and (consp expression)
               (proper-list-p expression))
        ;; We indent every remaining child as the first one.
      (let* ((children (children wad))
             (first-child (first children))
             (start-column (start-column first-child))
             (remaining-children (rest children)))
        (compute-binding-indentation first-child client)
        ;; We indent every remaining child as the first one, plus 2
        ;; columns.
        (loop for child in remaining-children
              unless (zerop (start-line child))
                do (setf (indentation child) start-column)
              do (compute-binding-indentation child client))))))

(defun indent-body (column body-wads client)
  (loop for wad in body-wads
        unless (zerop (start-line wad))
          do (setf (indentation wad) column)
        do (compute-child-indentations wad client)))

(defun indent-up-to-and-including-bindings (column wads client)
  (loop for remaining on wads
        for wad = (first remaining)
        unless (zerop (start-line wad))
          do (setf (indentation wad) column)
        when (typep wad 'expression-wad)
          do (compute-binding-indentations wad client)
        until (typep wad 'expression-wad)
        finally (return (rest remaining))))

(defun compute-let-and-letstar-indentation (wad client)
  (let ((children (rest (children wad)))
        (wad-start-column (start-column wad)))
    (if (null children)
        ;; We have no bindings and no LET body.  Do nothing.
        nil
        (let ((start-line (start-line (first children)))
              (start-column (start-column (first children))))
          (if (zerop start-line)
              ;; The first argument starts on the same line as the
              ;; operator.  Indent every line up to and including the
              ;; first expression wad as the first child.
              (let ((body-wads (indent-up-to-and-including-bindings
                                start-column children client)))
                (indent-body (+ wad-start-column 2) body-wads client))
              ;; The first argument starts on a different line from
              ;; that of the operator.
              (let ((body-wads (indent-up-to-and-including-bindings
                                (+ wad-start-column 4) children client)))
                (indent-body (+ wad-start-column 2) body-wads client)))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:let))) client)
  (compute-let-and-letstar-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:let*))) client)
  (compute-let-and-letstar-indentation wad client))
