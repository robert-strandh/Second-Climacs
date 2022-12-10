(cl:in-package #:second-climacs-syntax-common-lisp)

(defun compute-setf-and-setq-indentation (wad client)
  (flet ((indent-argument (argument indentation)
           (loop for wad in argument
                 unless (zerop (start-line wad))
                   do (setf (indentation wad) indentation))
           (when (typep (first (last argument)) 'expression-wad)
             (compute-child-indentations (first (last argument)) client))))
    (let ((arguments (split-wads (rest (children wad)))))
      (unless (null arguments)
        (let* ((first (first (first arguments)))
               (indentation (if (zerop (start-line first))
                                (start-column first)
                                (+ (start-column wad) 2))))
          (if (<= (length arguments) 2)
              ;; When there are only two arguments or fewer, indent
              ;; all arguments the same.
              (loop for argument in arguments
                    do (indent-argument argument indentation))
              ;; When there are more than two arguments, indent the
              ;; value arguments a bit more than the variable
              ;; arguments.
              (loop with additional = 0
                    for argument in arguments
                    do (indent-argument argument (+ indentation additional))
                       (setf additional (- 2 additional)))))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setf))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setq))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:psetf))) client)
  (compute-setf-and-setq-indentation wad client))
