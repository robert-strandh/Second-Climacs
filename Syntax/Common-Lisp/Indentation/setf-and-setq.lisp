(cl:in-package #:climacs-syntax-common-lisp)

;;; FIXME: Treat the case with only two arguments as a special case
;;; that does not need any additional indentation for the second
;;; argument.

(defun compute-setf-and-setq-indentation (wad client)
  (let ((arguments (split-wads (rest (children wad)))))
    (unless (null arguments)
      (let* ((first (first (first arguments)))
             (indentation (if (zerop (start-line first))
                              (start-column first)
                              (+ (start-column wad) 2))))

        (loop with additional = 0
              for argument in arguments
              do (loop for wad in argument
                       unless (zerop (start-line wad))
                         do (setf (indentation wad) (+ indentation additional)))
                 (when (typep (first (last argument)) 'expression-wad)
                   (compute-child-indentations (first (last argument)) client))
                 (setf additional (- 2 additional)))))))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setf))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:setq))) client)
  (compute-setf-and-setq-indentation wad client))

(defmethod compute-sub-form-indentations
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:psetf))) client)
  (compute-setf-and-setq-indentation wad client))
