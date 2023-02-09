(cl:in-package #:second-climacs-syntax-common-lisp)

;;; We indent MAKE-INSTANCE in a special way, because we can then
;;; often both save horizontal space and make the form more readable.
;;; The first argument to MAKE-INSTANCE (i.e. the CLASS form) is
;;; indented by 6 columns in case it is not on the same line as the
;;; MAKE-INSTANCE symbol.  Each keyword form is indented by 2
;;; positions compared to the entire form and each value form is
;;; indented by 4 positions.  That way, if there are many long keyword
;;; arguments, it is possible to put the keyword form on one line and
;;; the value form on the next one.  Here is an example:
;;;
;;; (make-instance <class>
;;;   :first-keyword
;;;     (cond ...
;;;           ...
;;;           ...)
;;;   :second-keyword
;;;     (case ...
;;;           ...))

(defun indent-make-instance-etc (wad client)
  (let ((arguments (split-wads (rest (children wad)))))
    (unless (null arguments)
      ;; First handle the distinguished argument.
      (let ((distinguished (first arguments)))
        (align-or-indent distinguished (+ (start-column wad) 4))
        (let ((distinguished-expression (first (last distinguished))))
          (when (typep distinguished-expression 'expression-wad)
            (compute-child-indentations distinguished-expression client))))
      ;; Next, indent the keyword/value pairs.
      (loop with indentation = (+ (start-column wad) 2)
            with additional = 0
            for argument in (rest arguments)
            do (loop for wad in argument
                     unless (zerop (start-line wad))
                       do (setf (indentation wad) (+ indentation additional)))
               (when (typep (first (last argument)) 'expression-wad)
                 (compute-child-indentations (first (last argument)) client))
               (setf additional (- 2 additional))))))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:make-instance))) client)
  (indent-make-instance-etc wad client))

(defmethod compute-form-indentation
    (wad (pawn (eql (intern-pawn '#:common-lisp '#:reinitialize-instance))) client)
  (indent-make-instance-etc wad client))
