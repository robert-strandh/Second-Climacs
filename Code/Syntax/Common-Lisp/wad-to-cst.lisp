(cl:in-package #:second-climacs-syntax-common-lisp)

;;; WAD has to be an EXPRESSION-WAD.
(defun wad-to-cst (wad)
  (if (null (children wad))
      (make-instance 'cst:atom-cst
        :source wad
        :raw (expression wad))
      (let ((result (cst:cst-from-expression nil)))
        (loop for child in (children wad)
              when (typep child 'expression-wad)
                do (setf result (cst:cons (wad-to-cst child) result)))
        (reinitialize-instance result :source wad)
        result)))
