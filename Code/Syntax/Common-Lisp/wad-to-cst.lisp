(cl:in-package #:second-climacs-syntax-common-lisp)

;;; WAD has to be an EXPRESSION-WAD.
(defun wad-to-cst (wad)
  (if (null (ip:children wad))
      (make-instance 'cst:atom-cst
        :source wad
        :raw (ip:expression wad))
      (let ((result (cst:cst-from-expression nil)))
        (loop for child in (reverse (ip:children wad))
              when (typep child 'ip:expression-wad)
                do (setf result (cst:cons (wad-to-cst child) result)))
        (reinitialize-instance result :source wad)
        result)))
