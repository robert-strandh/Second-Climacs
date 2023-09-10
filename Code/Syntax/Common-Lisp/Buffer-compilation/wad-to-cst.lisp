(cl:in-package #:second-climacs-buffer-compilation)

;;; WAD has to be an EXPRESSION-WAD.
(defun wad-to-cst (wad)
  (if (null (inc:children wad))
      (cst:cst-from-expression (inc:expression wad) :source wad)
      (let ((result (cst:cst-from-expression nil)))
        (loop for child in (reverse (inc:children wad))
              when (typep child 'inc:expression-wad)
                do (setf result (cst:cons (wad-to-cst child) result)))
        (reinitialize-instance result :source wad)
        result)))
