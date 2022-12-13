(cl:in-package #:second-climacs-syntax-common-lisp)

(defun mapwad (function wads reference-line &key from-end)
  (if from-end
      (mapwad-from-end function wads reference-line)
      (mapwad-from-beginning function wads reference-line)))
