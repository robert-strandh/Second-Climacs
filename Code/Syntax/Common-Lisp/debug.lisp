(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Return the first top-level wad, or NIL if there is no top-level
;;; wad.
(defun first-top-level-wad (cache)
  (if (null (prefix cache))
      (if (null (suffix cache))
          nil
          (first (suffix cache)))
      (first (last (prefix cache)))))

(defun print-wad-tree (root stream)
  (utilities.print-tree:print-tree
   stream root
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (princ node stream))
    nil
    #'children)))
