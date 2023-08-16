(cl:in-package #:second-climacs-syntax-common-lisp)

;;; Return the first top-level wad, or NIL if there is no top-level
;;; wad.
(defun first-top-level-wad (cache)
  (if (null (ip::prefix cache))
      (if (null (ip::suffix cache))
          nil
          (first (ip::suffix cache)))
      (first (last (ip::prefix cache)))))

(defun print-wad-tree (root stream)
  (utilities.print-tree:print-tree
   stream root
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (princ node stream))
    nil
    #'ip:children)))
