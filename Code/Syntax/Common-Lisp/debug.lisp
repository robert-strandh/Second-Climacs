(cl:in-package #:second-climacs-syntax-common-lisp)

(defun print-wad-tree (root stream)
  (utilities.print-tree:print-tree
   stream root
   (utilities.print-tree:make-node-printer
    (lambda (stream depth node)
      (declare (ignore depth))
      (princ node stream))
    nil
    #'children)))
