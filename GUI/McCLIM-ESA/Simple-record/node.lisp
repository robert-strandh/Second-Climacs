(cl:in-package #:clim-simple-editor-record)

(defclass node (clump-binary-tree:node-with-parent)
  (;; This slot contains the total number of lines in the entire
   ;; subtree rooted at this node.
   (%line-count :initarg :line-count :accessor line-count)
   ;; This slot contains the sum of the heights of all the lines in
   ;; the subtree rooted at this node.
   (%height :initarg :height :accessor height)
   ;; This slot contains the maximum width of all the lines in the
   ;; subtree rooted at this node.
   (%width :initarg :width :accessor width)
   ;; This slot contains the child output record representing the line
   ;; of this node.
   (%line :initarg :line :reader line)))
