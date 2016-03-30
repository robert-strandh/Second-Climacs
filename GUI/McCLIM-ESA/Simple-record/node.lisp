(cl:in-package #:clim-simple-editor-record)

(defclass node (clump-binary-tree:node-with-parent
		clim:output-record
		relative-coordinates-output-record-mixin)
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
   (%line :initarg :line :reader line)
   ;; This slot contains a reference to the RECORD instance contains
   ;; this node.
   (%record :initarg :record :reader record)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary methods specialized to our nodes.
;;;
;;; These methods are invoked by CLUMP when it alters the structure of
;;; the tree in order to splay or just rotate some nodes.  We use
;;; these methods to update summary information in the nodes.

(defmethod (setf clump-binary-tree:left) :before (new-left (node node))
  (declare (ignore new-left))
  (let ((left (clump-binary-tree:left node)))
    (unless (null left)
      (decf (height node) (height left))
      (setf (width node)
	    (max (width (line node))
		 (if (null (clump-binary-tree:right node))
		     0
		     (width (clump-binary-tree:right node))))))))

(defmethod (setf clump-binary-tree:left) :after ((new-left node) (node node))
  (incf (height node) (height new-left))
  (setf (width node)
	(max (width node)
	     (width new-left))))

(defmethod (setf clump-binary-tree:right) :before (new-right (node node))
  (declare (ignore new-right))
  (let ((right (clump-binary-tree:right node)))
    (unless (null right)
      (decf (height node) (height right))
      (setf (width node)
	    (max (width (line node))
		 (if (null (clump-binary-tree:left node))
		     0
		     (width (clump-binary-tree:left node))))))))

(defmethod (setf clump-binary-tree:right) :after ((new-right node) (node node))
  (incf (height node) (height new-right))
  (setf (width node)
	(max (width node)
	     (width new-right))))

(defmethod clump-binary-tree:splay :after ((node node))
  (setf (contents (record node)) node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods to satisfy the CLIM requirements for output records.

(defmethod clim:output-record-parent ((node node))
  (let ((tree-parent (clump-binary-tree:parent node)))
    (if (null tree-parent)
	;; This means that NODE is the root of the binary tree.  And
	;; this, in turn, means that the parent output record is the
	;; top-level output record.
	(record node)
	;; Otherwise, the parent of the output record is the same as
	;; the parent in the binary tree.
	tree-parent)))

(defmethod clim:bounding-rectangle* ((node node))
  (multiple-value-bind (x y)
      (clim:output-record-position node)
    (values x y (+ x (width node)) (+ y (height node)))))

(defmethod clim:bounding-rectangle-min-x ((node node))
  0)

(defmethod clim:bounding-rectangle-max-x ((node node))
  (width node))

(defmethod clim:bounding-rectangle-min-y ((node node))
  (nth-value 0 (clim:bounding-rectangle* node)))
