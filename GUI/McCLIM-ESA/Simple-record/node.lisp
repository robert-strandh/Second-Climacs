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
      (decf (line-count node) (line-count left))
      (setf (width node)
	    (max (width (line node))
		 (if (null (clump-binary-tree:right node))
		     0
		     (width (clump-binary-tree:right node))))))))

(defmethod (setf clump-binary-tree:left) :after ((new-left node) (node node))
  (incf (height node) (height new-left))
  (incf (line-count node) (line-count new-left))
  (setf (width node)
	(max (width node)
	     (width new-left))))

(defmethod (setf clump-binary-tree:right) :before (new-right (node node))
  (declare (ignore new-right))
  (let ((right (clump-binary-tree:right node)))
    (unless (null right)
      (decf (height node) (height right))
      (decf (line-count node) (line-count right))
      (setf (width node)
	    (max (width (line node))
		 (if (null (clump-binary-tree:left node))
		     0
		     (width (clump-binary-tree:left node))))))))

(defmethod (setf clump-binary-tree:right) :after ((new-right node) (node node))
  (incf (height node) (height new-right))
  (incf (line-count node) (line-count new-right))
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

(defmethod clim:bounding-rectangle-max-y ((node node))
  (nth-value 3 (clim:bounding-rectangle* node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find a node by its number.

(defmethod line-count ((node null))
  0)

(defun find-node-by-number (root node-number)
  (labels ((find-aux (node node-number)
	     (let* ((left (clump-binary-tree:left node))
		    (left-line-count (line-count left)))
	       (cond ((< node-number left-line-count)
		      (find-aux left node-number))
		     ((= node-number left-line-count)
		      node)
		     (t
		      (find-aux (clump-binary-tree:right node)
				(- node-number left-line-count)))))))
    (if (<= 0 node-number (1- (line-count root)))
	(find-aux root node-number)
	(error "Node number ~d was given for a tree with ~d nodes."
	       node-number (line-count root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION.
;;;
;;; There can be at most one output record containing a particular
;;; position.  As a result, we can search the tree using iteration
;;; until we either find THE record containing the position, or we
;;; discover that no output record contains that position.
;;;
;;; Furthermore, we do not need to call the CLIM functions for
;;; determining the position of the output record during the search,
;;; which is good because they rely on stream-relative positions which
;;; might be a bit costly here.  Instead, we can use our relative
;;; positions and update those positions as we walk down the tree.
;;;
;;; We do not bother to restructure the tree in this method.
;;; Restructuring is done in REPLAY-OUTPUT-RECORD and in
;;; MAP-OVER-OUTPUT-RECORD-OVERLAPPING-REGION, and since one of those
;;; has been called before there is one ore more calls to
;;; MAP-OVER-OUTPUT-RECORDS-CONTAINING-POSITION, we can count on the
;;; tree having a reasonable structure to search here.

(defmethod clim:map-over-output-records-containing-position
    (function (node node) x y &optional x-offset y-offset &rest args)
  (declare (ignore x-offset y-offset))
  (loop with relative-y = y
	with current-node = node
	do (let* ((line (line current-node))
		  (dy (dy line))
		  (height (clim:bounding-rectangle-height line))
		  (left (clump-binary-tree:left current-node))
		  (right (clump-binary-tree:right current-node)))
	     (cond ((<= dy relative-y (1- (+ dy height)))
		    ;; We found THE line containing the position
		    (apply function current-node args)
		    (loop-finish))
		   ((< relative-y dy)
		    ;; If there is a record containing the position,
		    ;; then it must be one of the ones in the left
		    ;; child of CURRENT-NODE.
		    (if (null left)
			(loop-finish)
			(setf current-node left)))
		   (t
		    ;; Come here when RELATIVE-Y is greater than or
		    ;; equal to the sum of DY and HEIGHT.  If there is
		    ;; a record containing the position, then it must
		    ;; be one of the ones in the right child of
		    ;; CURRENT-NODE.
		    (if (null right)
			(loop-finish)
			(progn (setf current-node left)
			       (incf relative-y (+ dy height)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Restructure a tree according to a region.
;;;
;;; We define a BAND in the Y direction based on the Y coordinates of
;;; the bounding rectangle of REGION.  This band is the basis for the
;;; restructuring.
;;;
;;; We say that a subtree T rooted at some node N is either IRRELEVANT
;;; to some band B, or RELEVANT to B.  We say that T is IRRELEVANT to
;;; B if none of the nodes of T overlaps B.  If T is RELEVANT to B
;;; then the root node of T overlaps B and T is either COMPLETELY
;;; RELEVANT, LEFT RELEVANT, RIGHT RELEVANT, or MIDDLE RELEVANT to B.
;;; We say that T is COMPLETELY RELEVANT to B if every node of T
;;; overlaps B.  We say that T is LEFT RELEVANT to B if the left
;;; subtree of T is completely relevant to B and the right subtree of
;;; T is either irrelevant to B or left relevant to B.  We say that T
;;; is RIGHT RELEVANT to B if the right subtree of T is completely
;;; relevant to B and the left subtree of T is either irrelevant to B
;;; or right relevant to B.  We say that T is MIDDLE RELEVANT to B if
;;; the left subtree of T is either irrelevant to B or right relevant
;;; to B, and if the right subtree of T is either irrelevant to B or
;;; left relevant to B.
;;;
;;; The purpose of this exercise is to have a DENSE PREFIX of the tree
;;; that overlaps the band.  Every line in this dense prefix needs to
;;; be visited when the record is replayed.  In most likely scenarios,
;;; this dense prefix is only going to change occasionally, making the
;;; traversal of the tree very efficient compared to traversing
;;; several nodes that do not overlap the region.

(defun transform-band-for-right-subtree (node min-y max-y)
  (let* ((left (clump-binary-tree:left node))
	 (delta (if (null left) 0 (height left))))
    (values (+ min-y delta) (+ max-y delta))))

;;; NODE is the root of the tree to restructure, which means that,
;;; although the coordinates are relative to the parent, for the root,
;;; the parent is the entire output record, so the coordinates happen
;;; to also be absolute.
(defun restructure-tree (node region)
  (multiple-value-bind (min-x min-y max-x max-y)
      (clim:bounding-rectangle* region)
    (declare (ignore min-x max-x))
    (let ((dy (dy node))
	  (height (height node)))
      (unless (or (>= min-y (+ dy height))
		  (<= max-y dy))
	;; We only restructure the tree if there is some overlap
	;; between REGION and the lines of the tree.
	(restructure-left node min-y max-y)
	(restructure-right node min-y max-y)))))
