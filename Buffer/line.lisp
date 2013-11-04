(cl:in-package #:climacs-line)

(defclass line (climacs-buffer:line)
   ((%contents :initform nil :initarg :contents :accessor contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Open line.
;;; 
;;; The items of an open line are stored in a splay tree.

(defclass open-line (line)
  ())

(defclass open-cursor-mixin ()
  ((%node :initarg :node :accessor node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLOSED-LINE. 
;;; 
;;; The contents of a closed line is a vector of items.  At the
;;; moment, it is always a simple vector.

(defclass closed-line (line)
  ((%cursors :initarg :cursors :accessor cursors)))

(defclass closed-cursor-mixin ()
  ((%cursor-position
    :initarg :cursor-position
    :accessor climacs-buffer:cursor-position)))

(defclass closed-left-sticky-cursor
    (climacs-buffer:attached-cursor
     closed-cursor-mixin
     climacs-buffer:left-sticky-mixin)
  ())

(defclass closed-right-sticky-cursor
    (climacs-buffer:attached-cursor
     closed-cursor-mixin
     climacs-buffer:right-sticky-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPEN-LEFT-STICKY CURSOR.
;;;
;;; An open left-sticky cursor is physically attached to the node
;;; that is immediately to the left of the conceptual cursor position.
;;;
;;; As a consequence, a left-sticky cursor can be physically attached
;;; to the left sentinel of the line, which means that the cursor
;;; position is then 0, so that the cursor is at the beginning of the
;;; line.
;;;
;;; It also follows that a left-sticky cursor can not be attached to
;;; the right sentinel, because it would then have an invalid
;;; conceptual position. 

(defclass open-left-sticky-cursor
    (climacs-buffer:attached-cursor
     open-cursor-mixin
     climacs-buffer:left-sticky-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPEN-RIGHT-STICKY CURSOR.
;;;
;;; An open right-sticky cursor is physically attached to the node
;;; that is immediately to the right of the conceptual cursor
;;; position.
;;;
;;; As a consequence, a right-sticky cursor can be physically attached
;;; to the right sentinel of the line, which means that the cursor
;;; position is then equal to the item count of the line, so that the
;;; cursor is at the end of the line.
;;;
;;; It also follows that a right-sticky cursor can not be attached to
;;; the left sentinel, because it would then have an invalid
;;; conceptual position.

(defclass open-right-sticky-cursor
    (climacs-buffer:attached-cursor
     open-cursor-mixin
     climacs-buffer:right-sticky-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  In a line, there are always two sentinel entries, so the NODE
;;; COUNT is always equal to the ITEM COUNT plus 2.

(defclass node (splay-tree:node)
  ((%item :initarg :item :reader item)
   (%node-count :initarg :node-count :accessor node-count)
   (%line :initarg :line :accessor line)
   (%cursors :initform '() :accessor cursors)))

(defun make-node (item line)
  (make-instance 'node
    :item item
    :node-count 1
    :line line))

(defmethod (setf splay-tree:left) :before ((new-left null) (node node))
  (unless (null (splay-tree:left node))
    (decf (node-count node) (node-count (splay-tree:left node)))))

(defmethod (setf splay-tree:left) :after ((new-left node) (node node))
  (incf (node-count node) (node-count new-left)))

(defmethod (setf splay-tree:right) :before ((new-right null) (node node))
  (unless (null (splay-tree:right node))
    (decf (node-count node) (node-count (splay-tree:right node)))))

(defmethod (setf splay-tree:right) :after ((new-right node) (node node))
  (incf (node-count node) (node-count new-right)))

(defmethod splay-tree:splay :after ((node node))
  (setf (contents (line node)) node))

(defmethod climacs-buffer:item-count ((line open-line))
  (- (node-count (contents line)) 2))

(defun make-empty-line ()
  (let* ((line (make-instance 'open-line))
	 (end-sentinel-node (make-node nil line))
	 (start-sentinel-node (make-node nil line)))
    (setf (splay-tree:right start-sentinel-node) end-sentinel-node)
    (setf (contents line) start-sentinel-node)
    line))
    
(setq climacs-buffer:*empty-line-constructor* 'make-empty-line)

;;; We have two different POSITION concepts.  
;;;
;;; The first one is that of the position of an NODE in the line.  By
;;; convention, the position of the left sentinel is 0 and the
;;; position of the right sentinel is N-1 where N is the NODE COUNT
;;; of the line.
;;;
;;; The second is the position of a CURSOR in the line.  Conceptually,
;;; this position is located either before the leftmost item in the
;;; line, after the rightmost item in the line, or between two items
;;; in the line.  The cursor position that is before the leftmost item
;;; in the line is position number 0.  The maximum cursor position
;;; possible is equal to the item count of the line.

;;; Return the node position of an node. 
(defun node-position (node)
  (splay-tree:splay node)
  (if (null (splay-tree:left node))
      0
      (node-count (splay-tree:left node))))

;;; Given a valid node position, return the node at that position.
(defun node-at-position (line position)
  (assert (<= 0 position (1+ (climacs-buffer:item-count line))))
  (labels
      ((node-at-relative-position (root position)
	 (let ((relative-position-of-root
		 (if (null (splay-tree:left root))
		     0
		     (node-count (splay-tree:left root)))))
	   (cond ((= relative-position-of-root position)
		  root)
		 ((< relative-position-of-root position)
		  (node-at-relative-position
		   (splay-tree:right root)
		   (- position relative-position-of-root 1)))
		 (t
		  (node-at-relative-position
		   (splay-tree:left root)
		   position))))))
    (node-at-relative-position (contents line) position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.

;;; Since an open left-sticky cursor is physically attached to the
;;; node that is located immediately to the left of the cursor, the
;;; cursor position of a left-sticky cursor is the same as the node
;;; position of the node to which the cursor is attached.
(defmethod climacs-buffer:cursor-position ((cursor open-left-sticky-cursor))
  (node-position (node cursor)))

;;; Since an open right-sticky cursor is physically attached to the
;;; node that is located immediately to the right of the cursor, the
;;; cursor position of a right-sticky cursor is one less than the
;;; node position of the node to which the cursor is attached.
(defmethod climacs-buffer:cursor-position ((cursor open-right-sticky-cursor))
  (1- (node-position (node cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.

(defmethod (setf climacs-buffer:cursor-position)
    (position (cursor open-left-sticky-cursor))
  (setf (node cursor)
	(node-at-position (climacs-buffer:line cursor) position)))

(defmethod (setf climacs-buffer:cursor-position)
    (position (cursor open-right-sticky-cursor))
  (setf (node cursor)
	(node-at-position (climacs-buffer:line cursor) (1+ position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Given a line, an item, and an node position, insert a new node
;;; containing the item AFTER the node at the position given.  
;;;
;;; The position given must not be that of the right sentinel. 

(defgeneric insert-item-at-position (line item position))

(defmethod insert-item-at-position ((line line) item position)
  (let ((node (node-at-position line position))
	(new-node (make-node item line)))
    (splay-tree:splay node)
    (let ((right (splay-tree:right node)))
      (setf (splay-tree:right node) nil)
      (setf (splay-tree:left new-node) node)
      (setf (splay-tree:right new-node) right)
      (setf (contents line) new-node)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a line and an node position, delete the node at that
;;; position.
;;;
;;; The position given must not be that of one of the sentinels.

(defgeneric delete-item-at-position (line position))

(defmethod delete-item-at-position ((line line) position)
  (assert (<= 0 position (1- (climacs-buffer:item-count line))))
  (let ((node (node-at-position line (1+ position)))
	(prev (node-at-position line position)))
    (splay-tree:splay node)
    (splay-tree:splay prev)
    ;; Check that this intrinsic property of splay trees actually
    ;; holds.
    (assert (eq node (splay-tree:right prev)))
    ;; Furthermore, there can be no entries between the two, so
    ;; NODE does not have any left child.  We can therefore attach
    ;; the right child of NODE as a right child of PREV.
    (let ((next-node (splay-tree:right node)))
      (setf (splay-tree:right prev) nil)
      (setf (splay-tree:right node) nil)
      (setf (splay-tree:right prev) next-node)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEMS.

(defmethod climacs-buffer:items ((line open-line) &key (start 0) (end nil))
  (when (null end)
    (setf end (climacs-buffer:item-count line)))
  (let ((result (make-array (- end start)))
	(pos -1))
    (labels ((traverse (tree)
	       (unless (null tree)
		 (traverse (splay-tree:left tree))
		 (when (and (>= pos start)
			    (< pos end))
		   (setf (aref result pos)
			 (item tree)))
		 (incf pos)
		 (traverse (splay-tree:right tree)))))
      (traverse (contents line)))
    result))

(defmethod climacs-buffer:items ((line closed-line) &key (start 0) (end nil))
  (subseq (contents line) start end))

(defgeneric close-line (line))

(defmethod close-line ((line closed-line))
  nil)

(defmethod close-line ((line open-line))
  (let* ((item-count (climacs-buffer:item-count line))
	 (items (make-array item-count))
	 (cursors '())
	 (pos -1))
    (labels ((traverse (tree)
	       (unless (null tree)
		 (traverse (splay-tree:left tree))
		 (when (and (>= pos 0) (< pos item-count))
		   (setf (aref items pos) (item tree))
		   (loop for cursor in (cursors tree)
			 do (push cursor cursors)
			    (change-class
			     cursor
			     (if (typep cursor 'open-left-sticky-cursor)
				 'closed-left-sticky-cursor
				 'closed-right-sticky-cursor)
			     :position pos)))
		 (incf pos)
		 (traverse (splay-tree:right tree)))))
      (traverse (contents line)))
    (change-class line 'closed-line
		  :contents items
		  :cursors cursors)))

(defgeneric open-line (line))

(defmethod open-line ((line open-line))
  nil)

;;; FIXME: write it
(defmethod open-line ((line closed-line))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Detaching and attaching a cursor.

(defmethod climacs-buffer:attach-cursor
    ((cursor climacs-buffer:attached-cursor) line &optional position)
  (declare (ignore line position))
  (error 'climacs-buffer:cursor-attached))

(defmethod climacs-buffer:attach-cursor
    ((cursor climacs-buffer:detached-cursor)
     (line closed-line)
     &optional
       (position 0))
  (open-line line)
  (climacs-buffer:attach-cursor cursor line position))

(defmethod climacs-buffer:attach-cursor
    ((cursor climacs-buffer:detached-left-sticky-cursor)
     (line open-line)
     &optional
       (position 0))
  (when (> position (climacs-buffer:item-count line))
    (error 'climacs-buffer:end-of-line))
  (let ((node (node-at-position line position)))
    (push cursor (cursors node))
    (change-class cursor 'open-left-sticky-cursor
		  :line line
		  :node node))
  nil)
  
(defmethod climacs-buffer:attach-cursor
    ((cursor climacs-buffer:detached-right-sticky-cursor)
     (line open-line)
     &optional
       (position 0))
  (when (> position (climacs-buffer:item-count line))
    (error 'climacs-buffer:end-of-line))
  (let ((node (node-at-position line (1+ position))))
    (push cursor (cursors node))
    (change-class cursor 'open-right-sticky-cursor
		  :line line
		  :node node))
  nil)

(defmethod climacs-buffer:detach-cursor
    ((cursor climacs-buffer:detached-cursor))
  (error 'climacs-buffer:cursor-detached))

(defmethod climacs-buffer:detach-cursor ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:detach-cursor cursor))

(defmethod climacs-buffer:detach-cursor ((cursor open-left-sticky-cursor))
  (let ((node (node cursor)))
    (splay-tree:splay node)
    (setf (cursors node) (remove cursor (cursors node)))
    (change-class cursor 'detached-left-sticky-cursor)))

(defmethod climacs-buffer:detach-cursor ((cursor open-right-sticky-cursor))
  (let ((node (node cursor)))
    (splay-tree:splay node)
    (setf (cursors node) (remove cursor (cursors node)))
    (change-class cursor 'detached-right-sticky-cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations on cursors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE-P.
;;;
;;; Given a cursor, return true if and only if it is at the beginning
;;; of the line.

(defmethod climacs-buffer:beginning-of-line-p
    ((cursor climacs-buffer:detached-cursor))
  (error 'climacs-buffer:cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is 0.
(defmethod climacs-buffer:beginning-of-line-p
    ((cursor climacs-buffer:attached-cursor))
  (zerop (climacs-buffer:cursor-position cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE-P.

(defmethod climacs-buffer:end-of-line-p
    ((cursor climacs-buffer:detached-cursor))
  (error 'climacs-buffer:cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is the same as the number of items in
;;; the line.
(defmethod climacs-buffer:end-of-line-p
    ((cursor climacs-buffer:attached-cursor))
  (= (climacs-buffer:cursor-position cursor)
     (climacs-buffer:item-count (climacs-buffer:line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INSERT-ITEM.

(defmethod climacs-buffer:insert-item ((cursor closed-cursor-mixin) item)
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:insert-item cursor item))

(defmethod climacs-buffer:insert-item ((cursor open-cursor-mixin) item)
  (insert-item-at-position (climacs-buffer:line cursor)
			   item
			   (climacs-buffer:cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on DELETE-ITEM.

(defmethod climacs-buffer:delete-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:delete-item cursor))

(defmethod climacs-buffer:delete-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'climacs-buffer:end-of-line))
  (delete-item-at-position (climacs-buffer:line cursor)
			   (climacs-buffer:cursor-position cursor))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ERASE-ITEM.

(defmethod climacs-buffer:erase-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:erase-item cursor))

(defmethod climacs-buffer:erase-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'climacs-buffer:beginning-of-line))
  (delete-item-at-position (climacs-buffer:line cursor)
			   (1- (climacs-buffer:cursor-position cursor)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on FORWARD-ITEM

(defmethod climacs-buffer:forward-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:forward-item cursor))

(defmethod climacs-buffer:forward-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'climacs-buffer:end-of-line))
  (incf (climacs-buffer:cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BACKWARD-ITEM

(defmethod climacs-buffer:backward-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:backward-item cursor))

(defmethod climacs-buffer:backward-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'climacs-buffer:beginning-of-line))
  (decf (climacs-buffer:cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE.
;;;
;;; Position the cursor at the beginning of the line.

(defmethod climacs-buffer:beginning-of-line
    ((cursor climacs-buffer:detached-cursor))
  (error 'climacs-buffer:cursor-detached))

;;; The default method calls BACKWARD-ITEM until the cursor is at the
;;; beginning of the line.
(defmethod climacs-buffer:beginning-of-line
    ((cursor climacs-buffer:attached-cursor))
  (loop until (climacs-buffer:beginning-of-line-p cursor)
	do (climacs-buffer:backward-item cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE.
;;;
;;; Position the cursor at the end of the line.

(defmethod climacs-buffer:end-of-line
    ((cursor climacs-buffer:detached-cursor))
  (error 'climacs-buffer:cursor-detached))

;;; The default method calls FORWARD-ITEM until the cursor is at the
;;; end of the line.
(defmethod climacs-buffer:end-of-line
    ((cursor climacs-buffer:attached-cursor))
  (loop until (climacs-buffer:end-of-line-p cursor)
	do (climacs-buffer:forward-item cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-BEFORE-CURSOR.

(defmethod climacs-buffer:item-before-cursor
    ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:item-before-cursor cursor))

(defmethod climacs-buffer:item-before-cursor
    ((cursor open-left-sticky-cursor))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'climacs-buffer:beginning-of-line))
  (item (node-at-position (climacs-buffer:line cursor)
			   (climacs-buffer:cursor-position cursor))))

(defmethod climacs-buffer:item-before-cursor
    ((cursor open-right-sticky-cursor))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'climacs-buffer:beginning-of-line))
  (item (node-at-position (climacs-buffer:line cursor)
			   (1- (climacs-buffer:cursor-position cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ITEM-AFTER-CURSOR.

(defmethod climacs-buffer:item-after-cursor
    ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:item-after-cursor cursor))

(defmethod climacs-buffer:item-after-cursor
    ((cursor open-left-sticky-cursor))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'climacs-buffer:end-of-line))
  (item (node-at-position (climacs-buffer:line cursor)
			   (1+ (climacs-buffer:cursor-position cursor)))))

(defmethod climacs-buffer:item-after-cursor
    ((cursor open-right-sticky-cursor))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'climacs-buffer:end-of-line))
  (item (node-at-position (climacs-buffer:line cursor)
			   (climacs-buffer:cursor-position cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LINE-SPLIT-LINE.

(defmethod climacs-buffer:line-split-line ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:line-split-line cursor))

(defmethod climacs-buffer:line-split-line ((cursor open-cursor-mixin))
  (let* ((existing-line (climacs-buffer:line cursor))
	 (position (climacs-buffer:cursor-position cursor))
	 (node (node-at-position existing-line position))
	 (new-line (make-instance 'open-line)))
    (splay-tree:splay node)
    ;; The node that is now the root of the tree is the last node of
    ;; the existing line that should be preserved.  If the cursor is
    ;; at the beginning of the existing line, then that node is the
    ;; left sentinel.  Otherwise, it is not a sentinel.  The right
    ;; child of this node always exists, but it might be the right
    ;; sentinel.  In order to split the line, we remove the right
    ;; child of the node, and make the node the left chlid of a new
    ;; right sentinel.
    ;;
    ;; Similary, the right child of the root is the first node of the
    ;; existing line that should become part of the new line.  If the
    ;; cursor is at the end of the existing line, then that node is
    ;; the right sentinel.  Otherwise it is not a sentinel.  In order
    ;; to split the line, this node should become the right child of
    ;; a new left sentinel.
    ;;
    ;; The node count of the right child of the root is correct and
    ;; thus does not have to be modified.  The node count of the
    ;; root, on the other hand, includes the node count of its right
    ;; child.  We need to subtract the node count of the right child
    ;; from the node count of the root.
    ;; The new sentinels will have no cursors attached to them.  
    (let ((new-left-sentinel (make-instance 'node
			       :item nil
			       :line new-line
			       :node-count 1))
	  (new-right-sentinel (make-instance 'node
				:item nil
				:line existing-line
				:node-count 1))
	  (right (splay-tree:right node)))
      (setf (splay-tree:right node) nil)
      (setf (splay-tree:left new-right-sentinel) node)
      (setf (splay-tree:right new-left-sentinel) right)
      (setf (contents existing-line) new-right-sentinel)
      (setf (contents new-line) new-left-sentinel))
    new-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on LINE-JOIN-LINE.

(defmethod climacs-buffer:line-join-line ((line1 closed-line) line2)
  (open-line line1)
  (climacs-buffer:line-join-line line1 line2))

(defmethod climacs-buffer:line-join-line (line1 (line2 closed-line))
  (open-line line2)
  (climacs-buffer:line-join-line line1 line2))

(defmethod climacs-buffer:line-join-line ((line1 open-line) (line2 open-line))
  (let ((node1 (node-at-position line1 (- (node-count (contents line1)) 2)))
	(node2 (node-at-position line2 0)))
    (splay-tree:splay node1)
    (splay-tree:splay node2)
    ;; Any left-sticky cursor at the beginning of LINE2 must be positioned
    ;; at the end of LINE1.
    (setf (cursors node1)
	  (append (cursors node1) (cursors node2)))
    ;; Any right-sticky cursor at the end of LINE1 must be positioned
    ;; at the beginning of LINE2.
    (setf (cursors (splay-tree:right node2))
	  (append (splay-tree:right node2) (cursors (splay-tree:right node1))))
    ;; Reassign all the nodes in LINE2 to LINE1
    (labels ((traverse (node)
	       (if (null node)
		   nil
		   (progn (setf (line node) line1)
			  (traverse (splay-tree:left node))
			  (traverse (splay-tree:right node))))))
      (traverse (splay-tree:right node2)))
    ;; Finally attach the nodes of LINE2 to the end of the nodes of
    ;; LINE1.
    (let ((right (splay-tree:right node2)))
      (setf (splay-tree:right node1) nil)
      (setf (splay-tree:right node2) nil)
      (setf (splay-tree:right node1) right)))
  nil)
