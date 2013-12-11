(cl:in-package #:chrono-tree)

(defclass node (splay-tree:node)
  ((%node-count :initform 1 :accessor node-count)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time
		 :accessor modify-time
		 :writer set-modify-time)
   (%max-modify-time :initarg :max-modify-time :accessor max-modify-time)))

(defmethod initialize-instance :after ((node node) &key &allow-other-keys)
  (set-modify-time (create-time node) node)
  (setf (max-modify-time node) (create-time node)))

(defmethod (setf modify-time) :before (new-time (node node))
  (if (null (splay-tree:parent node))
      (setf (max-modify-time node)
	    (max (max-modify-time node) new-time))
      (error "attempt to change the modify time of a node other than the root.")))

(defmethod (setf splay-tree:left) :before ((new-left null) (node node))
  (let ((left (splay-tree:left node))
	(right (splay-tree:right node)))
    (unless (null left)
      (decf (node-count node) (node-count left))
      (setf (max-modify-time node)
	    (max (modify-time node)
		 (if (null right)
		     0
		     (max-modify-time right)))))))

(defmethod (setf splay-tree:left) :before ((new-left node) (node node))
  (incf (node-count node) (node-count new-left))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-left))))

(defmethod (setf splay-tree:right) :before ((new-right null) (node node))
  (let ((left (splay-tree:left node))
	(right (splay-tree:right node)))
    (unless (null right)
      (decf (node-count node) (node-count right))
      (setf (max-modify-time node)
	    (max (modify-time node)
		 (if (null left)
		     0
		     (max-modify-time left)))))))

(defmethod (setf splay-tree:right) :before ((new-right node) (node node))
  (incf (node-count node) (node-count new-right))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-right))))

(defparameter *call-count* 0)
(defparameter *node-count* 0)
(defparameter *traversed-node-count* 0)
(defparameter *run-time* 0)

(defun reset ()
  (setf *call-count* 0
	*node-count* 0
	*traversed-node-count* 0
	*run-time* 0))

(defun report ()
  (format t "~%")
  (format t "Number of calls: ~d~%" *call-count*)
  (format t "Average number of nodes in tree: ~d~%"
	  (round (/ *node-count* *call-count*)))
  (format t "Average number of nodes traversed: ~d~%"
	  (round (/ *traversed-node-count* *call-count*)))
  (format t "Average runtime per call: ~a ms~%"
	  (float (/ *run-time* *call-count*))))

(defun synchronize (root time sync skip modify create)
  (let ((state :skip)
	(first-skip 0)
	(start-time (get-internal-run-time)))
    (incf *call-count*)
    (labels ((traverse (node offset)
	       (if (null node)
		   nil
		   (let* ((left (splay-tree:left node))
			  (left-count (if (null left) 0 (node-count left)))
			  (node-offset (+ offset left-count))
			  (right-offset (+ node-offset 1)))
		     (incf *traversed-node-count*)
		     (if (eq state :skip)
			 (if (> (max-modify-time node) time)
			     ;; We are in the :SKIP state and some
			     ;; nodes of this subtree have been
			     ;; modified.  We must traverse the left
			     ;; subtree in case some of theme are
			     ;; located there.
			     (progn
			       (traverse (splay-tree:left node) offset)
			       (if (eq state :skip)
				   ;; After traversing the left
				   ;; subtree, we are in the :SKIP
				   ;; state, either because none of
				   ;; the nodes in the left subtree
				   ;; were modified, or because we
				   ;; flipped to :UPDATE and then back
				   ;; to :SKIP.
				   (if (> (modify-time node) time)
				       ;; We are in the :SKIP state
				       ;; but the current node has
				       ;; been modified, so we must
				       ;; issue the SKIP operation,
				       ;; and then the CREATE or
				       ;; MODIFY operation, and also
				       ;; flip to the :UPDATE state.
				       (progn 
					 (let ((skip-count (- node-offset first-skip)))
					   (unless (zerop skip-count)
					     (funcall skip skip-count)))
					 (setf state :update)
					 (if (> (create-time node) time)
					     (funcall create node)
					     (funcall modify node))
					 (traverse (splay-tree:right node) right-offset))
				       ;; We are in the :SKIP state,
				       ;; and the current node has not
				       ;; been modified.
				       (traverse (splay-tree:right node) right-offset))
				   ;; We are in the :UPDATE state. 
				   (if (> (modify-time node) time)
				       (progn
					 (if (> (create-time node) time)
					     (funcall create node)
					     (funcall modify node))
					 (traverse (splay-tree:right node) right-offset))
				       (progn
					 (funcall sync node)
					 (setf state :skip)
					 (setf first-skip right-offset)
					 (traverse (splay-tree:right node) right-offset)))))
			     nil)
			 ;; We are in the :UPDATE state.
			 (if (> (max-modify-time node) time)
			     ;; We are in the :UPDATE state, and some
			     ;; nodes in this subtree have been
			     ;; modified.
			     (progn
			       (traverse (splay-tree:left node) offset)
			       (if (eq state :skip)
				   ;; In this situation, the traversal
				   ;; of the left subtree resulted in
				   ;; a state flip from :UPDATE to
				   ;; :SKIP.  This means that the
				   ;; nodes in some suffix of those in
				   ;; the left subtree have not been
				   ;; modified.
				   (if (> (modify-time node) time)
				       ;; We are in the :SKIP state,
				       ;; but the current node has
				       ;; been modified, so we must
				       ;; issue a SKIP operation and
				       ;; then either a CREATE or an
				       ;; UPDATE operation, and we
				       ;; must flip the state.
				       (progn 
					 ;; Issue hte SKIP operation.
					 (let ((skip-count (- node-offset first-skip)))
					   (unless (zerop skip-count)
					     (funcall skip skip-count)))
					 ;; Flip the state.
					 (setf state :update)
					 ;; Issue the CREATE or MODIFY operation.
					 (if (> (create-time node) time)
					     (funcall create node)
					     (funcall modify node))
					 ;; Continue traversing the
					 ;; remaining nodes in this
					 ;; subtree.
					 (traverse (splay-tree:right node) right-offset))
				       ;; We are in the :SKIP state
				       ;; and the current node has not
				       ;; been modified.  We just
				       ;; maintain the skip state and
				       ;; traverse the remaining nodes
				       ;; in this subtree.
				       (traverse (splay-tree:right node) right-offset))
				   ;; After traversing the left
				   ;; subtree, we are now in the
				   ;; :UPDATE state, either because
				   ;; the state was never flipped, or
				   ;; because it flipped to :SKIP and
				   ;; then back.
				   (if (> (modify-time node) time)
				       ;; We maintain a sequence of
				       ;; CREATE or MODIFY operations. 
				       (progn
					 (if (> (create-time node) time)
					     (funcall create node)
					     (funcall modify node))
					 ;; Continue traversing the
					 ;; remaining nodes in this
					 ;; subtree without changing
					 ;; the state.
					 (traverse (splay-tree:right node) right-offset))
				       ;; The current node was not
				       ;; modified, but we are in the
				       ;; :UPDATE state, so we need to
				       ;; flip to the :SKIP state and
				       ;; issue a SYNC operation.
				       (progn
					 (funcall sync node)
					 (setf state :skip)
					 (setf first-skip right-offset)
					 (traverse (splay-tree:right node) right-offset)))))
			     ;; We are in the :UPDATE state, but none
			     ;; of the nodes in this subtree have been
			     ;; modified or inserted.  This means that
			     ;; the previous node (call it P) that was
			     ;; traversed was created, modified, or
			     ;; inserted but the first node in this
			     ;; subtree (i.e., the one that is
			     ;; leftmost in the tree, call it N) is
			     ;; unchanged.  It is entirely possible
			     ;; that there have been nodes deleted
			     ;; that, as far as the client is
			     ;; concerned, are between P and N in the
			     ;; sequence.  For that reason, we must
			     ;; find issue a SYNC operation on N, so
			     ;; we must go left in the subtree.,
			     ;; maintaining the :SKIP state.
			     (progn
			       (traverse (splay-tree:left node) offset)
			       (if (eq state :skip)
				   ;; The traversal of the left
				   ;; subtree resulted in a state flip
				   ;; to :SKIP.  This will be the case
				   ;; whenever the left subtree is not
				   ;; NIL.  
				   nil
				   ;; The traversal of the left
				   ;; subtree did not result in a
				   ;; state flip to :SKIP, so we are
				   ;; still in the :UPDATE state.
				   ;; This happens only when the left
				   ;; subtree is NIL, and so because
				   ;; the current node has not been
				   ;; modified, we must flip here and
				   ;; issue a SYNC operation.
				   (progn 
				     (funcall sync node)
				     (setf state :skip)
				     (setf first-skip right-offset))))))))))
      (unless (null root)
	(incf *node-count* (node-count root)))
      (traverse root 0)
      (when (eq state :skip)
	(let ((skip-count (- (node-count root) first-skip)))
	  (unless (zerop skip-count)
	    (funcall skip skip-count)))))
    (incf *run-time* (- (get-internal-run-time) start-time))))
