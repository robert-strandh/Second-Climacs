(cl:in-package #:climacs-buffer)

(defun update (buffer time sync skip update create)
  (let ((state :skip)
	(first-skip 0))
    (labels ((traverse (node offset)
	       (if (null node)
		   nil
		   (let* ((left (splay-tree:left node))
			  (left-count (if (null left) 0 (line-count left)))
			  (node-offset (+ offset left-count))
			  (right-offset (+ node-offset 1)))
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
				       ;; UPDATE operation, and also
				       ;; flip to the :UPDATE state.
				       (progn 
					 (let ((skip-count (- node-offset first-skip)))
					   (unless (zerop skip-count)
					     (funcall skip skip-count)))
					 (setf state :update)
					 (if (> (create-time node) time)
					     (funcall create (line node))
					     (funcall update (line node)))
					 (traverse (splay-tree:right node) right-offset))
				       ;; We are in the :SKIP state,
				       ;; and the current node has not
				       ;; been modified.
				       (traverse (splay-tree:right node) right-offset))
				   ;; We are in the :UPDATE state. 
				   (if (> (modify-time node) time)
				       (progn
					 (if (> (create-time node) time)
					     (funcall create (line node))
					     (funcall update (line node)))
					 (traverse (splay-tree:right node) right-offset))
				       (progn
					 (funcall sync (line node))
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
					 ;; Issue the CREATE or UPDATE operation.
					 (if (> (create-time node) time)
					     (funcall create (line node))
					     (funcall update (line node)))
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
				       ;; CREATE or UPDATE operations. 
				       (progn
					 (if (> (create-time node) time)
					     (funcall create (line node))
					     (funcall update (line node)))
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
					 (funcall sync (line node))
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
				     (funcall sync (line node))
				     (setf state :skip)
				     (setf first-skip right-offset))))))))))
      (traverse (contents buffer) 0)
      (when (eq state :skip)
	(let ((skip-count (- (line-count (contents buffer)) first-skip)))
	  (unless (zerop skip-count)
	    (funcall skip skip-count)))))))
