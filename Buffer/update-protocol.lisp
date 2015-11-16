(cl:in-package #:climacs-buffer)

;;;; This function implements the buffer update protocol for a buffer
;;;; represented as a splay tree.
;;;;
;;;; The purpose of the buffer update protocol is to allow for a
;;;; number of edit operations to the buffer without updating the view
;;;; of the buffer.  This functionality is important because a single
;;;; command may result in an arbitrary number of edit operations to
;;;; the buffer, and we typically want the view to be updated only
;;;; once, when all those edit operations have been executed.
;;;;
;;;; The buffer update protocol has been designed to allow for
;;;; different representations of the view.  Seen from the UPDATE
;;;; function, the view takes the form of four different editing
;;;; operations, namely CREATE, MODIFY, SYNC, and SKIP.  These editing
;;;; operations are represented as functions that are supplied by
;;;; client code as arguments to the UPDATE function.  The editing
;;;; operations only assume that the view keeps a copy of the
;;;; structure of the lines of the buffer, and that this copy has a
;;;; cursor that is affected by the editing operations.  This cursor
;;;; can be before the first line of the view, after the last line of
;;;; the view, or between two lines of the view.  When BUFFER is
;;;; called by client code, the cursor is located before the first
;;;; line of the view.
;;;;
;;;; BUFFER is a buffer that might have been modified since the last
;;;; call to UPDATE.  TIME is the last time UPDATE was called; the
;;;; UPDATE function will report modifications since that time.
;;;;
;;;; The editing operations have the following meaning:
;;;;
;;;;  * CREATE indicates that a line has been created.  The function
;;;;    CREATE is called with the line as the argument.  Client code
;;;;    should insert the new line at the position of the cursor, and
;;;;    then leave the cursor positioned immediately after the
;;;;    inserted line.
;;;;
;;;;  * MODIFY indicates that a line has been modified.  The function
;;;;    MODIFY is called with the modified line as the argument.  The
;;;;    line that has been modified is the one immediately after the
;;;;    cursor.  At the end of this operation, client code should
;;;;    leave the cursor positioned immediately after the modified
;;;;    line.
;;;;
;;;;  * SYNC indicates the first unmodified line after a sequence of
;;;;    new or modified lines.  Accordingly, this function is called
;;;;    once, following one or more calls to CREATE or MODIFY.  This
;;;;    function is called with a single argument: the unmodified
;;;;    line.  Call this line L.  Client code must delete lines
;;;;    immediately following the cursor until the line immediately
;;;;    following the cursor is L.  At the end of this operation,
;;;;    client code should leave the cursor positioned immediately
;;;;    after L.
;;;;
;;;;  * SKIP indicates that a number of lines have not been subject to
;;;;    any modifications since the last call to UPDATE.  The SKIP
;;;;    function takes a single argument: the number of lines to skip.
;;;;    Call this number N.  The SKIP function is called first, to
;;;;    indicate that a prefix of length N of the buffer is
;;;;    unmodified, or after a SYNC operation to indicate that N lines
;;;;    following the one given as argument to the SYNC operation are
;;;;    unmodified.  Client code should move the cursor forward N
;;;;    positions.

(defun update (buffer time sync skip modify create)
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
				       ;; MODIFY operation, and also
				       ;; flip to the :UPDATE state.
				       (progn 
					 (let ((skip-count (- node-offset first-skip)))
					   (unless (zerop skip-count)
					     (funcall skip skip-count)))
					 (setf state :update)
					 (if (> (create-time node) time)
					     (funcall create (line node))
					     (funcall modify (line node)))
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
					     (funcall modify (line node)))
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
				       ;; MODIFY operation, and we
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
					     (funcall create (line node))
					     (funcall modify (line node)))
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
					     (funcall create (line node))
					     (funcall modify (line node)))
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
