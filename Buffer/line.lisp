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
  ((%entry :initarg :entry :accessor entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLOSED-LINE. 
;;; 
;;; The contents of a closed line is a vector of items.  At the
;;; moment, it is always a simple vector.

(defclass closed-line (line)
  ((%cursors :initarg :cursors :accessor cursors)))

(defclass closed-cursor-mixin ()
  ((%cursor-position :initarg :cursor-position :accessor cursor-position)))

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
;;; An open left-sticky cursor is physically attached to the entry
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
;;; An open right-sticky cursor is physically attached to the entry
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
;;; An entry is what is attached as `data' in every node in the splay
;;; tree.  In a line, there are always two sentinel entries, so the
;;; ENTRY COUNT is always equal to the ITEM COUNT plus 2.

(defclass entry ()
  ((%item :initarg :item :reader item)
   (%entry-count :initarg :entry-count :accessor entry-count)
   (%line :initarg :line :reader line)
   (%node :initform nil :initarg :node :accessor node)
   (%cursors :initform '() :accessor cursors)))

(defmethod splay-tree:notify-rotate ((e1 entry) (e2 entry) (e3 null))
  (decf (entry-count e1)
	(entry-count e2))
  (incf (entry-count e2)
	(entry-count e1)))

(defmethod splay-tree:notify-rotate ((e1 entry) (e2 entry) (e3 entry))
  (decf (entry-count e1)
	(- (entry-count e2) (entry-count e3)))
  (incf (entry-count e2)
	(- (entry-count e1) (entry-count e3))))

(defmethod splay-tree:notify-detach ((parent entry) (child entry))
  (decf (entry-count parent)
	(entry-count child)))

(defmethod splay-tree:notify-attach ((parent entry) (child entry))
  (incf (entry-count parent)
	(entry-count child)))

(defun make-entry (item line)
  (make-instance 'entry
    :item item
    :entry-count 1
    :line line))

(defmethod climacs-buffer:item-count ((line open-line))
  (- (entry-count (splay-tree:data (contents line))) 2))

(defun make-empty-line ()
  (let* ((line (make-instance 'open-line))
	 (end-sentinel-entry (make-entry nil line))
	 (end-node (splay-tree:make-node end-sentinel-entry))
	 (start-sentinel-entry (make-entry nil line))
	 (start-node (splay-tree:make-node start-sentinel-entry)))
    (splay-tree:attach-right start-node end-node)
    (setf (contents line) start-node)
    (setf (node end-sentinel-entry) end-node)
    (setf (node start-sentinel-entry) start-node)
    line))
    
(setq climacs-buffer:*empty-line-constructor* 'make-empty-line)

;;; We have two different POSITION concepts.  
;;;
;;; The first one is that of the position of an ENTRY in the line.  By
;;; convention, the position of the left sentinel is 0 and the
;;; position of the right sentinel is N-1 where N is the ENTRY COUNT
;;; of the line.
;;;
;;; The second is the position of a CURSOR in the line.  Conceptually,
;;; this position is located either before the leftmost item in the
;;; line, after the rightmost item in the line, or between two items
;;; in the line.  The cursor position that is before the leftmost item
;;; in the line is position number 0.  The maximum cursor position
;;; possible is equal to the item count of the line.

(defgeneric splay (thing))

(defmethod splay ((thing climacs-buffer:cursor))
  (splay (entry thing)))

(defmethod splay ((thing entry))
  (let ((node (node thing)))
    (splay-tree:splay node)
    (setf (contents (line thing)) node)))

;;; Return the entry position of an entry. 
(defun entry-position (entry)
  (splay entry)
  (let ((node (node entry)))
    (if (null (splay-tree:left node))
	0
	(entry-count (splay-tree:data (splay-tree:left node))))))

;;; Given a valid entry position, return the entry at that position.
(defun entry-at-position (line position)
  (assert (<= 0 position (1+ (climacs-buffer:item-count line))))
  (labels
      ((node-at-relative-position (root position)
	 (let ((relative-position-of-root
		 (if (null (splay-tree:left root))
		     0
		     (entry-count (splay-tree:data (splay-tree:left root))))))
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
    (splay-tree:data (node-at-relative-position (contents line) position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.

;;; Since an open left-sticky cursor is physically attached to the
;;; entry that is located immediately to the left of the cursor, the
;;; cursor position of a left-sticky cursor is the same as the entry
;;; position of the entry to which the cursor is attached.
(defmethod climacs-buffer:cursor-position ((cursor open-left-sticky-cursor))
  (entry-position (entry cursor)))

;;; Since an open right-sticky cursor is physically attached to the
;;; entry that is located immediately to the right of the cursor, the
;;; cursor position of a right-sticky cursor is one less than the
;;; entry position of the entry to which the cursor is attached.
(defmethod climacs-buffer:cursor-position ((cursor open-right-sticky-cursor))
  (1- (entry-position (entry cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.

(defmethod (setf climacs-buffer:cursor-position)
    (position (cursor open-left-sticky-cursor))
  (setf (entry cursor)
	(entry-at-position (climacs-buffer:line cursor) position)))

(defmethod (setf climacs-buffer:cursor-position)
    (position (cursor open-right-sticky-cursor))
  (setf (entry cursor)
	(entry-at-position (climacs-buffer:line cursor) (1+ position))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Given a line, an item, and an entry position, insert a new entry
;;; containing the item AFTER the entry at the position given.  
;;;
;;; The position given must not be that of the right sentinel. 

(defgeneric insert-item-at-position (line item position))

(defmethod insert-item-at-position ((line line) item position)
  (let* ((entry (entry-at-position line position))
	 (new-entry (make-entry item line))
	 (new-node (splay-tree:make-node new-entry)))
    (setf (node new-entry) new-node)
    (splay entry)
    (let* ((root (node entry))
	   (right (splay-tree:right root)))
      (splay-tree:detach-right root)
      (splay-tree:attach-left root new-node)
      (splay-tree:attach-right new-node right)
      (setf (contents line) new-node)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given a line and an entry position, delete the entry at that
;;; position.
;;;
;;; The position given must not be that of one of the sentinels.

(defgeneric delete-item-at-position (line position))

(defmethod delete-item-at-position ((line line) position)
  (assert (<= 1 position (climacs-buffer:item-count line)))
  (let ((entry (entry-at-position line position))
	(prev (entry-at-position line (1- position))))
    (splay entry)
    (splay prev)
    ;; Check that this intrinsic property of splay trees actually
    ;; holds.
    (assert (eq (node entry) (splay-tree:right (node prev))))
    ;; Furthermore, there can be no entries between the two, so
    ;; ENTRY does not have any left child.  We can therefore attach
    ;; the right child of ENTRY as a right child of PREV.
    (let ((next-node (splay-tree:right (node entry))))
      (splay-tree:detach-right (node entry))
      (splay-tree:attach-right (node prev) next-node)))
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
			 (item (splay-tree:data tree))))
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
		   (let ((entry (splay-tree:data tree)))
		     (setf (aref items pos) (item entry))
		     (loop for cursor in (cursors entry)
			   do (push cursor cursors)
			      (change-class
			       cursor
			       (if (typep cursor 'open-left-sticky-cursor)
				   'closed-left-sticky-cursor
				   'closed-right-sticky-cursor)
			       :position pos))))
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
  (error 'cursor-attached))

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
    (error 'end-of-line))
  (let ((entry (entry-at-position line position)))
    (push cursor (cursors entry))
    (change-class cursor 'open-left-sticky-cursor
		  :line line
		  :entry entry))
  nil)
  
(defmethod climacs-buffer:attach-cursor
    ((cursor climacs-buffer:detached-right-sticky-cursor)
     (line open-line)
     &optional
       (position 0))
  (when (> position (climacs-buffer:item-count line))
    (error 'end-of-line))
  (let ((entry (entry-at-position line (1+ position))))
    (push cursor (cursors entry))
    (change-class cursor 'open-right-sticky-cursor
		  :line line
		  :entry entry))
  nil)

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
  (error 'cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is 0.
(defmethod climacs-buffer:beginning-of-line-p
    ((cursor climacs-buffer:attached-cursor))
  (zerop (cursor-position cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on END-OF-LINE-P.

(defmethod climacs-buffer:end-of-line-p
    ((cursor climacs-buffer:detached-cursor))
  (error 'cursor-detached))

;;; The default method just calls CURSOR-POSITION and returns true if
;;; and only if that position is the same as the number of items in
;;; the line.
(defmethod climacs-buffer:end-of-line-p
    ((cursor climacs-buffer:attached-cursor))
  (= (cursor-position cursor) (climacs-buffer:item-count (climacs-buffer:line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on INSERT-ITEM.

(defmethod climacs-buffer:insert-item ((cursor closed-cursor-mixin) item)
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:insert-item cursor item))

(defmethod climacs-buffer:insert-item ((cursor open-cursor-mixin) item)
  (insert-item-at-position (climacs-buffer:line cursor) item (cursor-position cursor))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on DELETE-ITEM.

(defmethod climacs-buffer:delete-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:delete-item cursor))

(defmethod climacs-buffer:delete-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'end-of-line))
  (delete-item-at-position (climacs-buffer:line cursor) (cursor-position cursor))
  nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on ERASE-ITEM.

(defmethod climacs-buffer:erase-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:erase-item cursor))

(defmethod climacs-buffer:erase-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'beginning-of-line))
  (delete-item-at-position (climacs-buffer:line cursor) (1- (cursor-position cursor)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on FORWARD-ITEM

(defmethod climacs-buffer:forward-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:forward-item cursor))

(defmethod climacs-buffer:forward-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'end-of-line))
  (setf (cursor-position cursor) (1+ (cursor-position cursor)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BACKWARD-ITEM

(defmethod climacs-buffer:backward-item ((cursor closed-cursor-mixin))
  (open-line (climacs-buffer:line cursor))
  (climacs-buffer:backward-item cursor))

(defmethod climacs-buffer:backward-item ((cursor open-cursor-mixin))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'beginning-of-line))
  (setf (cursor-position cursor) (1- (cursor-position cursor)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BEGINNING-OF-LINE.
;;;
;;; Position the cursor at the beginning of the line.

(defmethod climacs-buffer:beginning-of-line
    ((cursor climacs-buffer:detached-cursor))
  (error 'cursor-detached))

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
  (error 'cursor-detached))

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
    (error 'beginning-of-line))
  (item (entry-at-position (climacs-buffer:line cursor) (cursor-position cursor))))

(defmethod climacs-buffer:item-before-cursor
    ((cursor open-right-sticky-cursor))
  (when (climacs-buffer:beginning-of-line-p cursor)
    (error 'beginning-of-line))
  (item (entry-at-position (climacs-buffer:line cursor) (1- (cursor-position cursor)))))

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
    (error 'end-of-line))
  (item (entry-at-position (climacs-buffer:line cursor) (1+ (cursor-position cursor)))))

(defmethod climacs-buffer:item-after-cursor
    ((cursor open-right-sticky-cursor))
  (when (climacs-buffer:end-of-line-p cursor)
    (error 'end-of-line))
  (item (entry-at-position (climacs-buffer:line cursor) (cursor-position cursor))))

