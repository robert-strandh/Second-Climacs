(cl:in-package #:climacs-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

(define-condition beginning-of-line (error)
  ())

(define-condition end-of-line (error)
  ())

(define-condition beginning-of-buffer (error)
  ())

(define-condition end-of-buffer (error)
  ())

(define-condition cursor-attached (error)
  ())

(define-condition cursor-detached (error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Line, cursor.

(defclass line ()
  ((%mediator :initarg :mediator :initform nil :accessor mediator)))

(defvar *empty-line-constructor*)

(defclass cursor () ())

(defclass detached-cursor (cursor)
  ())

(defclass attached-cursor (cursor)
  ((%line :initarg :line :accessor line)))

(defclass left-sticky-mixin () ())

(defclass right-sticky-mixin () ())

(defclass detached-left-sticky-cursor (detached-cursor left-sticky-mixin)
  ())

(defclass detached-right-sticky-cursor (detached-cursor right-sticky-mixin)
  ())

(defun make-left-sticky-cursor ()
  (make-instance 'detached-left-sticky-cursor))

(defun make-right-sticky-cursor ()
  (make-instance 'detached-right-sticky-cursor))

(defgeneric attach-cursor (cursor line &optional position))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer.

(defclass buffer ()
  ((%current-time :initform 0 :initarg :current-time :accessor current-time)
   (%contents :initarg :contents :accessor contents)))

;;; A mediator sits between a tree node of the splay tree and a line
;;; object.  Given a tree node, the mediator can be access by using
;;; the SPLAY-TREE:DATA function, and it can be set using the function
;;; (SETF SPLAY-TREE:DATA).  From the point of view of a line, the
;;; mediator can be accessed using the MEDIATOR function, and it can
;;; be set by using the (SETF MEDIATOR) function.
;;;
;;; There are several reasons for the existence of the mediator. One
;;; is to contain data about the corresponding line that is not
;;; strictly part of what a line object should contain, such as the
;;; creation time and modification time of the line, which rather has
;;; to do with the way the lines are organized into a buffer.  Another
;;; is to contain information about the entire subree rooted at the
;;; corresponding node such as the line count and the item count of
;;; that subtree.
;;;
;;; The mediator also contains a reference to the buffer in which it
;;; is located.  This reference is needed because when a node of the
;;; tree is splayed, that node must be explicitly assigned to the
;;; CONTENTS field of the buffer.
(defclass mediator ()
  ((%buffer
    :initform nil
    :initarg :buffer
    :accessor :buffer)
   (%tree-node
    :initform nil
    :initarg :tree-node
    :accessor tree-node)
   (;; The line count of the entire subtree.
    %line-count
    :initarg :line-count
    :accessor line-count)
   (;; The item count of the entire subtree.
    %item-count
    :initarg :item-count
    :accessor item-count)
   (%create-time
    :initarg :create-time
    :reader create-time)
   (%modify-time
    :initarg :modify-time
    :accessor modify-time)
   (%line
    :initarg :line
    :accessor line)))

(defmethod splay-tree:notify-rotate
    ((mediator1 mediator) (mediator2 mediator) (mediator3 null))
  (decf (line-count mediator1)
	(line-count mediator2))
  (decf (item-count mediator1)
	(item-count mediator2))
  (incf (line-count mediator2)
	(line-count mediator1))
  (incf (item-count mediator2)
	(item-count mediator1)))
  
  
(defmethod splay-tree:notify-rotate
    ((mediator1 mediator) (mediator2 mediator) (mediator3 mediator))
  (decf (line-count mediator1)
	(- (line-count mediator2) (line-count mediator3)))
  (decf (item-count mediator1)
	(- (item-count mediator2) (item-count mediator3)))
  (incf (line-count mediator2)
	(- (line-count mediator1) (line-count mediator3)))
  (incf (item-count mediator2)
	(- (item-count mediator1) (item-count mediator3))))

(defun make-empty-buffer ()
  (let* ((line (funcall *empty-line-constructor*))
	 (mediator (make-instance 'mediator
		     :line-count 1
		     :item-count 0
		     :create-time 0
		     :modify-time 0
		     :line line))
	 (node (splay-tree:make-node mediator)))
    (setf (tree-node mediator) node)
    (setf (mediator line) mediator)
    (make-instance 'buffer
      :current-time 1
      :contents node)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This convenience function allows us to call SPLAY on a line or a
;;; mediator.  It makes sure that the corresponding tree node is the
;;; root of the CONTENTS tree of the buffer.
;;;
;;; It is recommended to use this function, rather than calling
;;; SPLAY-TREE:SPLAY directly, because it is easy to forget that
;;; SPLAY-TREE:SPLAY does not return a useful value, and that it does
;;; not automatically change the contents of the buffer.

(defgeneric splay (thing))

(defmethod splay ((thing line))
  (splay (mediator line)))

(defmethod splay ((thing mediator))
  (let ((node (tree-node mediator)))
    (setf (contents (buffer mediator)) node)
    (splay-tree:splay node))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.

(defgeneric cursor-position (cursor))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.

(defgeneric (setf cursor-position) (position cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGINNING-OF-LINE-P.

(defgeneric beginning-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; END-OF-LINE-P.

(defgeneric end-of-line-p (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FORWARD-ITEM.

(defgeneric forward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BEGINNING-OF-LINE.

(defgeneric beginning-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; END-OF-LINE.

(defgeneric end-of-line (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BACKWARD-ITEM.

(defgeneric backward-item (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEM-BEFORE-CURSOR.

(defgeneric item-before-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEM-AFTER-CURSOR.

(defgeneric item-after-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; LINE-COUNT.

(defgeneric line-count (buffer))

(defmethod line-count ((buffer buffer))
  (line-count (splay-tree:data (contents buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; ITEM-COUNT.

(defgeneric item-count (buffer))

(defmethod item-count ((buffer buffer))
  (item-count (splay-tree:data (contents buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INSERT-ITEM.

(defgeneric insert-item (cursor item))

(defmethod insert-item ((cursor detached-cursor) item)
  (error 'cursor-detached))

(defmethod insert-item :after ((cursor attached-cursor) item)
  (splay (line cursor))
  (incf (item-count (mediator (line cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE-ITEM.

(defgeneric delete-item (cursor))

(defmethod delete-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod delete-item :after ((cursor attached-cursor))
  (splay (line cursor))
  (decf (item-count (mediator (line cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ERASE-ITEM.

(defgeneric erase-item (cursor))

(defmethod erase-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod erase-item :after ((cursor attached-cursor))
  (splay (line cursor))
  (decf (item-count (mediator (line cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIND-LINE.

(defgeneric find-line (buffer line-number))

(defmethod find-line ((buffer buffer) line-number)
  (when (minusp line-number)
    (error 'begining-of-buffer))
  (when (>= line-number (line-count buffer))
    (error 'end-of-buffer))
  (labels ((traverse (node line-number)
	     (let* ((left (splay-tree:left node))
		    (right (splay-tree:right node))
		    (left-count (if (null left)
				    0
				    (line-count (splay-tree:data left)))))
	       (cond ((< line-number left-count)
		      (traverse left line-number))
		     ((= line-number left-count)
		      (line (splay-tree:data node)))
		     (t
		      (traverse right (- line-number left-count 1)))))))
    (traverse (contents buffer) line-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SPLIT-LINE.

(defgeneric split-line (cursor))

;;; This generic function is part of the line-editing protocol, and
;;; should not be used directly by the application.  The application
;;; uses SPLIT-LINE, and SPLIT-LINE calls LINE-SPLIT-LINE.
;;;
;;; This generic function removes all the items to the right of the
;;; cursor in the line in which the cursor is located before the call,
;;; and returns a second line in which those items have been inserted.
;;; SPLIT-LINE must then insert that new line AFTER the one that the
;;; cursor is in before the call.
(defgeneric line-split-line (cursor))

(defmethod split-line (cursor)
  (let* ((existing-line (line cursor))
	 (existing-mediator (mediator existing-line))
	 (existing-node (tree-node existing-mediator))
	 (right-node (splay-tree:right existing-node))
	 (new-line (line-split-line cursor))
	 (buffer (buffer existing-mediator)))
    ;; Make sure the existing line is the root of the tree.
    (splay exiting-mediator)
    (unless (null right-node)
      ;; Detach the right subtree from the root.  We must update the
      ;; mediator so that it accurately reflects the item count and
      ;; the line count of the subtree.
      (let ((right-mediator (splay-tree:data right-node)))
	(decf (item-count existing-mediator) (item-count right-mediator))
	(decf (line-count existing-mediator) (line-count right-mediator)))
      (setf (splay-tree:right existing-node) nil))
    (let* ((new-mediator (make-instance 'mediator
			  :buffer buffer
			  :line-count (1+ (line-count existing-mediator))
			  :item-count (+ (item-count existing-mediator)
				         (item-count new-line))
			  :create-time (incf (current-time buffer))
			  :modify-time (current-time buffer)
			  :line new-line))
	   (new-node (splay-tree:make-node new-mediator)))
      (setf (tree-node new-mediator) new-node)
      (unless (null right-node)
	;; We must add the item count and the line count of the right
	;; subtree to the new node.
	(let ((right-mediator (splay-tree:data right-node)))
	  (incf (item-count new-mediator) (item-count right-mediator))
	  (incf (line-count new-mediator) (line-count right-mediator))))
      ;; Finally, we must make the new node the root of the buffer
      ;; splay tree.
      (setf (contents buffer) new-node)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update protocol.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEMS.
;;;
;;; Return the items of a line in a vector.  

(defgeneric items (line &key start end))


