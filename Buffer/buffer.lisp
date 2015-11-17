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
  ((%node :initarg :node :initform nil :accessor node)))

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

(defgeneric detach-cursor (cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer.

(defclass buffer ()
  ((%current-time :initform 0 :initarg :current-time :accessor current-time)
   (%contents :initarg :contents :accessor contents)))

;;; Return the buffer of a node, a cursor, or a line.
(defgeneric buffer (thing))

;;; The node contains a reference to the buffer in which it is
;;; located.  This reference is needed because when a node of the tree
;;; is splayed, that node must be explicitly assigned to the CONTENTS
;;; slot of the buffer.
(defclass node (splay-tree:node)
  ((%buffer :initform nil :initarg :buffer :accessor buffer)
   (;; The line count of the entire subtree.
    %line-count :initarg :line-count :accessor line-count)
   (;; The item count of the entire subtree.
    %item-count :initarg :item-count :accessor item-count)
   (%create-time :initarg :create-time :reader create-time)
   (%modify-time :initarg :modify-time :accessor modify-time)
   (%max-modify-time :initarg :max-modify-time :accessor max-modify-time)
   (%line :initarg :line :accessor line)))

(defmethod (setf splay-tree:left) :before ((new-left null) (node node))
  (unless (null (splay-tree:left node))
    (decf (line-count node) (line-count (splay-tree:left node)))
    (decf (item-count node) (item-count (splay-tree:left node)))
    (setf (max-modify-time node)
	  (max (modify-time node)
	       (if (null (splay-tree:right node))
		   0
		   (max-modify-time (splay-tree:right node)))))))

(defmethod (setf splay-tree:left) :after ((new-left node) (node node))
  (incf (line-count node) (line-count new-left))
  (incf (item-count node) (item-count new-left))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-left))))

(defmethod (setf splay-tree:right) :before ((new-right null) (node node))
  (unless (null (splay-tree:right node))
    (decf (line-count node) (line-count (splay-tree:right node)))
    (decf (item-count node) (item-count (splay-tree:right node)))
    (setf (max-modify-time node)
	  (max (modify-time node)
	       (if (null (splay-tree:left node))
		   0
		   (max-modify-time (splay-tree:left node)))))))

(defmethod (setf splay-tree:right) :after ((new-right node) (node node))
  (incf (line-count node) (line-count new-right))
  (incf (item-count node) (item-count new-right))
  (setf (max-modify-time node)
	(max (max-modify-time node)
	     (max-modify-time new-right))))

(defmethod splay-tree:splay :after ((node node))
  (setf (contents (buffer node)) node))

(defun make-empty-buffer ()
  (let* ((line (funcall *empty-line-constructor*))
	 (node (make-instance 'node
		     :line-count 1
		     :item-count 0
		     :create-time 0
		     :modify-time 0
		     :max-modify-time 0
		     :line line))
	 (buffer (make-instance 'buffer
		   :current-time 1
		   :contents node)))
    (setf (node line) node)
    (setf (buffer node) buffer)
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit protocol

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BUFFER
;;;
;;; Given a line or a cursor, return the buffer to which
;;; the line or cursor belongs. 

(defmethod buffer ((line line))
  (buffer (node line)))

(defmethod buffer ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod buffer ((cursor attached-cursor))
  (buffer (line cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CURSOR-POSITION.
;;;
;;; Given a cursor, return its conceptual position.

(defgeneric cursor-position (cursor))

(defmethod cursor-position ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CURSOR-POSITION).
;;;
;;; Given a cursor, set its conceptual position.

(defgeneric (setf cursor-position) (position cursor))

(defmethod (setf cursor-position) (position (cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE-P.

(defgeneric beginning-of-line-p (cursor))

(defmethod beginning-of-line-p ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE-P.

(defgeneric end-of-line-p (cursor))

(defmethod end-of-line-p ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FORWARD-ITEM.

(defgeneric forward-item (cursor))

(defmethod forward-item ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BACKWARD-ITEM.

(defgeneric backward-item (cursor))

(defmethod backward-item ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-LINE.

(defgeneric beginning-of-line (cursor))

(defmethod beginning-of-line ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-LINE.

(defgeneric end-of-line (cursor))

(defmethod end-of-line ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-BEFORE-CURSOR.

(defgeneric item-before-cursor (cursor))

(defmethod item-before-cursor ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ITEM-AFTER-CURSOR.

(defgeneric item-after-cursor (cursor))

(defmethod item-after-cursor ((cursor detached-cursor))
  (error 'cursor-detached))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Generic function LINE-COUNT.

(defgeneric line-count (buffer))

(defmethod line-count ((buffer buffer))
  (line-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Generic function ITEM-COUNT.

(defgeneric item-count (buffer))

(defmethod item-count ((buffer buffer))
  (item-count (contents buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function INSERT-ITEM.

(defgeneric insert-item (cursor item))

(defmethod insert-item ((cursor detached-cursor) item)
  (error 'cursor-detached))

(defmethod insert-item :after ((cursor attached-cursor) item)
  (let* ((node (node (line cursor)))
	 (buffer (buffer cursor)))
    (splay-tree:splay node)
    (incf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DELETE-ITEM.

(defgeneric delete-item (cursor))

(defmethod delete-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod delete-item :after ((cursor attached-cursor))
  (let ((node (node (line cursor)))
	(buffer (buffer cursor)))
    (splay-tree:splay node)
    (decf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ERASE-ITEM.

(defgeneric erase-item (cursor))

(defmethod erase-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod erase-item :after ((cursor attached-cursor))
  (let ((node (node (line cursor)))
	(buffer (buffer cursor)))
    (splay-tree:splay node)
    (decf (item-count node))
    (setf (modify-time node) (incf (current-time buffer)))
    (setf (max-modify-time node) (modify-time node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-LINE.

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
				    (line-count left))))
	       (cond ((< line-number left-count)
		      (traverse left line-number))
		     ((= line-number left-count)
		      (line node))
		     (t
		      (traverse right (- line-number left-count 1)))))))
    (traverse (contents buffer) line-number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LINE-NUMBER.

(defgeneric line-number (line))

(defmethod line-number (line)
  (let ((node (node line)))
    (splay-tree:splay node)
    (if (null (splay-tree:left node))
	0
	(line-count (splay-tree:left node)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIRST-LINE-P.

(defgeneric first-line-p (line))

(defmethod first-line-p (line)
  (= (line-number line) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function LAST-LINE-P.

(defgeneric last-line-p (line))

(defmethod last-line-p (line)
  (= (line-number line) (1- (line-count (buffer (node line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function BEGINNING-OF-BUFFER-P.

(defgeneric beginning-of-buffer-p (cursor))

(defmethod beginning-of-buffer-p ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod beginning-of-buffer-p ((cursor attached-cursor))
  (and (beginning-of-line-p cursor)
       (first-line-p (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function END-OF-BUFFER-P.

(defgeneric end-of-buffer-p (cursor))

(defmethod end-of-buffer-p ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod end-of-buffer-p ((cursor attached-cursor))
  (and (end-of-line-p cursor)
       (last-line-p (line cursor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPLIT-LINE.

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
	 (existing-node (node existing-line))
	 (buffer (buffer existing-node))
	 ;; The number of items that will be removed from the existing
	 ;; line and also the number of items of the new line.
	 (diff (- (item-count existing-line) (cursor-position cursor))))
    ;; Make sure the existing line is the root of the tree.
    (splay-tree:splay existing-node)
    (decf (item-count existing-node) diff)
    ;; If the cursor is at the end of the line, then the line
    ;; is not modified, but we don't take that into account.
    (let ((time (incf (current-time buffer))))
      (setf (modify-time existing-node) time)
      (setf (max-modify-time existing-node) time))
    (let* ((new-line (line-split-line cursor))
	   (time (incf (current-time buffer)))
	   (new-node (make-instance 'node
		       :buffer buffer
		       :line-count 1
		       :item-count diff
		       :create-time time
		       :modify-time time
		       :max-modify-time time
		       :line new-line)))
      (setf (node new-line) new-node)
      (let ((right-node (splay-tree:right existing-node)))
	(setf (splay-tree:right existing-node) nil)
	(setf (splay-tree:left new-node) existing-node)
	(setf (splay-tree:right new-node) right-node)
	(setf (contents buffer) new-node))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function JOIN-LINE.

(defgeneric join-line (cursor))

;;; This generic function is part of the line-editing protocol, and
;;; should not be used directly by the application.  The application
;;; uses JOIN-LINE, and JOIN-LINE calls LINE-JOIN-LINE.
;;;
;;; This generic function attaches all of the items of the second line
;;; to the end of the first line.  JOIN-LINE must then delete the
;;; second of these two lines from the tree in the buffer.
(defgeneric line-join-line (line1 line2))

(defmethod join-line ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod join-line ((cursor attached-cursor))
  (let ((line (line cursor)))
    (when (last-line-p line)
      (error 'end-of-buffer))
    (let* ((line-number (line-number line))
	   (next-line (find-line (buffer (node line)) (1+ line-number))))
      (let ((node-line (node line))
	    (node-next-line (node next-line)))
	(splay-tree:splay node-next-line)
	(splay-tree:splay node-line)
	;; Now LINE is on top and NEXT-LINE is its right child.
	;; Furthermore NEXT-LINE does not have any left children.
	(let ((time (incf (current-time (buffer node-line)))))
	  (setf (modify-time node-line) time)
	  (setf (max-modify-time node-line) time))
	(let ((right (splay-tree:right node-next-line)))
	  (setf (splay-tree:right node-line) nil)
	  (setf (splay-tree:right node-next-line) nil)
	  (setf (splay-tree:right node-line) right))
	(line-join-line line next-line))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEMS.
;;;
;;; Return the items of a line in a vector.  

(defgeneric items (line &key start end))
