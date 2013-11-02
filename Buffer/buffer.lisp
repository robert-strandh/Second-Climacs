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

(defclass mediator ()
  ((%tree-node
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
  (splay-tree:splay (tree-node (mediator (line cursor))))
  (incf (item-count (mediator (line cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DELETE-ITEM.

(defgeneric delete-item (cursor))

(defmethod delete-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod delete-item :after ((cursor attached-cursor))
  (splay-tree:splay (tree-node (mediator (line cursor))))
  (decf (item-count (mediator (line cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ERASE-ITEM.

(defgeneric erase-item (cursor))

(defmethod erase-item ((cursor detached-cursor))
  (error 'cursor-detached))

(defmethod erase-item :after ((cursor attached-cursor))
  (splay-tree:splay (tree-node (mediator (line cursor))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Update protocol.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ITEMS.
;;;
;;; Return the items of a line in a vector.  

(defgeneric items (line &key start end))


