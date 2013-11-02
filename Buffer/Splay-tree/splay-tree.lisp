(cl:in-package #:splay-tree)

(defclass node ()
  ((%parent :initform nil :initarg :parent :accessor parent)
   (%left :initform nil :accessor left)
   (%right :initform nil :accessor right)
   (%data :initarg :data :accessor data)))

(defun make-node (data)
  (make-instance 'node :data data))

(defgeneric notify-rotate (data1 data2 data3))

(defmethod notify-rotate (data1 data2 data3)
  (declare (ignore data1 data2 data3))
  nil)

(defun rotate-right (node)
  (let ((temp (left node)))
    (notify-rotate (data node)
		   (data temp)
		   (if (null (right temp)) nil (data (right temp))))
    (setf (left node) (right temp))
    (setf (right temp) node)
    (setf (parent temp) (parent node))
    (setf (parent node) temp)
    temp))

(defun rotate-left (node)
  (let ((temp (right node)))
    (notify-rotate (data node)
		   (data temp)
		   (if (null (left temp)) nil (data (left temp))))
    (setf (right node) (left temp))
    (setf (left temp) node)
    (setf (parent temp) (parent node))
    (setf (parent node) temp)
    temp))

(defgeneric splay (node))
(defgeneric splay-2 (node parent))
(defgeneric splay-3 (node parent grandparent))
(defgeneric splay-4 (node parent grandparent great-grandparent))

(defmethod splay ((node node))
  (splay-2 node (parent node)))

(defmethod splay-2 ((node node) (parent null))
  nil)

(defmethod splay-2 ((node node) (parent node))
  (splay-3 node parent (parent parent)))

(defmethod splay-3 ((node node) (parent node) (grandparent null))
  (if (eq node (left parent))
      (rotate-right parent)
      (rotate-left parent)))

(defmethod splay-3 ((node node) (parent node) (grandparent node))
  (splay-4 node parent grandparent (parent grandparent)))

(defun zig-zag (node parent grandparent)
  (if (eq node (left parent))
      (if (eq parent (left grandparent))
	  (rotate-right (rotate-right node))
	  (progn (setf (right grandparent)
		       (rotate-right parent))
		 (rotate-left grandparent)))
      (if (eq parent (left grandparent))
	  (progn (setf (left grandparent)
		       (rotate-left parent))
		 (rotate-right grandparent))
	  (rotate-left (rotate-left grandparent)))))

(defmethod splay-4
    ((node node) (parent node) (grandparent node) (great-grandparent null))
  (zig-zag node parent grandparent))

(defmethod splay-4  
    ((node node) (parent node) (grandparent node) (great-grandparent node))
  (let ((new (zig-zag node parent grandparent)))
    (if (eq grandparent (left great-grandparent))
	(setf (left great-grandparent) new)
	(setf (right great-grandparent) new))
    (splay grandparent)))

(defgeneric notify-detach (parent child))

(defmethod notify-detach (parent child)
  (declare (ignore parent child))
  nil)

(defun detach-left (node)
  (let ((left (left node)))
    (notify-detach (data node) (if (null left) nil (data left)))
    (unless (null left)
      (setf (parent left) nil))
    (setf (left node) nil)
    nil))

(defun detach-right (node)
  (let ((right (right node)))
    (notify-detach (data node) (if (null right) nil (data right)))
    (unless (null right)
      (setf (parent right) nil))
    (setf (right node) nil)
    nil))

(defgeneric notify-attach (parent child))
  
(defmethod notify-attach (parent child)
  (declare (ignore parent child))
  nil)

(defun attach-left (left node)
  (assert (null (left node)))
  (assert (null (parent left)))
  (notify-attach (data node) (data left))
  (setf (parent left) node)
  (setf (left node) left)
  nil)

(defun attach-right (node right)
  (assert (null (right node)))
  (assert (null (parent right)))
  (notify-attach (data node) (data right))
  (setf (parent right) node)
  (setf (right node) right)
  nil)
