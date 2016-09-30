(cl:in-package #:climacs-syntax-common-lisp-test)

;;; We define a simplified version of the parse result.  It is
;;; simplified in that it always has absolute line numbers in it, and
;;; there are not column numbers, since they are not used or altered
;;; by the invalidation protocol.

(defclass node ()
  ((%start-line :initarg :start-line :accessor start-line)
   (%end-line :initarg :end-line :accessor end-line)
   (%children :initarg :children :accessor children)))

(defclass cache ()
  ((%nodes :initarg :nodes :accessor nodes)))

(defun random-child-count ()
  (cond ((zerop (random 2)) 0)
	((zerop (random 2)) 1)
	((zerop (random 2)) 2)
	((zerop (random 2)) 3)
	((zerop (random 2)) 4)
	((zerop (random 2)) 5)
	((zerop (random 2)) 6)
	((zerop (random 2)) 7)
	((zerop (random 2)) 8)
	((zerop (random 2)) 9)
	((zerop (random 2)) 10)
	(t 11)))

(defun make-random-node (start-line end-line)
  (make-instance 'node
    :start-line start-line
    :end-line end-line
    :children (make-random-children (random-child-count) start-line end-line)))

;;; Given two values A and B such that A <= B, return two random
;;; values C and D such that A <= C <= D <= B.
(defun random-middle (a b)
  (if (= a b)
      (values a a))
      (let ((c (+ a (random (1+ (- b a))))))
	(values c (if (= c b)
		      c
		      (+ c (random (1+ (- b c))))))))

(defun make-random-children (child-count start-line end-line)
  (cond ((zerop child-count) '())
	((= child-count 1)
	 (list (make-random-node start-line end-line)))
	(t (multiple-value-bind (end1 start2)
	       (random-middle start-line end-line)
	     (let ((half (floor child-count 2)))
	       (append (make-random-children
			half start-line end1)
		       (make-random-children
			(- child-count half) start2 end-line)))))))

(defun make-random-cache ()
  (let* ((start-line (random 10))
	 (end-line (+ start-line (random 20))))
    (make-instance 'cache
      :nodes (make-random-children (random 10) start-line end-line))))

;;; Return a list of residual nodes that are not affected by the
;;; modification.
(defun handle-modified-line (node line-number)
  (if (or (< line-number (start-line node))
	  (> line-number (end-line node)))
      (list node)
      (loop for child in (children node)
	    append (handle-modified-line child line-number))))

(defun move-node (node delta)
  (incf (start-line node))
  (incf (end-line node))
  (loop for child in (children node)
	do (move-node child delta)))

;;; Return a list of residual nodes that are not affected by the
;;; insertion.
(defun handle-inserted-line (node line-number)
  (cond ((> line-number (end-line node))
	 (list node))
	((<= line-number (start-line node))
	 (move-node node 1)
	 (list node))
	(t
	 (loop for child in (children node)
	       append (handle-inserted-line child line-number)))))

;;; Return a list of residual nodes that are not affected by the
;;; deletion.
(defun handle-deleted-line (node line-number)
  (cond ((> line-number (end-line node))
	 (list node))
	((< line-number (start-line node))
	 (move-node node -1)
	 (list node))
	(t
	 (loop for child in (children node)
	       append (handle-deleted-line child line-number)))))
