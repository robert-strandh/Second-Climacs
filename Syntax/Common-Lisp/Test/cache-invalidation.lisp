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

(defun handle-modify (cache line-number)
  (setf (children cache)
	(loop for child in (children cache)
	      append (handle-modified-line child line-number))))

(defun handle-insert (cache line-number)
  (setf (children cache)
	(loop for child in (children cache)
	      append (handle-inserted-line child line-number))))

(defun handle-delete (cache line-number)
  (setf (children cache)
	(loop for child in (children cache)
	      append (handle-deleted-line child line-number))))

(defun random-update (cache)
  (let ((current-line-number 0))
    (flet ((random-operation ()
	     (case (random 4)
	       (0 ;; Skip operation.
		(incf current-line-number (random 3)))
	       (1 ;; The current line has been modified.
		(handle-modify cache current-line-number)
		(incf current-line-number))
	       (2 ;; A line has been inserted before the current line.
		(handle-insert cache current-line-number)
		(incf current-line-number))
	       (3 ;; The current line has been deleted.
		(handle-delete cache current-line-number)
		;; Do not increment the current line number
		))))
      (loop repeat 4
	    do (random-operation)))))

(defun compare-caches (real-cache test-cache)
  (let ((prefix (climacs-syntax-common-lisp::prefix real-cache))
	(residue (climacs-syntax-common-lisp::residue real-cache))
	(suffix (climacs-syntax-common-lisp::suffix real-cache))
	(nodes (nodes test-cache)))
    (assert (= (length nodes)
	       (+ (length prefix)
		  (length residue)
		  (length suffix))))
    (loop for parse-result in (append prefix residue)
	  for node in nodes
	  do (assert (= (climacs-syntax-common-lisp::start-line parse-result)
			(start-line node)))
	     (assert (= (climacs-syntax-common-lisp::end-line parse-result)
			(- (end-line node) (start-line node)))))
    (let ((rest (nthcdr (+ (length prefix) (length residue)) nodes)))
      (unless (null suffix)
	(assert (= (climacs-syntax-common-lisp::start-line (first suffix))
		   (start-line (first rest))))
	(assert (= (climacs-syntax-common-lisp::end-line (first suffix))
		   (- (end-line (first rest)) (start-line (first rest)))))
	(loop for parse-result in (rest suffix)
	      for node in (rest rest)
	      for line-number = (+ (climacs-syntax-common-lisp::start-line
				    (first suffix))
				   (climacs-syntax-common-lisp::start-line
				    (second suffix)))
		then (+ line-number (climacs-syntax-common-lisp::start-line
				     parse-result))
	      do (assert (= (climacs-syntax-common-lisp::start-line parse-result)
			    (start-line node)))
		 (assert (= (climacs-syntax-common-lisp::end-line parse-result)
			    (- (end-line node) (start-line node)))))))))

(defun analyzer-from-cache (cache prefix-length)
  (let* ((class-name 'climacs-syntax-common-lisp::parse-result)
	 (prefix (loop for node in (nodes cache)
		       for start-line = (start-line node)
		       for end-line = (end-line node)
		       repeat prefix-length
		       collect (make-instance class-name
				 :start-line start-line
				 :end-line (- end-line start-line))))
	 (rest (nthcdr prefix-length (nodes cache)))
	 (suffix (cons (make-instance class-name
			 :start-line (start-line (first rest))
			 :end-line (- (end-line (first rest))
				      (start-line (first rest))))
		       (loop for (prev node) on rest
			     until (null node)
			     collect (make-instance 'class-name
				       :start-line (- (start-line node)
						      (start-line prev))
				       :end-line (- (end-line node)
						    (start-line node)))))))
    (make-instance 'climacs-syntax-common-lisp::analyzer
      :prefix prefix
      :suffix suffix)))
