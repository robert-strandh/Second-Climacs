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

;;; Compare a parse result with an absolute location to a node.  The
;;; children of the parse result have relative locations as usual.
(defun equal-absolute (parse-result node)
  (and (= (climacs-syntax-common-lisp::start-line parse-result)
	  (start-line node))
       (= (climacs-syntax-common-lisp::end-line parse-result)
	  (- (end-line node) (start-line node)))
       (equal-relative-list (climacs-syntax-common-lisp::children parse-result)
			    (children node)
			    (start-line node))))

;;; Compare a parse result with a relative location to a node.
(defun equal-relative (parse-result node base)
  (and (= (climacs-syntax-common-lisp::start-line parse-result)
	  (- (start-line node) base))
       (= (climacs-syntax-common-lisp::end-line parse-result)
	  (- (end-line node) (start-line node)))
       (equal-relative-list (climacs-syntax-common-lisp::children parse-result)
			    (children node)
			    (start-line node))))

(defun equal-relative-list (parse-results nodes base)
  (or (and (null parse-results) (null nodes))
      (equal-relative (first parse-results) (first nodes) base)
      (equal-relative-list (rest parse-results)
			   (rest nodes)
			   (+ base (start-line (first nodes))))))

(defun compare-caches (analyzer cache)
  (let ((prefix (climacs-syntax-common-lisp::prefix analyzer))
	(residue (climacs-syntax-common-lisp::residue analyzer))
	(suffix (climacs-syntax-common-lisp::suffix analyzer))
	(nodes (nodes cache)))
    (assert (= (length nodes)
	       (+ (length prefix)
		  (length residue)
		  (length suffix))))
    (loop for parse-result in (append (reverse prefix) residue)
	  for node in nodes
	  do (assert (equal-absolute parse-result node)))
    (let ((rest (nthcdr (+ (length prefix) (length residue)) nodes)))
      (unless (null suffix)
	(assert (equal-absolute (first suffix) (first rest)))
	(assert (equal-relative-list (rest suffix)
				     (rest rest)
				     (start-line (first rest))))))))

;;; Given a node, create a parse result with an absolute location.
;;; The children of the parse results have relative locations as
;;; usual.
(defun make-absolute (node)
  (make-instance 'climacs-syntax-common-lisp::parse-result
    :start-line (start-line node)
    :end-line (- (end-line node) (start-line node))
    :children (make-relative-list (children node) (start-line node))))

;;; Given a node, create a parse result with a relative location.
(defun make-relative (node base)
  (make-instance 'climacs-syntax-common-lisp::parse-result
    :start-line (- (start-line node) base)
    :end-line (- (end-line node) (start-line node))
    :children (make-relative-list (children node) (start-line node))))

;;; Given a list of nodes (which have absolute locations), return a
;;; list of parse results where the first one has a location relative
;;; to BASE, and each of the others has a location relative to its
;;; predecessor.
(defun make-relative-list (nodes base)
  (if (null nodes)
      '()
      (cons (make-relative (first nodes) base)
	    (make-relative-list (rest nodes) (start-line (first nodes))))))

(defun analyzer-from-cache (cache prefix-length)
  (let* ((reverse-prefix (loop for node in (nodes cache)
			       repeat prefix-length
			       collect (make-absolute node)))
	 (rest (nthcdr prefix-length (nodes cache)))
	 (analyzer (make-instance 'climacs-syntax-common-lisp::analyzer)))
    (setf (climacs-syntax-common-lisp::prefix analyzer)
	  (reverse reverse-prefix))
    (unless (null rest)
      (let* ((suffix-start (start-line (first rest)))
	     (suffix (cons (make-absolute (first rest))
			   (make-relative-list (rest rest) suffix-start))))
	(setf (climacs-syntax-common-lisp::suffix analyzer) suffix)))
    analyzer))

(defun test-translation-and-comparison (n)
  (loop repeat n
	for cache = (make-random-cache)
	for prefix-length = (random (1+ (length (nodes cache))))
	for analyzer = (analyzer-from-cache cache prefix-length)
	do (compare-caches analyzer cache)))
