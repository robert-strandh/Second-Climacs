(cl:in-package #:climacs-analyzer-fundamental)

;;;; The fundamental analyzer basically does no processing at all.  It
;;;; just takes the contents of each buffer line and organizes those
;;;; lines into a splay tree.  This might seem silly because after all
;;;; if the standard buffer representation is used, then the buffer
;;;; lines are already organized into a splay tree, so it might seem
;;;; that we could just use that tree rather than creating a mirror of
;;;; it.
;;;;
;;;; However, there are several reasons we can not do that:
;;;;
;;;; First of all, we are forced to access the buffer contents through
;;;; the buffer protocols, and we can not assume that it is
;;;; represented in any particular way.
;;;;
;;;; Second, the updates of the buffer and the updates of the analyzer
;;;; happen at different frequencies and granularities.  The buffer
;;;; contents is updated by each elementary operation on an item.  The
;;;; analyzer is updated for each iteration around the command loop.
;;;; There could be arbitrary many buffer updates in a single
;;;; iteration of the command loop.
;;;;
;;;; Third, while the organization of the buffer tree depends only the
;;;; hot spots of buffer modification, the organization of the
;;;; analyzer tree might also depend on what part of the buffer
;;;; is currently on display in the visible views.
;;;;
;;;; Recall that the result of the analysis can depend only on the
;;;; buffer contents, and not on how that contents is ultimatly
;;;; displayed, and that several views might share an analyzer.  So
;;;; for instance, different views might be on display in windows with
;;;; different width, requireing a buffer line to be split in
;;;; different ways in different views.  The syntax analyzer can not
;;;; take such display-specific information into account. 

(defgeneric current-time (clock))

(defclass clock ()
  ((%current-time :initform 0 :accessor current-time)))

(defun make-clock ()
  (make-instance 'clock))

(defun new-time (clock)
  (incf (current-time clock)))

;;; When the analysis starts, this variable is bound to the result of
;;; calling NEW-TIME on the clock of the analyzer.  Any creation or
;;; modification during the analysis should be considered has having
;;; taken place at the time indicated by this variable.
(defvar *current-time*)
  
(defun touch (thing)
  (setf (chrono-tree:modify-time thing) *current-time*))

(defclass node (chrono-tree:node)
  ((%buffer-line :initarg :buffer-line :reader buffer-line)
   (%analyzer :initarg :analyzer :reader analyzer)
   (%line-count :initform 1 :initarg line-count :accessor line-count)))
  
(defmethod (setf splay-tree:left) :before ((new-left null) (node node))
  (let ((left (splay-tree:left node)))
    (unless (null left)
      (decf (line-count node) (line-count left)))))

(defmethod (setf splay-tree:left) :before ((new-left node) (node node))
  (incf (line-count node) (line-count new-left)))

(defmethod (setf splay-tree:right) :before ((new-right null) (node node))
  (let ((right (splay-tree:right node)))
    (unless (null right)
      (decf (line-count node) (line-count right)))))

(defmethod (setf splay-tree:right) :before ((new-right node) (node node))
  (incf (line-count node) (line-count new-right)))

(defun splay (node)
  (setf (contents (analyzer node))
	(splay-tree:splay node)))

(defclass fundamental-analyzer ()
  (;; The buffer being analyzed.
   (%buffer :initarg :buffer :reader buffer)
   ;; This slot contains the current time of the buffer as it was when
   ;; it was last analyzed.  Only if the current time of the buffer is
   ;; greater than the contents of this slot is an update required.
   (%buffer-time :initform -1 :accessor buffer-time)
   ;; Each analyzer defines it own time so it has its own clock.  We
   ;; do not reuse the clock of the buffer, because some analyzers may
   ;; have more than one buffer in them.  This clock is used as usual
   ;; to mark when some item (paragraph or line) was created and when
   ;; it was modified.  This clock does not tick very fast; only once
   ;; for for every complete analysis of the buffer.  When the
   ;; analysis is complete, the current time of this clock is the same
   ;; as the create time of any paragraph or line that was created as
   ;; a result of the anlysis, and as the modify time of any paragraph
   ;; or line that was modified as a result of the analysis.
   (%clock :initform (make-clock) :reader clock)
   ;; 
   (%contents :initform '() :accessor contents)))
   
(defun find-node (node-number analyzer)
  (labels ((aux (node-number node)
	     (let* ((left (splay-tree:left node))
		    (left-count (if (null left) 0 (line-count left))))
	       (cond ((< node-number left-count)
		      (aux node-number left))
		     ((= node-number left-count)
		      node)
		     (t
		      (aux (- node-number (1+ left-count))
			   (splay-tree:right node)))))))
    (aux node-number (contents analyzer))))

(defun delete-node (node)
  (let ((analyzer (analyzer node)))
    (splay node)
    (cond ((null (splay-tree:left node))
	   (setf (contents analyzer) (splay-tree:right node)))
	  ((null (splay-tree:right node))
	   (setf (contents analyzer) (splay-tree:left node)))
	  (t
	   (let ((prev (loop for n = (splay-tree:left node)
			       then (splay-tree:right n)
			     until (null (splay-tree:right n))
			     finally (return n))))
	     (splay prev)
	     (let ((next (splay-tree:right node)))
	       (setf (splay-tree:right prev) nil)
	       (setf (splay-tree:right node) nil)
	       (setf (splay-tree:right prev) next)))))))

(defun update (analyzer)
  (let ((current-line-number 0)
	(*current-time* (new-time (clock analyzer))))
    (flet ((create (buffer-line)
	     (let ((new (make-instance 'node
			  :create-time *current-time*
			  :buffer-line buffer-line
			  :analyzer analyzer)))
	       (if (zerop current-line-number)
		   (setf (contents analyzer) new)
		   (let ((prev (find-node (1- current-line-number) analyzer)))
		     (splay prev)
		     (let ((next (splay-tree:right prev)))
		       (setf (splay-tree:right prev) nil)
		       (setf (splay-tree:left new) prev)
		       (setf (splay-tree:right new) next)
		       (setf (contents analyzer) new)))))
	     (incf current-line-number))
	   (sync (buffer-line)
	     (loop for node = (find-node current-line-number analyzer)
		   until (eq (buffer-line node) buffer-line)
		   do (delete-node node))
	     (incf current-line-number))
	   (skip (count)
	     (incf current-line-number count))
	   (modify (buffer-line)
	     (loop for node = (find-node current-line-number analyzer)
		   until (eq (buffer-line node) buffer-line)
		   do (delete-node node))
	     (touch (splay (find-node current-line-number analyzer)))
	     (incf current-line-number)))
      (climacs-buffer:update (buffer analyzer)
			     (buffer-time analyzer)
			     #'sync #'skip #'modify #'create)))
  ;; Record the time at which this update was made.
  (setf (buffer-time analyzer)
	(climacs-buffer:current-time (buffer analyzer))))

(defun synchronize (analyzer time sync skip modify create)
  (update analyzer)
  (chrono-tree:synchronize
   (contents analyzer)
   time
   (lambda (node) (funcall sync (buffer-line node)))
   skip
   (lambda (node) (funcall modify (buffer-line node)))
   (lambda (node) (funcall create (buffer-line node))))
  (current-time (clock analyzer)))

(defun make-fundamental-analyzer (buffer)
  (make-instance 'fundamental-analyzer :buffer buffer))
