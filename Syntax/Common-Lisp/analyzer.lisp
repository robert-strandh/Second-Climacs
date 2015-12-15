(cl:in-package #:climacs-syntax-common-lisp)

;;; The items contained in the 2-3-tree are instances of this class.
(defclass line ()
  ((%parent :initarg :parent :accessor parent)
   ;; The corresponding buffer line so that we can synchronize and ask
   ;; for the iterms.
   (%buffer-line :initarg :buffer-line :reader buffer-line)
   ;; A simple vector which has the same length as the items of the
   ;; line and which contains instances of the class ENTRY, or
   ;; NIL.  It contains NIL if the corresponding line item is a
   ;; whitespace character, or if we do not know what the reader would
   ;; return if applied at this position. 
   (%cache :initarg :cache :accessor cache)))

(defclass entry ()
  (;; The stream position where this entry starts.
   (%start :initarg :start :reader start)
   ;; The stream position where this entry ends, i.e. where the next
   ;; read should start.
   (%end :initarg :end :reader end)
   ;; This slot contains a list of zero or one element.  If it
   ;; contains zero elements, then calling the reader yealded no
   ;; values, and if it contains one element, then calling the reader
   ;; yealded that element.
   (%value :initform nil :initarg :value :accessor value)
   ;; This slot contains a list of entries that were used to construct
   ;; this one.
   (%children :initform '() :initarg :children :accessor children)))

(defun items (line)
  (cluffer:items (buffer-line line)))

(defclass node-mixin ()
  ((%parent :initarg :parent :accessor parent)
   (%entries :initform '() :accessor entries)))

(defun delete-entries (node)
  (loop for entry in (entries node)
	for start = (start entry)
	do (setf (aref (cache (line start)) (column start)) nil))
  (setf (entries node) '()))

(defclass tree (2-3-tree:tree)
  ())

(defclass leaf (2-3-tree:leaf node-mixin)
  ())

(defmethod 2-3-tree:leaf-class ((tree tree))
  'leaf)

(defclass 2-node (2-3-tree:2-node node-mixin)
  ())

(defmethod 2-3-tree:2-node-class ((tree tree))
  '2-node)

(defmethod 2-3-tree:insert :before ((node 2-node) item position)
  (declare (ignore item position))
  (delete-entries node))

(defmethod 2-3-tree:delete :before ((node 2-node) position)
  (declare (ignore position))
  (delete-entries node))

(defclass 3-node (2-3-tree:3-node node-mixin)
  ())

(defmethod 2-3-tree:insert :before ((node 3-node) item position)
  (declare (ignore item position))
  (delete-entries node))

(defmethod 2-3-tree:delete :before ((node 3-node) position)
  (declare (ignore position))
  (declare (ignore position))
  (delete-entries node))

(defmethod 2-3-tree:3-node-class ((tree tree))
  '3-node)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; :BEFORE methods to assign the parent whenever a node or a line
;;; becomes the child of a node or a tree.  We don't bother with
;;; the case where some child is set to NIL.

(defmethod (setf 2-3-tree:root) :before ((new-root node-mixin) tree)
  (setf (parent new-root) tree))

(defmethod (setf 2-3-tree:left) :before ((new-left node-mixin) node)
  (setf (parent new-left) node))

(defmethod (setf 2-3-tree:middle) :before ((new-middle node-mixin) node)
  (setf (parent new-middle) node))

(defmethod (setf 2-3-tree:right) :before ((new-right node-mixin) node)
  (setf (parent new-right) node))

(defmethod (setf 2-3-tree:item) :before ((new-item line) leaf)
  (setf (parent new-item) leaf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn the 2-3-tree into a stream.

(defclass stream-position ()
  ((%tree :initarg :tree :reader tree)
   ;; The line is an instance of the LINE class above.
   (%line :initarg :line :accessor line)
   ;; The column is the index into the vector of items.
   (%column :initform 0 :initarg :column :accessor column)
   ;;; True if the position is at the end-of-file
   (%eof-p :initform nil :initarg :eof-p :accessor eof-p)))

(defun clone-position (stream-position)
  (make-instance 'stream-position
    :tree (tree stream-position)
    :line (line stream-position)
    :column (column stream-position)
    :eof-p (eof-p stream-position)))

(defun clobber-position (destination source)
  (setf (line destination) (line source))
  (setf (column destination) (column source))
  (setf (eof-p destination) (eof-p source)))

(defun make-stream (tree)
  (if (null (2-3-tree:root tree))
      (make-instance 'stream-position :tree tree :eof-p t)
      (loop for node = (2-3-tree:root tree) then (2-3-tree:left node)
	    until (typep node 'leaf)
	    finally (return (make-instance 'stream-position
			      :tree tree
			      :line (2-3-tree:item node))))))

(defun forward (stream-position)
  (let ((line (line stream-position)))
    (if (< (column stream-position) (length (cache line)))
	(incf (column stream-position))
	(let* ((node (parent line))
	       (parent (parent node)))
	  (loop while (and (not (typep parent 'tree))
			   (eq (2-3-tree:right parent) node))
		do (setf node parent
			 parent (parent node)))
	  (if (typep parent 'tree)
	      (setf (eof-p stream-position) t)
	      (let ((next (if (and (typep parent '3-node)
				   (eq (2-3-tree:left parent) node))
			      (2-3-tree:middle parent)
			      (2-3-tree:right parent))))
		(loop until (typep next 'leaf)
		      do (setf next (2-3-tree:left next)))
		(setf (line stream-position) (2-3-tree:item next))
		(setf (column stream-position) 0)))))))

(defun backward (stream-position)
  (setf (eof-p stream-position) nil)
  (let ((line (line stream-position)))
    (if (plusp (column stream-position))
	(decf (column stream-position))
	(let* ((node (parent line))
	       (parent (parent node)))
	  (loop while (and (not (typep parent 'tree))
			   (eq (2-3-tree:left parent) node))
		do (setf node parent
			 parent (parent node)))
	  (let ((next (if (and (typep parent '3-node)
			       (eq (2-3-tree:right parent) node))
			  (2-3-tree:middle parent)
			  (2-3-tree:left parent))))
	    (loop until (typep next 'leaf)
		  do (setf next (2-3-tree:right next)))
	    (setf (line stream-position) (2-3-tree:item next))
	    (setf (column stream-position)
		  (length (cache (line stream-position)))))))))
  
(defmethod sb-gray:stream-read-char ((stream stream-position))
  (if (eof-p stream)
      nil
      (let* ((line (line stream))
	     (cache (cache line))
	     (column (column stream)))
	(prog1 (if (= column (length cache))
		   #\Newline
		   (aref (items line) column))
	  (forward stream)))))

(defmethod sb-gray:stream-read-char-no-hang ((stream stream-position))
  (sb-gray:stream-read-char stream))

(defmethod sb-gray:stream-unread-char ((stream stream-position) char)
  (declare (ignore char))
  (backward stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Populating the 2-3-tree.

(defun add-entry (entry)
  (let* ((start (start entry))
	 (line (line start))
	 (column (column start)))
    (setf (aref (cache line) column) entry)
    (loop for node1 = (parent (line (start entry))) then (parent node1)
	  for node2 = (parent (line (end entry))) then (parent node2)
	  until (eq node1 node2)
	  finally (push entry (entries node1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The analyzer.

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

(defclass common-lisp-analyzer ()
  ((%buffer :initarg :buffer :reader buffer)
   ;; This slot contains the current time of the buffer as it was when
   ;; it was last analyzed.  Only if the current time of the buffer is
   ;; greater than the contents of this slot is an update required.
   (%buffer-time :initform -1 :accessor buffer-time)
   (%tree :initform (make-instance 'tree) :reader tree)
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
   (%clock :initform (make-clock) :reader clock)))

(defun update (analyzer)
  (let ((current-line-number 0)
	(tree (tree analyzer))
	(*current-time* (new-time (clock analyzer))))
    (flet ((create (buffer-line)
	     (2-3-tree:insert
	      tree
	      (make-instance 'line
		:buffer-line buffer-line
		:cache (make-array (length (cluffer:items buffer-line))
			:initial-element nil))
	      current-line-number)
	     (incf current-line-number))
	   (sync (buffer-line)
	     (loop for line = (2-3-tree:find tree current-line-number)
		   until (eq (buffer-line line) buffer-line)
		   do (2-3-tree:delete tree current-line-number))
	     (incf current-line-number))
	   (skip (count)
	     (incf current-line-number count))
	   (modify (buffer-line)
	     (let ((items (cluffer:items buffer-line)))
	       (loop for line = (2-3-tree:find tree current-line-number)
		     until (eq (buffer-line line) buffer-line)
		     do (2-3-tree:delete tree current-line-number)
		     finally (setf (cache line)
				   (make-array (length items)
					       :initial-element nil))))
	     (incf current-line-number)))
      (cluffer:update (buffer analyzer)
			     (buffer-time analyzer)
			     #'sync #'skip #'modify #'create)))
  ;; Record the time at which this update was made.
  (setf (buffer-time analyzer)
	(cluffer:current-time (buffer analyzer))))

   
