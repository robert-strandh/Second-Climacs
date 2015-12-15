(cl:in-package #:climacs-show-fundamental)

(defclass line (clim3:wrap)
  ((%buffer-line :initarg :buffer-line :reader buffer-line)))

(defclass fundamental-show ()
  ((%text-style
    :initform (clim3:text-style :free :fixed :roman 12)
    :initarg :text-style
    :accessor text-style)
   (%text-color
    :initform (clim3:make-color 0.0 0.0 0.0)
    :initarg :text-color
    :accessor text-color)
   ;; This wrap zone is supplied by the creator of this show.
   (%wrap :initarg :wrap :reader wrap)
   ;; The analyzer is supplied by the creator of this show.
   (%analyzer :initarg :analyzer :reader analyzer)
   ;; The cursor is supplied by the creator of this show.
   (%cursor :initarg :cursor :reader cursor)
   (%previous-cursor-line :initform nil :accessor previous-cursor-line)
   (%analyzer-time :initform -1 :accessor analyzer-time)))
  
(defun modify-line (show line)
  (let* ((style (text-style show))
	 (color (text-color show))
	 (buffer-line (buffer-line line))
	 (cursor (cursor show))
	 (items (cluffer:items (buffer-line line))))
    (setf (clim3:children line)
	  (if (eq (cluffer:line cursor) buffer-line)
	      (let* ((position (cluffer:cursor-position cursor))
		     (prefix (subseq items 0 position))
		     (prefix-text (clim3-text:text prefix style color))
		     (suffix (subseq items position))
		     (suffix-text (clim3-text:text suffix style color))
		     (cursor-color (clim3:make-color 0.0 0.0 1.0))
		     (box (clim3:hbrick 5 (clim3:opaque cursor-color))))
		(clim3:hbox* prefix-text box suffix-text (clim3:sponge)))
	      (let ((text (clim3-text:text items style color)))
		(clim3:hbox* text (clim3:sponge)))))))

(defun insert-line (show buffer-line line-number)
  (let ((line (make-instance 'line :buffer-line buffer-line)))
    (modify-line show line)
    (clim3:insert-zone (clim3:children (wrap show)) line line-number)))

(defun maybe-adjust-vpos (show)
  (let* ((wrap (wrap show))
	 (tree (clim3:children wrap))
	 (cursor-line (cluffer:line (cursor show)))
	 (line-number (cluffer:line-number cursor-line))
	 (leaf (clim3:find-zone tree line-number))
	 (offset (clim3:offset leaf))
	 (scroll-height (clim3:height (clim3-ext:parent wrap)))
	 (leaf-height (clim3:height leaf)))
    (cond ((and (>= (+ offset (clim3:vpos wrap)) 0)
		(<= (+ offset (clim3:vpos wrap) leaf-height) scroll-height))
	   ;; The line of the cursor is entirely visible, so we don't
	   ;; need to do anything.
	   nil)
	  ((< scroll-height leaf-height)
	   ;; In this situation, the line is currently not visible,
	   ;; and we eigther have a very tall line or a very short
	   ;; scroller.  We do our best and position the bottom of the
	   ;; line at the bottom of the scroll zone.
	   (setf (clim3:vpos wrap) (- scroll-height leaf-height)))
	  (t
	   ;; In this situation, the line is currently not visible,
	   ;; but the scroll zone is sufficiently tall to show the
	   ;; entire line.  We position the wrap so that the current
	   ;; line is in the middle of the scroll zone, or if that
	   ;; would make the position of the entire positive, then set
	   ;; it to 0 instead.
	   (setf (clim3:vpos wrap)
		 (min 0
		      (- (floor (- scroll-height leaf-height) 2) offset)))))))

(defmethod climacs-show:update (show)
  (let ((line-number 0)
	(tree (clim3:children (wrap show))))
    (unless (null (previous-cursor-line show))
      (modify-line show (clim3:find-zone tree (previous-cursor-line show))))
    (flet ((skip (n)
	     (incf line-number n))
	   (sync (buffer-line)
	     (loop for line = (clim3:find-zone tree line-number)
		   until (eq (buffer-line line) buffer-line)
		   do (clim3:delete-zone tree line-number))
	     (incf line-number))
	   (modify (buffer-line)
	     (loop for line = (clim3:find-zone tree line-number)
		   until (eq (buffer-line line) buffer-line)
		   do (clim3:delete-zone tree line-number))
	     (modify-line show (clim3:find-zone tree line-number))
	     (incf line-number))
	   (create (buffer-line)
	     (insert-line show buffer-line line-number)
	     (incf line-number)))
      (setf (analyzer-time show)
	    (climacs-analyzer-fundamental:synchronize
	     (analyzer show)
	     (analyzer-time show)
	     #'sync #'skip #'modify #'create)))
    (setf (previous-cursor-line show)
	  (cluffer:line-number (cluffer:line (cursor show))))
    (modify-line show (clim3:find-zone tree (previous-cursor-line show)))
    (maybe-adjust-vpos show)))

(defmethod climacs-show:make-show
    ((analyzer climacs-analyzer-fundamental:fundamental-analyzer)
     cursor
     wrap)
  (setf (clim3:children wrap) (clim3:vtree))
  (make-instance 'fundamental-show
    :analyzer analyzer
    :cursor cursor
    :wrap wrap))
