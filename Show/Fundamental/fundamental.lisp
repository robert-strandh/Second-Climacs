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
	 (items (climacs-buffer:items (buffer-line line))))
    (setf (clim3:children line)
	  (if (eq (climacs-buffer:line cursor) buffer-line)
	      (let* ((position (climacs-buffer:cursor-position cursor))
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
	  (climacs-buffer:line-number (climacs-buffer:line (cursor show))))
    (modify-line show (clim3:find-zone tree (previous-cursor-line show)))))

(defmethod climacs-show:make-show
    ((analyzer climacs-analyzer-fundamental:fundamental-analyzer)
     cursor
     wrap)
  (setf (clim3:children wrap) (clim3:vtree))
  (make-instance 'fundamental-show
    :analyzer analyzer
    :cursor cursor
    :wrap wrap))
