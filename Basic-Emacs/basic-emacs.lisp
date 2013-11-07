(cl:in-package #:climacs-basic-emacs)

;;;; This file contains definitions of functions that sit on top of
;;;; the buffer editing protocol, and which gives the illusion that
;;;; the buffer is one long sequence of items, separated by newline
;;;; characters.

(defun forward-item (cursor &optional (count 1))
  (flet ((forward-one-item ()
	   (cond ((climacs-buffer:end-of-buffer-p cursor)
		  (error 'climacs-buffer:end-of-buffer))
		 ((climacs-buffer:end-of-line-p cursor)
		  (let* ((line (climacs-buffer:line cursor))
			 (buffer (climacs-buffer:buffer line))
			 (lineno (climacs-buffer:line-number line))
			 (next-line
			   (climacs-buffer:find-line buffer (1+ lineno))))
		    (climacs-buffer:detach-cursor cursor)
		    (climacs-buffer:attach-cursor cursor next-line)))
		 (t
		  (climacs-buffer:forward-item cursor)))))
    (cond ((zerop count)
	   nil)
	  ((minusp count)
	   (backward-item (- count)))
	  (t
	   (loop repeat count
		 do (forward-one-item))))))
	 
(defun backward-item (cursor &optional (count 1))
  (flet ((backward-one-item ()
	   (cond ((climacs-buffer:beginning-of-buffer-p cursor)
		  (error 'climacs-buffer:beginning-of-buffer))
		 ((climacs-buffer:beginning-of-line-p cursor)
		  (let* ((line (climacs-buffer:line cursor))
			 (buffer (climacs-buffer:buffer line))
			 (lineno (climacs-buffer:line-number line))
			 (prev-line
			   (climacs-buffer:find-line buffer (1- lineno))))
		    (climacs-buffer:detach-cursor cursor)
		    (climacs-buffer:attach-cursor cursor prev-line)))
		 (t
		  (climacs-buffer:backward-item cursor)))))
    (cond ((zerop count)
	   nil)
	  ((minusp count)
	   (forward-item (- count)))
	  (t
	   (loop repeat count
		 do (backward-one-item))))))
	 
(defun insert-item (cursor item &optional (count 1))
  (flet ((insert-one-item ()
	   (if (eql item #\Newline)
	       (climacs-buffer:split-line cursor)
	       (climacs-buffer:insert-item cursor item))))
    (loop repeat count
	  do (insert-one-item))))

(defun delete-item (cursor &optional (count 1))
  (flet ((delete-one-item ()
	   (cond ((climacs-buffer:end-of-buffer-p cursor)
		  (error 'climacs-buffer:end-of-buffer))
		 ((climacs-buffer:end-of-line-p cursor)
		  (climacs-buffer:join-line cursor))
		 (t
		  (climacs-buffer:delete-item cursor)))))
    (loop repeat count
	  do (delete-one-item))))

(defun erase-item (cursor &optional (count 1))
  (flet ((erase-one-item ()
	   (if (climacs-buffer:beginning-of-buffer-p cursor)
	       (error 'climacs-buffer:beginning-of-buffer)
	       (progn (backward-item cursor)
		      (delete-item cursor)))))
    (loop repeat count
	  do (erase-one-item))))

(defun item-before-cursor (cursor)
  (cond ((climacs-buffer:beginning-of-buffer-p cursor)
	 (error 'climacs-buffer:end-of-buffer))
	((climacs-buffer:beginning-of-line-p cursor)
	 #\Newline)
	(t
	 (climacs-buffer:item-before-cursor cursor))))

(defun item-after-cursor (cursor)
  (cond ((climacs-buffer:end-of-buffer-p cursor)
	 (error 'climacs-buffer:end-of-buffer))
	((climacs-buffer:end-of-line-p cursor)
	 #\Newline)
	(t
	 (climacs-buffer:item-after-cursor cursor))))

(defun buffer-from-stream (stream)
  (let* ((buffer (climacs-buffer:make-empty-buffer))
	 (line (climacs-buffer:find-line buffer 0))
	 (cursor (climacs-buffer:make-right-sticky-cursor)))
    (climacs-buffer:attach-cursor cursor line)
    (loop for line = (read-line stream nil nil)
	  until (null line)
	  do (loop for char across line
		   do (insert-item cursor char))
	     (insert-item cursor #\Newline))
    (climacs-buffer:detach-cursor cursor)
    buffer))

    
  
