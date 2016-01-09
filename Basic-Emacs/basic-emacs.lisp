(cl:in-package #:climacs-basic-emacs)

;;;; This file contains definitions of functions that sit on top of
;;;; the buffer editing protocol, and which gives the illusion that
;;;; the buffer is one long sequence of items, separated by newline
;;;; characters.

(defun forward-item (cursor &optional (count 1))
  (flet ((forward-one-item ()
	   (cond ((cluffer:end-of-buffer-p cursor)
		  (error 'cluffer:end-of-buffer))
		 ((cluffer:end-of-line-p cursor)
		  (let* ((line (cluffer:line cursor))
			 (buffer (cluffer:buffer line))
			 (lineno (cluffer:line-number line))
			 (next-line
			   (cluffer:find-line buffer (1+ lineno))))
		    (cluffer:detach-cursor cursor)
		    (cluffer:attach-cursor cursor next-line)))
		 (t
		  (cluffer:forward-item cursor)))))
    (cond ((zerop count)
	   nil)
	  ((minusp count)
	   (backward-item (- count)))
	  (t
	   (loop repeat count
		 do (forward-one-item))))))
	 
(defun backward-item (cursor &optional (count 1))
  (flet ((backward-one-item ()
	   (cond ((cluffer:beginning-of-buffer-p cursor)
		  (error 'cluffer:beginning-of-buffer))
		 ((cluffer:beginning-of-line-p cursor)
		  (let* ((line (cluffer:line cursor))
			 (buffer (cluffer:buffer line))
			 (lineno (cluffer:line-number line))
			 (prev-line
			   (cluffer:find-line buffer (1- lineno))))
		    (cluffer:detach-cursor cursor)
		    (cluffer:attach-cursor cursor prev-line)
		    (cluffer:end-of-line cursor)))
		 (t
		  (cluffer:backward-item cursor)))))
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
	       (cluffer:split-line cursor)
	       (cluffer:insert-item cursor item))))
    (loop repeat count
	  do (insert-one-item))))

(defun delete-item (cursor &optional (count 1))
  (flet ((delete-one-item ()
	   (cond ((cluffer:end-of-buffer-p cursor)
		  (error 'cluffer:end-of-buffer))
		 ((cluffer:end-of-line-p cursor)
		  (cluffer:join-line cursor))
		 (t
		  (cluffer:delete-item cursor)))))
    (loop repeat count
	  do (delete-one-item))))

(defun erase-item (cursor &optional (count 1))
  (flet ((erase-one-item ()
	   (if (cluffer:beginning-of-buffer-p cursor)
	       (error 'cluffer:beginning-of-buffer)
	       (progn (backward-item cursor)
		      (delete-item cursor)))))
    (loop repeat count
	  do (erase-one-item))))

(defun item-before-cursor (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
	 (error 'cluffer:end-of-buffer))
	((cluffer:beginning-of-line-p cursor)
	 #\Newline)
	(t
	 (cluffer:item-before-cursor cursor))))

(defun item-after-cursor (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
	 (error 'cluffer:end-of-buffer))
	((cluffer:end-of-line-p cursor)
	 #\Newline)
	(t
	 (cluffer:item-after-cursor cursor))))

(defun buffer-from-stream (stream)
  (let* ((line (make-instance 'cluffer-standard-line:closed-line))
	 (buffer (make-instance 'cluffer-standard-buffer:buffer
		   :initial-line line))
	 (cursor (make-instance
		     'cluffer-standard-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor line)
    (loop for char = (read-char stream nil nil)
	  until (null char)
	  do (insert-item cursor char))
    (cluffer:detach-cursor cursor)
    buffer))

    
  
