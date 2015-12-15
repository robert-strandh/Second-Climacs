(cl:in-package #:climacs-basic-emacs)

;;;; This file contains definitions of functions that sit on top of
;;;; the buffer editing protocol, and which gives the illusion that
;;;; the buffer is one long sequence of items, separated by newline
;;;; characters.

(defun forward-item (cursor &optional (count 1))
  (flet ((forward-one-item ()
	   (cond ((clufferend-of-buffer-p cursor)
		  (error 'clufferend-of-buffer))
		 ((clufferend-of-line-p cursor)
		  (let* ((line (clufferline cursor))
			 (buffer (clufferbuffer line))
			 (lineno (clufferline-number line))
			 (next-line
			   (clufferfind-line buffer (1+ lineno))))
		    (clufferdetach-cursor cursor)
		    (clufferattach-cursor cursor next-line)))
		 (t
		  (clufferforward-item cursor)))))
    (cond ((zerop count)
	   nil)
	  ((minusp count)
	   (backward-item (- count)))
	  (t
	   (loop repeat count
		 do (forward-one-item))))))
	 
(defun backward-item (cursor &optional (count 1))
  (flet ((backward-one-item ()
	   (cond ((clufferbeginning-of-buffer-p cursor)
		  (error 'clufferbeginning-of-buffer))
		 ((clufferbeginning-of-line-p cursor)
		  (let* ((line (clufferline cursor))
			 (buffer (clufferbuffer line))
			 (lineno (clufferline-number line))
			 (prev-line
			   (clufferfind-line buffer (1- lineno))))
		    (clufferdetach-cursor cursor)
		    (clufferattach-cursor cursor prev-line)
		    (clufferend-of-line cursor)))
		 (t
		  (clufferbackward-item cursor)))))
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
	       (cluffersplit-line cursor)
	       (clufferinsert-item cursor item))))
    (loop repeat count
	  do (insert-one-item))))

(defun delete-item (cursor &optional (count 1))
  (flet ((delete-one-item ()
	   (cond ((clufferend-of-buffer-p cursor)
		  (error 'clufferend-of-buffer))
		 ((clufferend-of-line-p cursor)
		  (clufferjoin-line cursor))
		 (t
		  (clufferdelete-item cursor)))))
    (loop repeat count
	  do (delete-one-item))))

(defun erase-item (cursor &optional (count 1))
  (flet ((erase-one-item ()
	   (if (clufferbeginning-of-buffer-p cursor)
	       (error 'clufferbeginning-of-buffer)
	       (progn (backward-item cursor)
		      (delete-item cursor)))))
    (loop repeat count
	  do (erase-one-item))))

(defun item-before-cursor (cursor)
  (cond ((clufferbeginning-of-buffer-p cursor)
	 (error 'clufferend-of-buffer))
	((clufferbeginning-of-line-p cursor)
	 #\Newline)
	(t
	 (clufferitem-before-cursor cursor))))

(defun item-after-cursor (cursor)
  (cond ((clufferend-of-buffer-p cursor)
	 (error 'clufferend-of-buffer))
	((clufferend-of-line-p cursor)
	 #\Newline)
	(t
	 (clufferitem-after-cursor cursor))))

(defun buffer-from-stream (stream)
  (let* ((buffer (cluffermake-empty-buffer))
	 (line (clufferfind-line buffer 0))
	 (cursor (cluffermake-right-sticky-cursor)))
    (clufferattach-cursor cursor line)
    (loop for char = (read-char stream nil nil)
	  until (null char)
	  do (insert-item cursor char))
    (clufferdetach-cursor cursor)
    buffer))

    
  
