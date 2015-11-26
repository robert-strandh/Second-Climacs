(cl:in-package #:test-buffer)

(defun test-line-insert/delete-by-position ()
  (let ((line (funcall climacs-buffer:*empty-line-constructor*))
	(mirror '())
	(item 10000))
    (flet ((test ()
	     (let ((insertp (zerop (random 2))))
	       (if insertp
		   (let ((position (random (1+ (length mirror))))
			 (item (incf item)))
		     (climacs-line::insert-item-at-position line item position)
		     (setf mirror
			   (append (subseq mirror 0 position)
				   (list item)
				   (subseq mirror position))))
		   (unless (null mirror)
		     (let ((position (random (length mirror))))
		       (climacs-line::delete-item-at-position line position)
		       (setf mirror
			     (append (subseq mirror 0 position)
				     (subseq mirror (1+ position)))))))
	       (assert (equalp (climacs-buffer:items line)
			       (coerce mirror 'vector))))))
      (loop repeat 1000000
	    do (test)))))
