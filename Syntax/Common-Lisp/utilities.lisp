(cl:in-package #:climacs-syntax-common-lisp)

;;; Check that an object is a proper list.  Return true if the object
;;; is a proper list.  Return false if the object is an atom other
;;; than NIL or if the list is dotted or circular.
(defun proper-list-p (object)
  (cond  ((null object) t)
	 ((atom object) nil)
	 (t (let ((slow object)
		  (fast (cdr object)))
	      (declare (type cons slow))
	      (tagbody
	       again
		 (unless (consp fast)
		   (return-from proper-list-p
		     (if (null fast) t nil)))
		 (when (eq fast slow)
		   (return-from proper-list-p nil))
		 (setq fast (cdr fast))
		 (unless (consp fast)
		   (return-from proper-list-p
		     (if (null fast) t nil)))
		 (setq fast (cdr fast))
		 (setq slow (cdr slow))
		 (go again))))))

(defun convert-according-to-readtable-case (token token-escapes)
  (ecase (readtable-case *readtable*)
    (:upcase
     (loop for escape across token-escapes
	   for i from 0
	   do (when (null escape)
		(setf (aref token i)
		      (char-upcase (aref token i))))))
    (:downcase
     (loop for escape across token-escapes
	   for i from 0
	   do (when (null escape)
		(setf (aref token i)
		      (char-downcase (aref token i))))))
    (:preserve
     nil)
    (:invert
     (let ((count-upper-case 0)
	   (count-lower-case 0))
       (loop for escape across token-escapes
	     for char across token
	     do (when (null escape)
		  (cond ((upper-case-p char)
			 (incf count-upper-case))
			((lower-case-p char)
			 (incf count-lower-case))
			(t
			 nil))))
       (cond ((zerop count-upper-case)
	      (loop for escape across token-escapes
		    for i from 0
		    do (when (null escape)
			 (setf (aref token i)
			       (char-upcase (aref token i))))))
	     ((zerop count-lower-case)
	      (loop for escape across token-escapes
		    for i from 0
		    do (when (null escape)
			 (setf (aref token i)
			       (char-downcase (aref token i))))))
	     (t
	      nil))))))
