(defpackage #:climacs-analyzer-fundamental
  (:use #:common-lisp)
  (:export
   ))

(in-package #:climacs-analyzer-fundamental)

(defclass thing ()
  ((%buffer :initarg :buffer :reader buffer)
   (%time-stamp :initform -1 :accessor time-stamp)
   (%paragraphs :initform (list nil) :reader paragraphs)
   (%remaining :initform nil :accessor remaining)
   (%relative-line-number :initform nil :accessor relative-line-number)
   ;; The zone that holds all the paragraphs.
   (%zone :initform (clim3:vbox*) :reader zone)
   ;; This flag is true when a paragraph has been inserted or deleted.
   (%dirty-p :initform t :accessor dirty-p)))

(defun make-thing (buffer)
  (make-instance 'thing :buffer buffer))

(defclass paragraph ()
  ((%lines :initarg :lines :initform '() :accessor lines)
   (%line-count :initarg :line-count :accessor line-count)
   ;; The zone that holds all the lines of the paragraph.
   (%zone :initform (clim3:vbox*) :reader zone)
   ;; This flag is true when a line has been inserted or deleted.
   (%dirty-p :initform t :accessor dirty-p)))

(defclass line ()
  ((%buffer-line :initarg :buffer-line :reader buffer-line)
   ;; The zone that holds all the characters of the line.
   (%zone :initform (clim3:vbox*) :reader zone)))

(defun compute-line-zones (line)
  (setf (clim3:children (zone line))
	(clim3:hbox*
	 (clim3-text:text (climacs-buffer:items (buffer-line line))
			  (clim3:text-style :free :fixed :roman 12)
			  (clim3:make-color 0.0 0.0 0.0))
	 (clim3:sponge))))

(defun delete-current-line (thing)
  (let ((paragraph (cadr (remaining thing)))
	(line-number (relative-line-number thing)))
    (if (= (line-count paragraph) 1)
	;; We are deleting the only line in a paragraph, so the
	;; paragraph must be deleted as well.
	(progn
	  (if (or (null (car (remaining thing)))
		  (null (cddr (remaining thing))))
	      ;; The paragraph to delete is either the first one or
	      ;; the last one in the buffer.  Just remove it.
	      (pop (cdr (remaining thing)))
	      ;; The paragraph to delete is surrounded by other
	      ;; paragraphs.  We need to merge the surrounding
	      ;; paragraphs.
	      (let ((next (caddr (remaining thing))))
		(setf (lines paragraph)
		      (append (lines paragraph) (lines next)))
		(incf (line-count paragraph) (line-count next))
		(pop (cddr (remaining thing)))
		;; Since we added lines to the current paragraph, we
		;; must mark it as dirty.
		(setf (dirty-p paragraph) t)))
	  ;; Since we have deleted a paragraph, we must indicate that
	  ;; the thing is now dirty.
	  (setf (dirty-p thing) t))
	;; There there is more than one line in the current paragraph.
	;; This is the easy case, because we can just delete the line
	;; and keep the paragraph.
	(progn 
	  (setf (lines paragraph)
		(append (subseq (lines paragraph) 0 line-number)
			(subseq (lines paragraph) (1+ line-number))))
	  (decf (line-count paragraph))
	  ;; Since we deleted a line from the current paragraph, we
	  ;; must mark it as dirty.
	  (setf (dirty-p paragraph) t)))))
	   
(defun blank-line-p (buffer-line)
  (every (lambda (x) (member x '(#\Space #\Tab)))
	 (climacs-buffer:items buffer-line)))

(defun same-line-type (line1 line2)
  (let ((b1 (blank-line-p line1))
	(b2 (blank-line-p line2)))
    (or (and b1 b2) (not (or b1 b2)))))

(defun insert-line (thing buffer-line)
  (let* ((paragraph (cadr (remaining thing)))
	 (lines (if (null paragraph) nil (lines paragraph)))
	 (line-number (relative-line-number thing))
	 (new-line (make-instance 'line :buffer-line buffer-line)))
    (if (equal (remaining thing) '(nil))
	;; There are no paragraphs.  This situation can only happen
	;; when we update for the first time.
	(progn (push (make-instance 'paragraph
		       :lines (list new-line)
		       :line-count 1)
		     (cdr (remaining thing)))
	       (pop (remaining thing))
	       ;; Since we have added a paragraph, we must indicate
	       ;; that the thing is now dirty.
	       (setf (dirty-p thing) t))
	(cond ((null paragraph)
	       ;; We are positioned at the very end, so there is no
	       ;; current paragraph.
	       (let ((prev (car (remaining thing))))
		 (if (same-line-type buffer-line
				     (buffer-line (car (lines prev))))
		     ;; We insert the line at the end of the last
		     ;; paragraph.
		     (progn
		       (setf (lines prev)
			     (append (lines prev) (list new-line)))
		       (incf (line-count prev))
		       ;; Since we added lines to the last paragraph,
		       ;; we must mark it as dirty.
		       (setf (dirty-p prev) t))
		     ;; We must add a new paragraph
		     (let ((new (make-instance 'paragraph
				  :lines (list new-line)
				  :line-count 1)))
		       (push new (cdr (remaining thing)))
		       (pop (remaining thing))
		       ;; Since we have added a paragraph, we must
		       ;; indicate that the thing is now dirty.
		       (setf (dirty-p thing) t)))))
	      ((same-line-type buffer-line
			       (buffer-line (car (lines paragraph))))
	       ;; Either the line to be inserted contains only
	       ;; whitespace, and the current paragraph contains only
	       ;; such lines as well, or the line to be inserted
	       ;; contains some other characters, and so do the lines
	       ;; of the current paragraph.  In either case, the new
	       ;; line belongs to the current paragraph, so we just
	       ;; insert it in its right place.
	       (setf (lines paragraph)
		     (append (subseq lines 0 line-number)
			     (list new-line)
			     (subseq lines line-number)))
	       (incf (line-count paragraph))
	       (incf (relative-line-number thing))
	       ;; Since we added lines to the current paragraph, we
	       ;; must mark it as dirty.
	       (setf (dirty-p paragraph) t))
	      ((zerop line-number)
	       ;; In this case, the line does not belong to the
	       ;; current paragraph, and it belongs to the paragraph
	       ;; immediately preceding the current one, if it exists.
	       ;; If it does not exist, we must create one.
	       (if (null (car (remaining thing)))
		   ;; The current paragraph is the first one, so we
		   ;; must create a new paragraph to hold the new
		   ;; line.
		   (let ((new (make-instance 'paragraph
				:lines (list new-line)
				:line-count 1)))
		     (push new (cdr (remaining thing)))
		     (pop (remaining thing))
		     ;; Since we have added a paragraph, we must
		     ;; indicate that the thing is now dirty.
		     (setf (dirty-p thing) t))
		   ;; There is a paragraph preceding the current
		   ;; one, and we insert the new line last in
		   ;; that paragraph.
		   (let ((prev (car (remaining thing))))
		     (setf (lines prev)
			   (append (lines prev) (list new-line)))
		     (incf (line-count prev))
		     ;; Since we added lines to the paragraph, we must
		     ;; mark it as dirty.
		     (setf (dirty-p prev) t))))
	      (t
	       ;; In this situation, the new line does not belong to
	       ;; the current paragraph, but it should be inserted
	       ;; between two existing lines in the current paragraph.
	       ;; We therefore need to split the current paragraph
	       ;; into two parts, and create a new paragraph holding
	       ;; the new line that will sit between the two parts.
	       (let ((prefix (subseq (lines paragraph) 0 line-number)))
		 (setf (lines paragraph)
		       (subseq (lines paragraph) line-number))
		 (decf (line-count paragraph) line-number)
		 ;; Since we removed lines to the current paragraph,
		 ;; we must mark it as dirty.
		 (setf (dirty-p paragraph) t)
		 (push (make-instance 'paragraph
			 :lines prefix
			 :line-count line-number)
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (push (make-instance 'paragraph
			 :lines (list new-line)
			 :line-count 1)
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (setf (relative-line-number thing) 0)
		 ;; Since we have added paragraphs, we must indicate
		 ;; that the thing is now dirty.
		 (setf (dirty-p thing) t)))))))

(defun modify-current-line (thing)
  (let* ((paragraph (cadr (remaining thing)))
	 (line-number (relative-line-number thing))
	 (line (elt (lines paragraph) line-number)))
    (if (= (line-count paragraph) 1)
	;; We must check wheter the modified line has been changed in
	;; such a way that this paragraph changed from being one with
	;; all lines containing only whitespace, to one with all lines
	;; containing text, or vice versa.
	(if (null (car (remaining thing)))
	    ;; The current paragraph is the first one in the buffer.
	    (if (null (cddr (remaining thing)))
		;; The current paragraph is the only one in the
		;; buffer, so no matter what type paragraph it is,
		;; just keep it as it is.
		(progn
		  ;; Set the current line to the end. 
		  (pop (remaining thing))
		  (setf (relative-line-number thing) 0))
		;; The current paragraph is the first in the buffer,
		;; and there is at least one more paragraph in the
		;; buffer.  We must check whether the line now should
		;; be added to the second paragraph instead.
		(let ((next (caddr (remaining thing))))
		  (if (same-line-type (buffer-line line)
				      (buffer-line (first (lines next))))
		      ;; The first paragraph should disappear and the
		      ;; only line that it contained should be added
		      ;; to the beginning of the second paragraph.
		      (progn
			(push line (lines next))
			(incf (line-count next))
			;; Since we added a line to the next
			;; paragraph, we must mark it as dirty.
			(setf (dirty-p next) t)
			(pop (cdr (remaining thing)))
			;; Since we have deleted a paragraph, we must
			;; indicate that the thing is now dirty.
			(setf (dirty-p thing) t))
		      ;; The modified line has a different type from
		      ;; the lines of the following paragraph, so we
		      ;; keep the current paragraph intact.
		      (progn
			;; Advance the current line to the first one
			;; of the next paragraph.
			(pop (remaining thing))
			(setf (relative-line-number thing) 0)))))
	    ;; The current paragraph is not the first one in the
	    ;; buffer.
	    (if (null (cddr (remaining thing)))
		;; The current paragraph is the last one in the
		;; buffer, but there is at least one paragraph
		;; preceding it.  We must check wheter the line now
		;; should be added to the preceding paragraph instead.
		(let ((prev (car (remaining thing))))
		  (if (same-line-type (buffer-line line)
				      (buffer-line (first (lines prev))))
		      ;; The last paragraph should disappear and the
		      ;; only line that it contained should be added
		      ;; to the end of the next-to-last paragraph.
		      (progn
			(setf (lines prev)
			      (append (lines prev) (list line)))
			(incf (line-count prev))
			;; Since we added lines to the next-to-last
			;; paragraph, we must mark it as dirty.
			(setf (dirty-p prev) t)
			(pop (cdr (remaining thing)))
			;; Since we have deleted a paragraph, we must
			;; indicate that the thing is now dirty.
			(setf (dirty-p thing) t))
		      ;; The modified line has a different type from
		      ;; the lines in the preceding paragraph, so we
		      ;; keep the current paragraph intact.
		      (progn
			;; Advance the current line to the first one
			;; of the next paragraph.
			(pop (remaining thing))
			(setf (relative-line-number thing) 0))))))
	;; The current paragraph has more than one line.  Therefore it
	;; will not disappear as a result of one of the lines being
	;; modified.
	(cond ((same-line-type (buffer-line line)
			       (buffer-line
				(elt (lines paragraph)
				     (if (zerop line-number)
					 1
					 0))))
	       ;; The line still belongs in the current paragraph, so
	       ;; for now just advance the current line.  Later, we
	       ;; update the zones to be displayed.
	       (incf (relative-line-number thing))
	       (when (= (relative-line-number thing)
			(line-count paragraph))
		 (pop (remaining thing))
		 (setf (relative-line-number thing) 0)))
	      ((zerop line-number)
	       ;; The line no longer belongs in the current paragraph
	       ;; in which it is the first line.
	       (if (null (car (remaining thing)))
		   ;; The current paragraph is the first one in the
		   ;; buffer.  We must therefore create a new
		   ;; paragraph to hold the modified line.
		   (let ((new (make-instance 'paragraph
				:lines (list line)
				:line-count 1)))
		     ;; Remove the line from the current paragraph.
		     (pop (lines paragraph))
		     (decf (line-count paragraph))
		     ;; Since we removed a line from the current
		     ;; paragraph, we must mark it as dirty.
		     (setf (dirty-p paragraph) t)
		     (push new (cdr (remaining thing)))
		     ;; Make the next paragraph the current one.
		     (pop (remaining thing))
		     ;; Since we have added a paragraph, we must indicate that
		     ;; the thing is now dirty.
		     (setf (dirty-p thing) t))
		   ;; There is a paragraph preceding the current one.
		   ;; Move the modified line to it.
		   (let ((prev (car (remaining thing))))
		     ;; Remove the line from the current paragraph.
		     (pop (lines paragraph))
		     (decf (line-count paragraph))
		     ;; Since we removed a line from the current
		     ;; paragraph, we must mark it as dirty.
		     (setf (dirty-p paragraph) t)
		     ;; Insert it at the end of the preceding
		     ;; paragraph.
		     (setf (lines prev)
			   (append (lines prev) (list line)))
		     (incf (line-count prev))
		     ;; Since we added a line to the previous
		     ;; paragraph, we must mark it as dirty.
		     (setf (dirty-p prev) t))))
	      ((= line-number (1- (line-count paragraph)))
	       ;; The line no longer belongs in the current paragraph
	       ;; in which it is the last line.
	       (if (null (cddr (remaining thing)))
		   ;; The current paragraph is the last one in the
		   ;; buffer.  We must therefore create a new
		   ;; paragraph to hold the modified line.
		   (let ((new (make-instance 'paragraph
				  :lines (list line)
				  :line-count 1)))
		     ;; Remove the line from the current paragraph.
		     (setf (lines paragraph) (butlast (lines paragraph)))
		     (decf (line-count paragraph))
		     ;; Since we deleted a line from the current
		     ;; paragraph, we must mark it as dirty.
		     (setf (dirty-p paragraph) t)
		     ;; Add the paragraph.
		     (push new (cddr (remaining thing)))
		     ;; Since we have added a paragraph, we must
		     ;; indicate that the thing is now dirty.
		     (setf (dirty-p thing) t)
		     ;; Advance to the end.
		     (pop (remaining thing))
		     (pop (remaining thing))
		     (setf (relative-line-number thing) 0))
		   ;; There is a paragraph following the current
		   ;; one.  Move the modified line to it.
		   (let ((next (caddr (remaining thing))))
		     ;; Remove the line from the current paragraph.
		     (setf (lines paragraph) (butlast (lines paragraph)))
		     (decf (line-count paragraph))
		     ;; Since we removed a line from the current
		     ;; paragraph, we must mark it as dirty.
		     (setf (dirty-p paragraph) t)
		     ;; Insert it at the beginning of the following
		     ;; paragraph.
		     (push line (lines next))
		     (incf (line-count next))
		     ;; Since we added a line to the next paragraph,
		     ;; we must mark it as dirty.
		     (setf (dirty-p next) t)
		     ;; Advance the current line.
		     (pop (remaining thing))
		     (setf (relative-line-number thing) 1))))
	      (t
	       ;; The line no longer belongs in the current paragraph,
	       ;; but it is neither the first nor the last line of
	       ;; that paragraph.  We must therefore split the current
	       ;; paragraph into two parts, and add a new paragraph
	       ;; between the two parts.
	       (let ((prefix (subseq (lines paragraph) 0 line-number)))
		 (setf (lines paragraph)
		       (subseq (lines paragraph) (1+ line-number)))
		 (decf (line-count paragraph) (1+ line-number))
		 ;; Since we removed lines to the current paragraph,
		 ;; we must mark it as dirty.
		 (setf (dirty-p paragraph) t)
		 (push (make-instance 'paragraph
			 :lines prefix
			 :line-count line-number)
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (push (make-instance 'paragraph
			 :lines (list line)
			 :line-count 1)
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (setf (relative-line-number thing) 0)
		 ;; Since we have added paragraphs, we must indicate
		 ;; that the thing is now dirty.
		 (setf (dirty-p thing) t)))))))
		 
(defun update (thing)
  ;; Initialize REMAINING and RELATIVE-LINE-NUMBER to prepare for the
  ;; update.
  (setf (remaining thing) (paragraphs thing))
  (setf (relative-line-number thing) 0)
  ;; Clear all the dirty flags.
  (loop for paragraph in (paragraphs thing)
	do (setf (dirty-p paragraph) nil)
	   (loop for line in (lines paragraph)
		 do (setf (dirty-p line) nil)))
  ;; Do the update.
  (flet ((sync (line)
	   (break)
	   (loop until (eq (buffer-line
			    (elt (lines (cadr (remaining thing)))
				 (relative-line-number thing)))
			   line)
		 do (delete-current-line thing))
	   (break)))
    (flet ((skip (n)
	     (break)
	     (loop while (plusp n)
		   for paragraph = (cadr (remaining thing))
		   for rest-count = (- (line-count paragraph)
				       (relative-line-number thing))
		   do (if (>= n rest-count)
			  (progn (decf n rest-count)
				 (pop (cdr (remaining thing)))
				 (setf (relative-line-number thing) 0))
			  (progn (incf (relative-line-number n))
				 (setf n 0))))
	     (break))
	   (modify (line)
	     (break)
	     (sync line)
	     (modify-current-line thing)
	     (break))
	   (create (line)
	     (break)
	     (insert-line thing line)
	     (break)))
      (climacs-buffer:update (buffer thing)
			     (time-stamp thing)
			     #'sync #'skip #'modify #'create)))
  ;; Record the time at which this update was made.
  (setf (time-stamp thing)
	(climacs-buffer:current-time (buffer thing)))
  ;; Update the children of paragraphs with the dirty flag set.
  (loop for paragraph in (paragraphs thing)
	do (when (dirty-p paragraph)
	     (setf (clim3:children (zone paragraph))
		   (mapcar #'zone (lines paragraph)))))
  ;; If the dirty flag is set for the entire thing, then update
  ;; the children of the thing as well.
  (when (dirty-p thing)
    (setf (clim3:children (zone thing))
	  (mapcar #'zone (paragraphs thing)))))
