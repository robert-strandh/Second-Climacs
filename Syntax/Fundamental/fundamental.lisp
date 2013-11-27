(defpackage #:climacs-analyzer-fundamental
  (:use #:common-lisp)
  (:shadow fresh-line)
  (:export
   #:paragraphs
   #:lines
   #:buffer-line
   #:buffer
   #:fundamental-analyzer
   ;; FIXME: replace this with a generic function that dispatches
   ;; on the type of the analyzer.
   #:update
   ))

(in-package #:climacs-analyzer-fundamental)

(defgeneric current-time (clock))

(defclass clock ()
  ((%current-time :initform 0 :accessor current-time)))

(defun make-clock ()
  (make-instance 'clock))

(defun new-time (clock)
  (incf (current-time clock)))

(defclass fundamental-analyzer ()
  (;; The buffer being analyzed.
   (%buffer :initarg :buffer :reader buffer)
   ;; This slot contains the current time of the buffer as it was when
   ;; it was last analyzed.  Only if the current time of the buffer is
   ;; greater than the contents of this slot is an update required.
   (%buffer-time :initform -1 :accessor buffer-time)
   ;; A list of paragraphs resulting from the analysis.
   (%paragraphs :initform '() :accessor paragraphs)
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
   ;; This slot contains some suffix of the list of paragraphs.  The
   ;; paragraph considered the "current" pagraph, is the second
   ;; element (the CADR) of the remaining list.  This way we can
   ;; access the paragraph immediately preceding the current one as
   ;; the fist element of the list and we can delete the current
   ;; paragraph by modifying the CDR of the remaining list.
   (%remaining :initform nil :accessor remaining)
   ;; The relative line number is the line element number of the
   ;; current line in the list of lines of the current paragraph.  
   (%relative-line-number :initform nil :accessor relative-line-number)))

(defun make-fundamental-analyzer (buffer)
  (make-instance 'fundamental-analyzer :buffer buffer))

;;; When the analysis starts, this variable is bound to the result of
;;; calling NEW-TIME on the clock of the analyzer.  Any creation or
;;; modification during the analysis should be considered has having
;;; taken place at the time indicated by this variable.
(defvar *current-time*)

(defclass time-mixin ()
  (;; The analyzer time at which this item was created.
   (%create-time :initform *current-time* :reader create-time)
   ;; The analyzer time at which this item was last modified.
   (%modify-time :initform *current-time* :accessor modify-time)
   ;; This slot is set to true when the item is about to be deleted.
   ;; The purpose is to inform client code that holds on to references
   ;; to this item that it should be descarded.
   (%deleted-p :initform nil :accessor deleted-p)))
  
(defclass paragraph (time-mixin)
  (;; A simple list of all the lines of this paragraph.
   (%lines :initarg :lines :initform '() :accessor lines)
   ;; The length of the list in the LINES slot.  This slot exists only
   ;; for efficiency reasons so that we don't have to traverse the
   ;; list of lines of a paragraph each time we want to know how many
   ;; lines there are.
   (%line-count :accessor line-count)))

(defun touch (thing)
  (setf (modify-time thing) *current-time*))

(defun kill (thing)
  (setf (deleted-p thing) t))

;;; We avoid having to remember to initialize and update the line
;;; count by automatically updating it when the lines are updated.
(defmethod initialize-instance :after
    ((paragraph paragraph) &key &allow-other-keys)
  (setf (line-count paragraph) (length (lines paragraph))))

;;; We also avoid having to rememeber to touch the paragraph manually
;;; when the lines are updated by automatically touching it.
(defmethod (setf lines) :around (new-lines (paragraph paragraph))
  (let ((lines-before (lines paragraph)))
    (call-next-method)
    (let ((lines-after (lines paragraph)))
      ;; Kill every line that existed before but that no longer does.
      (loop for line in (set-difference lines-before lines-after)
	    do (kill line))
      (setf (line-count paragraph) (length lines-after))))
  (touch paragraph))

(defclass line (time-mixin)
  ((%buffer-line :initarg :buffer-line :reader buffer-line)))

(defun fresh-line (line)
  (make-instance 'line :buffer-line (buffer-line line)))

(defun fresh-lines (lines)
  (mapcar #'fresh-line lines))

(defun delete-current-line (thing)
  (let ((paragraph (cadr (remaining thing)))
	(prev (car (remaining thing)))
	(next (caddr (remaining thing)))
	(line-number (relative-line-number thing)))
    (if (= (line-count paragraph) 1)
	;; We are deleting the only line in a paragraph, so the
	;; paragraph must be deleted as well.
	(if (or (null prev) (null next))
	    ;; The paragraph to delete is either the first one or
	    ;; the last one in the buffer.  Just remove it.
	    (progn (kill paragraph)
		   (pop (cdr (remaining thing))))
	    ;; The paragraph to delete is surrounded by other
	    ;; paragraphs.  We need to merge the surrounding
	    ;; paragraphs.  We do this by adding to the preceding
	    ;; paragraph fresh copies of the lines in the following
	    ;; paragraph, and then deleting both the current and the
	    ;; following paragraph.
	    (progn (setf (lines prev)
			 (append (lines prev)
				 (fresh-lines (lines next))))
		   ;; Delete the current and the following paragraphs.
		   (kill paragraph)
		   (kill next)
		   (pop (cdr (remaining thing)))
		   (pop (cdr (remaining thing)))))
	;; There there is more than one line in the current paragraph.
	;; This is the easy case, because we can just delete the line
	;; and keep the paragraph.
	(setf (lines paragraph)
	      (append (subseq (lines paragraph) 0 line-number)
		      (subseq (lines paragraph) (1+ line-number)))))))
	   
(defun blank-line-p (buffer-line)
  (every (lambda (x) (member x '(#\Space #\Tab)))
	 (climacs-buffer:items buffer-line)))

(defun same-line-type (line1 line2)
  (let ((b1 (blank-line-p line1))
	(b2 (blank-line-p line2)))
    (or (and b1 b2) (not (or b1 b2)))))

(defun insert-line (thing buffer-line)
  (let* ((paragraph (cadr (remaining thing)))
	 (prev (car (remaining thing)))
	 (lines (if (null paragraph) nil (lines paragraph)))
	 (line-number (relative-line-number thing))
	 (new-line (make-instance 'line :buffer-line buffer-line)))
    (if (equal (remaining thing) '(nil))
	;; There are no paragraphs.  This situation can only happen
	;; when we update for the first time.  We insert a new
	;; paragraph and then position ourselves at the very end.
	(progn (push (make-instance 'paragraph :lines (list new-line))
		     (cdr (remaining thing)))
	       (pop (remaining thing)))
	(cond ((null paragraph)
	       ;; We are positioned at the very end, so there is no
	       ;; current paragraph.
	       (if (same-line-type buffer-line
				   (buffer-line (car (lines prev))))
		   ;; We insert the line at the end of the last
		   ;; paragraph, and we keep the current position.
		   (setf (lines prev)
			 (append (lines prev) (list new-line)))
		   ;; We must add a new paragraph, and position
		   ;; ourselves at the following paragraph.
		   (progn
		     (push (make-instance 'paragraph :lines (list new-line))
			   (cdr (remaining thing)))
		     (pop (remaining thing)))))
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
	       ;; Advance our relative position to the next line.
	       (incf (relative-line-number thing)))
	      ((zerop line-number)
	       ;; In this case, the line does not belong to the
	       ;; current paragraph, and it belongs to the paragraph
	       ;; immediately preceding the current one, if it exists.
	       ;; If it does not exist, we must create one.
	       (if (null prev)
		   ;; The current paragraph is the first one, so we
		   ;; must create a new paragraph to hold the new
		   ;; line.
		   (progn
		     (push (make-instance 'paragraph :lines (list new-line))
			   (cdr (remaining thing)))
		     ;; We want to keep the position that we had.
		     (pop (remaining thing)))
		   ;; There is a paragraph preceding the current one,
		   ;; and we insert the new line last in that
		   ;; paragraph.
		   (setf (lines prev)
			 (append (lines prev) (list new-line)))))
	      (t
	       ;; In this situation, the new line does not belong to
	       ;; the current paragraph, but it should be inserted
	       ;; between two existing lines in the current paragraph.
	       ;; We therefore need to split the current paragraph
	       ;; into two parts, and create a new paragraph holding
	       ;; the new line that will sit between the two parts.
	       ;; We keep the original paragraph to hold the prefix,
	       ;; and we create new paragraphs to hold the new line
	       ;; and the suffix.
	       (let ((prefix (subseq (lines paragraph) 0 line-number))
		     (suffix (subseq (lines paragraph) line-number)))
		 (setf (lines paragraph) prefix)
		 (pop (remaining thing))
		 (push (make-instance 'paragraph :lines (list new-line))
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (push (make-instance 'paragraph :lines (fresh-lines suffix))
		       (cdr (remaining thing)))
		 (setf (relative-line-number thing) 0)))))))

(defun modify-current-line (thing)
  (let* ((paragraph (cadr (remaining thing)))
	 (prev (car (remaining thing)))
	 (next (caddr (remaining thing)))
	 (line-number (relative-line-number thing))
	 (line (elt (lines paragraph) line-number)))
    (if (= (line-count paragraph) 1)
	;; We must check wheter the modified line has been changed in
	;; such a way that this paragraph changed from being one with
	;; all lines containing only whitespace, to one with all lines
	;; containing text, or vice versa.
	(if (null prev)
	    ;; The current paragraph is the first one in the buffer.
	    (if (null next)
		;; The current paragraph is the only one in the
		;; buffer, so no matter what type paragraph it is,
		;; just keep it as it is.
		(progn
		  (touch line)
		  (touch paragraph)
		  ;; Set the position to the very end.
		  (pop (remaining thing))
		  (setf (relative-line-number thing) 0))
		;; The current paragraph is the first in the buffer,
		;; and there is at least one more paragraph in the
		;; buffer.  We must check whether the line now should
		;; be added to the second paragraph instead.
		(if (same-line-type (buffer-line line)
				    (buffer-line (first (lines next))))
		    ;; The first paragraph should disappear and the
		    ;; only line that it contained should be added
		    ;; to the beginning of the second paragraph.
		    (progn
		      (push (fresh-line line) (lines next))
		      (kill paragraph)
		      (pop (cdr (remaining thing))))
		    ;; The modified line has a different type from
		    ;; the lines of the following paragraph, so we
		    ;; keep the current paragraph intact.
		    (progn
		      (touch line)
		      (touch paragraph)
		      ;; Advance the current line to the first one
		      ;; of the next paragraph.
		      (pop (remaining thing))
		      (setf (relative-line-number thing) 0))))
	    ;; The current paragraph is not the first one in the
	    ;; buffer.
	    (if (null next)
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
			(kill paragraph)
			(pop (cdr (remaining thing))))
		      ;; The modified line has a different type from
		      ;; the lines in the preceding paragraph, so we
		      ;; keep the current paragraph intact.
		      (progn
			(touch line)
			(touch paragraph)
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
	       ;; The line still belongs in the current paragraph.
	       ;; Just mark it as modified.
	       (touch line)
	       (touch paragraph)
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
				:lines (list (fresh-line line)))))
		     ;; Remove the line from the current paragraph.
		     (pop (lines paragraph))
		     (push new (cdr (remaining thing)))
		     ;; Make the next paragraph the current one.
		     (pop (remaining thing)))
		   ;; There is a paragraph preceding the current one.
		   ;; Move the modified line to it.
		   (progn
		     ;; Remove the line from the current paragraph.
		     (pop (lines paragraph))
		     ;; Insert it at the end of the preceding
		     ;; paragraph.
		     (setf (lines prev)
			   (append (lines prev)
				   (list (fresh-line line)))))))
	      ((= line-number (1- (line-count paragraph)))
	       ;; The line no longer belongs in the current paragraph
	       ;; in which it is the last line.
	       (if (null (cddr (remaining thing)))
		   ;; The current paragraph is the last one in the
		   ;; buffer.  We must therefore create a new
		   ;; paragraph to hold the modified line.
		   (let ((new (make-instance 'paragraph
				  :lines (list (fresh-line line)))))
		     ;; Remove the line from the current paragraph.
		     (setf (lines paragraph) (butlast (lines paragraph)))
		     ;; Add the paragraph.
		     (push new (cddr (remaining thing)))
		     ;; Advance to the end.
		     (pop (remaining thing))
		     (pop (remaining thing))
		     (setf (relative-line-number thing) 0))
		   ;; There is a paragraph following the current
		   ;; one.  Move the modified line to it.
		   (progn
		     ;; Remove the line from the current paragraph.
		     (setf (lines paragraph) (butlast (lines paragraph)))
		     ;; Insert it at the beginning of the following
		     ;; paragraph.
		     (push (fresh-line line) (lines next))
		     ;; Advance the current line.
		     (pop (remaining thing))
		     (setf (relative-line-number thing) 1))))
	      (t
	       ;; In this situation, the modified line no longer
	       ;; belongs to the current paragraph, but it is neither
	       ;; the first nor the last line of that paragraph.  We
	       ;; therefore need to split the current paragraph into
	       ;; two parts, and create a new paragraph holding the
	       ;; modified line that will sit between the two parts.
	       ;; We keep the original paragraph to hold the prefix,
	       ;; and we create new paragraphs to hold the modified
	       ;; line and the suffix.
	       (let ((prefix (subseq (lines paragraph) 0 line-number))
		     (suffix (subseq (lines paragraph) (1+ line-number))))
		 (setf (lines paragraph) prefix)
		 (pop (remaining thing))
		 (push (make-instance 'paragraph
			 :lines (list (fresh-line line)))
		       (cdr (remaining thing)))
		 (pop (remaining thing))
		 (push (make-instance 'paragraph :lines (fresh-lines suffix))
		       (cdr (remaining thing)))
		 (setf (relative-line-number thing) 0)))))))
		 
(defun update (analyzer)
  ;; Initialize REMAINING and RELATIVE-LINE-NUMBER to prepare for the
  ;; update.
  (setf (relative-line-number analyzer) 0)
  (let ((*current-time* (current-time (clock analyzer)))
	(remaining (cons nil (paragraphs analyzer))))
    (setf (remaining analyzer) remaining)
    ;; Do the update.
    (flet ((sync (line)
	     (loop until (eq (buffer-line
			      (elt (lines (cadr (remaining analyzer)))
				   (relative-line-number analyzer)))
			     line)
		   do (delete-current-line analyzer))))
      (flet ((skip (n)
	       (loop while (plusp n)
		     for paragraph = (cadr (remaining analyzer))
		     for rest-count = (- (line-count paragraph)
					 (relative-line-number analyzer))
		     do (if (>= n rest-count)
			    (progn (decf n rest-count)
				   (pop (remaining analyzer))
				   (setf (relative-line-number analyzer) 0))
			    (progn (incf (relative-line-number analyzer) n)
				   (setf n 0)))))
	     (modify (line)
	       (sync line)
	       (modify-current-line analyzer))
	     (create (line)
	       (insert-line analyzer line)))
	(climacs-buffer:update (buffer analyzer)
			       (buffer-time analyzer)
			       #'sync #'skip #'modify #'create)))
    (setf (paragraphs analyzer) (cdr remaining)))
  ;; Record the time at which this update was made.
  (setf (buffer-time analyzer)
	(climacs-buffer:current-time (buffer analyzer))))
