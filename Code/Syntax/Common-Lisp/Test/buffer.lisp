(cl:in-package #:second-climacs-syntax-common-lisp-test)

(defun buffer-from-string (string)
  (let* ((line (make-instance 'cluffer-standard-line:open-line))
	 (buffer (make-instance 'cluffer-standard-buffer:buffer
		   :initial-line line)))
    (loop with line-number = 0
	  with item-number = 0
	  for char across string
	  for line = (cluffer:find-line buffer line-number)
	  do (if (eql char #\Newline)
		 (progn (cluffer:split-line-at-position line item-number)
			(setf item-number 0)
			(incf line-number))
		 (progn (cluffer:insert-item-at-position line char item-number)
			(incf item-number))))
    buffer))

(defun analyzer-from-buffer (buffer)
  (let ((cache (make-instance 'cl-syntax::cache)))
    (make-instance 'cl-syntax::analyzer
      :buffer buffer
      :folio cache)))

(defun forms-from-cache (cache)
  (append (reverse (mapcar #'cl-syntax::expression
			   (cl-syntax::prefix cache)))
	  (mapcar #'cl-syntax::expression
		  (cl-syntax::suffix cache))))

;;; Mark a buffer line as modified without really modifying it by
;;; inserting an item in the first position and then immediately
;;; deleting that item again.
(defun mark-buffer-line-as-modified (buffer-line)
  (cluffer:insert-item-at-position buffer-line #\Space 0)
  (cluffer:delete-item-at-position buffer-line 0))

;;; Given a buffer, return a random line in that buffer.
(defun random-buffer-line (buffer)
  (let* ((line-count (cluffer:line-count buffer))
	 (line-number (random line-count)))
    (cluffer:find-line buffer line-number)))
