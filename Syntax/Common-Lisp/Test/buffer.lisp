(cl:in-package #:climacs-syntax-common-lisp-test)

(defclass line ()
  ((%items :initarg :items :accessor items)))

(defclass buffer ()
  ((%lines :initarg :lines :accessor lines)))

(defmethod cluffer:items ((line line) &key &allow-other-keys)
  (items line))

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

(defmethod cluffer:update ((buffer buffer) time sync skip modify create)
  (let* ((line-count (length (lines buffer)))
	 (modified-line-number (random line-count)))
    (unless (zerop modified-line-number)
      (funcall skip modified-line-number))
    (funcall modify (nth modified-line-number (lines buffer)))
    (unless (= modified-line-number (1- line-count))
      (funcall sync (nth (1+ modified-line-number) (lines buffer))))
    (unless (= modified-line-number (- line-count 2))
      (funcall skip (- line-count modified-line-number 2)))))

(defun analyzer-from-buffer (buffer)
  (let* ((analyzer (make-instance 'climacs-syntax-common-lisp::analyzer
		     :buffer buffer))
	 (chain (climacs-syntax-common-lisp::lines analyzer)))
    (loop for position from 0
	  for test-line in (lines buffer)
	  for analyzer-line = (make-instance 'climacs-syntax-common-lisp::line
				:contents (items test-line)
				:cluffer-line test-line)
	  do (flexichain:insert* chain position analyzer-line))
    analyzer))
