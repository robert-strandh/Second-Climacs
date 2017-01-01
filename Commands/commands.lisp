(cl:in-package #:climacs-commands)

(defclass emacs-style-command-processor
    (clim3:command-table)
  ((%mappings :initarg :mappings :accessor mappings)
   (%numeric-argument :initform nil :accessor numeric-argument)
   (%keystrokes-so-far :initform '() :accessor keystrokes-so-far)
   (%state :initform :start :accessor state)))

(defmethod clim3:command-name-in-table-p
    (command-name (command-table emacs-style-command-processor))
  (member command-name (mappings command-table)
	  :test #'eq :key #'cadr))

(defun prefix-p (partial-sentence sentence)
  (and (<= (length partial-sentence) (length sentence))
       (every #'equal partial-sentence sentence)))

(defun form-action (template numeric-argument)
  (if (eq (car (last template)) :opt-num)
      (if (null numeric-argument)
	  (butlast template)
	  (append (butlast template) (list numeric-argument)))
      (substitute numeric-argument :num template)))

(defmethod clim3:submit-keystroke
    ((key-processor emacs-style-command-processor) keystroke)
  (with-accessors ((mappings mappings)
		   (numeric-argument numeric-argument)
		   (keystrokes-so-far keystrokes-so-far)
		   (state state))
      key-processor
    (cond ((equal keystroke '(#\g :control))
	   (setf numeric-argument nil)
	   (setf keystrokes-so-far '())
	   (setf state :start)
	   (throw :accept '(clim3:abort)))
	  ((and (eq state :start)
		(equal keystroke '(#\u :control)))
	   (setf numeric-argument 4)
	   (setf state :numeric-argument-prefix))
	  ((and (eq state :numeric-argument-prefix)
		(equal keystroke '(#\u :control)))
	   (setf numeric-argument (* numeric-argument 4)))
	  ((and (eq state :numeric-argument-prefix)
		(member keystroke
			'((#\0) (#\1) (#\2) (#\3) (#\4)
			  (#\5) (#\6) (#\7) (#\8) (#\9))
			:test #'equal))
	   (setf numeric-argument
		 (- (char-code (car keystroke)) (char-code #\0)))
	   (setf state :numeric-argument-digits))
	  ((and (eq state :numeric-argument-digits)
		(member keystroke
			'((#\0) (#\1) (#\2) (#\3) (#\4)
			  (#\5) (#\6) (#\7) (#\8) (#\9))
			:test #'equal))
	   (setf numeric-argument
		 (+ (* numeric-argument 10)
		    (- (char-code (car keystroke)) (char-code #\0)))))
	  (t
	   (setf state :command)
	   (setf keystrokes-so-far
		 (append keystrokes-so-far (list keystroke)))
	   (let ((entries (remove-if-not
			   (lambda (entry)
			     (prefix-p keystrokes-so-far (car entry)))
			   mappings)))
	     (cond  ((null entries)
		     (let ((temp keystrokes-so-far))
		       (setf numeric-argument nil)
		       (setf keystrokes-so-far nil)
		       (setf state :start)
		       (throw :accept `(no-match ,temp))))
		    ((equal keystrokes-so-far (caar entries))
		     ;; We found a perfect match.
		     (let ((temp numeric-argument))
		       (setf numeric-argument nil)
		       (setf keystrokes-so-far nil)
		       (setf state :start)
		       (throw :accept
			 (form-action (cadar entries) temp))))
		    (t
		     nil)))))))

(defvar *point*)

(defun point ()
  *point*)

(defun message (control-string &rest arguments)
  (apply #'format *error-output* control-string arguments)
  (terpri *error-output*))

(clim3:define-command no-match (keystrokes)
  (message "No match for keystrokes: ~s" keystrokes))

(clim3:define-command forward-item (&optional (count 1))
  (loop repeat count
	do (cluffer-emacs:forward-item (point))))

(clim3:define-command backward-item (&optional (count 1))
  (loop repeat count
	do (cluffer-emacs:backward-item (point))))

(clim3:define-command insert-character
    ((character character) &optional (count 1))
  (loop repeat count
	do (cluffer-emacs:insert-item (point) character)))

(clim3:define-command delete-item (&optional (count 1))
  (loop repeat count
	do (cluffer-emacs:delete-item (point))))

(clim3:define-command erase-item (&optional (count 1))
  (loop repeat count
	do (cluffer-emacs:erase-item (point))))

(clim3:define-command quit ()
  (throw :quit nil))

(clim3:define-command beginning-of-line ()
  (cluffer:beginning-of-line (point)))

(clim3:define-command end-of-line ()
  (cluffer:end-of-line (point)))

;;; This command currently does not do what the equivalent Emacs
;;; command does.  This one currently preserves the horizontal
;;; position as defined by the number of items to the left of the
;;; cursor in the line.  Emacs, on the other hand, works on lines as
;;; they are displayed, so that if a logical line is displayed as
;;; several physical lines, then the Emacs command might stay within
;;; the logical line and move to the next physical line.  In addition,
;;; Emacs preserves the horizontal position as defined by the number
;;; of pixels to the left of the cursor in the line, so that if the
;;; font is proportional, the number of character to the left of the
;;; cursor may be quite different before and after executing the
;;; command.
;;;
;;; Furthermore, the equivalent Emacs command "remembers" the original
;;; column so that if this command is repeated, it tries to position
;;; itself in the original colum.  We don't do that yet. 
(clim3:define-command next-line (&optional (count 1))
  (if (minusp count)
      (previous-line count)
      (let* ((cursor (point))
	     (pos (cluffer:cursor-position cursor))
	     (line (cluffer:line cursor))
	     (line-number (cluffer:line-number line))
	     (buffer (cluffer:buffer cursor))
	     (line-count (cluffer:line-count buffer))
	     (new-line-number (min (1- line-count) (+ line-number count))))
	(when (>= (+ line-number count) line-count)
	  (message "End of buffer"))
	(cluffer:detach-cursor cursor)
	(let* ((new-line (cluffer:find-line buffer new-line-number))
	       (line-length (cluffer:item-count new-line))
	       (new-pos (min line-length pos)))
	  (cluffer:attach-cursor cursor new-line new-pos)))))

(clim3:define-command previous-line (&optional (count 1))
  (if (minusp count)
      (next-line (- count))
      (let* ((cursor (point))
	     (pos (cluffer:cursor-position cursor))
	     (line (cluffer:line cursor))
	     (line-number (cluffer:line-number line))
	     (buffer (cluffer:buffer cursor))
	     (new-line-number (max 0 (- line-number count))))
	(when (minusp (- line-number count))
	  (message "Beginning of buffer"))
	(cluffer:detach-cursor cursor)
	(let* ((new-line (cluffer:find-line buffer new-line-number))
	       (line-length (cluffer:item-count new-line))
	       (new-pos (min line-length pos)))
	  (cluffer:attach-cursor cursor new-line new-pos)))))

(clim3:define-command newline (&optional (count 1))
  (loop repeat count
	do (cluffer-emacs:insert-item (point) #\Newline)))

;;; FIXME: make several groups
(defparameter *command-mappings*
  '((((#\x :control) (#\c :control))
     (quit))
    (((#\f :control))
     (forward-item :opt-num))
    (((#\b :control))
     (backward-item :opt-num))
    (((#\d :control))
     (delete-item :opt-num))
    (((#\h :control))
     (erase-item :opt-num))
    (((#\a :control))
     (beginning-of-line))
    (((#\e :control))
     (end-of-line))
    (((#\p :control))
     (previous-line :opt-num))
    (((#\n :control))
     (next-line :opt-num))
    (((#\Return))
     (newline :opt-num))
    (((#\x :control) (#\f :control))
     (find-file))
    (((#\x :control) (#\i))
     (insert-file))))

;;; We assume ASCII encoding
(defparameter *ascii-insert-mappings*
  (loop for code from #x20 to #x7e
	for char = (code-char code)
	collect `(((,char)) (insert-character ,char :opt-num))))

;;; We assume Unicode encoding.
(defparameter *latin-1-insert-mappings*
  (loop for code from #xa0 to #xff
	for char = (code-char code)
	collect `(((,char)) (insert-character ,char :opt-num))))
  
(clim3:define-command inspect-application ()
  (clueless:inspect clim3:*application*))

;;; Various mappings for debugging purposes.
(defparameter *debug-mappings*
  '((((#\i :control :meta))
     (inspect-application))))

;;; FIXME: make a generic function called (say) make-command-processor
;;; that dispatches on the type of the view.  Right now we have only
;;; one type of view, so that problem would have to be addressed
;;; first.
(defun make-fundamental-command-processor ()
  (make-instance 'emacs-style-command-processor
    :mappings (append
	       *command-mappings*
	       *ascii-insert-mappings*
	       *latin-1-insert-mappings*
	       *debug-mappings*)))
