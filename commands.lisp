(defpackage #:climacs-commands
  (:use #:common-lisp)
  (:export
   #:emacs-style-command-processor
   #:*command-mappings*
   #:*ascii-insert-mappings*
   #:*latin-1-insert-mappings*
   #:*point*
   ;; FIXME: remove this later when we create command the processors
   ;; based on the type of the view.
   #:make-fundamental-command-processor
   ))

(in-package #:climacs-commands)

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

(clim3:define-command no-match (keystrokes)
  (format *error-output*
	  "No match for keystrokes: ~s~%"
	  keystrokes))

(clim3:define-command forward-item (&optional (count 1))
  (climacs-basic-emacs:forward-item (point) count))

(clim3:define-command backward-item (&optional (count 1))
  (climacs-basic-emacs:backward-item (point) count))

(clim3:define-command insert-character
    ((character character) &optional (count 1))
  (climacs-basic-emacs:insert-item (point) character count))

(clim3:define-command delete-item (&optional (count 1))
  (climacs-basic-emacs:delete-item (point) count))

(clim3:define-command erase-item (&optional (count 1))
  (climacs-basic-emacs:erase-item (point) count))

(clim3:define-command quit ()
  (throw :quit nil))

;;; FIXME: make several groups
(defparameter *command-mappings*
  '((((#\x :control) (#\c :control))
     (quit))
    (((#\f :control))
     (forward-item :opt-num))
    (((#\b :control))
     (backward-item :opt-num))
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
