(defpackage #:climacs-commands
  (:use #:common-lisp))

(in-package #:climacs-commands)

(defclass emacs-style-command-processor ()
  ((%mappings :initarg :mappings :accessor mappings)
   (%numeric-argument :initform nil :accessor numeric-argument)
   (%keystrokes-so-far :initform '() :accessor keystrokes-so-far)
   (%state :initform :start :accessor state)))

(defgeneric submit-keystroke (key-processor keystroke))

(defun prefix-p (partial-sentence sentence)
  (and (<= (length partial-sentence) (length sentence))
       (every #'equal partial-sentence sentence)))

(defun form-action (template numeric-argument)
  (if (eq (car (last template)) :opt-num)
      (if (null numeric-argument)
	  (butlast template)
	  (append (butlast template) (list numeric-argument)))
      (substitute numeric-argument :num template)))

(defmethod submit-keystroke
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

(defclass climacs-view (clim3:view)
  ((%command-table
    :initarg :command-table
    :accessor command-table)
   (%command-key-processor
    :initform (make-instance 'emacs-style-command-processor)
    :initarg :command-key-processor
    :accessor command-key-processor)
   (%cursor :initarg cursor :reader cursor)
   (%buffer :initarg :buffer :reader buffer)))

(defclass climacs (clim3:application)
  ((%current-view
    :initarg :current-view
    :accessor clim3:current-view)
   (%views :initarg :views :accessor views)))

(defun point ()
  (cursor (current-view clim3:*application*)))

(clim3:define-command forward-item (&optional (count 1))
  (climacs-basic-emacs:forward-item (point) count))

(clim3:define-command backward-item (&optional (count 1))
  (climacs-basic-emacs:backward-item (point) count))

(clim3:define-command insert-character
    ((character character) &optional (count 1))
  (climacs-basic-emacs:insert-item character (point) count))

(clim3:define-command delete-item (&optional (count 1))
  (climacs-basic-emacs:delete-item (point) count))

(clim3:define-command erase-item (&optional (count 1))
  (climacs-basic-emacs:erase-item (point) count))

;;; FIXME: make several groups
(defparameter *mappings*
  '((((#\x :control) (#\c :control))
     (quit))
    (((#\f :control))
     (forward-item :opt-num))
    (((#\b :control))
     (backward-item :opt-num))
    (((#\x :control) (#\f :control))
     (find-file))
    (((#\x :control) (#\i))
     (insert-file))

;;; We assume ASCII encoding
(defparameter *ascii-insert-mappings*
  (loop for code from #x20 to #x7e
	for char = (code-char code)
	collect `(((,char)) (insert-char ,char :opt-num))))

;;; We assume Unicode encoding.
(defparameter *latin-1-insert-mappings*
  (loop for code from #xa0 to #xff
	for char = (code-char code)
	collect `(((,char)) (insert-char ,char :opt-num))))
  
