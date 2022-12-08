(cl:in-package #:climacs-command)

;;; A COMMAND consists of two parts:
;;;
;;;   * A FUNCTION that is called to execute some code that may modify
;;;     the model or the view.  This function can either be called
;;;     directly as a normal Common Lisp function, or through the
;;;     process of COMMAND INVOCATION.
;;;
;;;   * A list of TYPE NAMES, one for each required parameter of the
;;;     function.
;;;
;;; When a command is invoked, the arguments are first acquired
;;; according to the types.  The function is then called with those
;;; arguments.
;;;
;;; In order to keep the two parts of a command together, we define a
;;; class COMMAND to be a subclass of STANDARD-GENERIC-FUNCTION.  That
;;; way, we can add new slots for keeping track of the list of type
;;; names and perhaps of other information as well.

(defclass command (standard-generic-function)
  ((%types :initarg :types :reader types))
  (:metaclass #.(class-name (class-of (find-class 'standard-generic-function)))))

(defun split-lambda-list (lambda-list)
  (let* ((position-or-nil (position '&optional lambda-list))
	 (position (if (null position-or-nil)
		       (length lambda-list)
		       position-or-nil)))
    (values (subseq lambda-list 0 position)
	    (subseq lambda-list position))))

(defun extract-lambda-lists (lambda-list)
  (multiple-value-bind (required optional)
      (split-lambda-list lambda-list)
    (let ((ordinary-required
	    (loop for req in required
		  collect (if (symbolp req) req (first req))))
	  (gf-optionals
	    (if (null optional)
		'()
		(cons '&optional
		      (loop for opt in (rest optional)
			    collect (if (symbolp opt) opt (first opt)))))))
      (values (loop for req in required
		    collect (if (symbolp req) t (second req)))
	      (append ordinary-required gf-optionals)
	      (append ordinary-required optional)))))

(defmacro define-command (name lambda-list &body body)
  (multiple-value-bind (types gf-lambda-list method-lambda-list)
      (extract-lambda-lists lambda-list)
    `(progn (defgeneric ,name ,gf-lambda-list
	      (:method ,method-lambda-list ,@body)
	      (:generic-function-class command))
	    (reinitialize-instance #',name
				   :types ',types))))

(defgeneric command-pretty-name (command-name language)
  (:method (command-name language) nil))
