(cl:in-package #:climacs-syntax-common-lisp)

;;; A folio stream is a stream that uses a folio as a source for the
;;; items to return as a result of reading.
(defclass folio-stream ()
  ((%folio :initarg :folio :reader folio)
   (%current-line-number :initform 0 :accessor current-line-number)
   (%current-item-number :initform 0 :accessor current-item-number)))

(defgeneric next-position (folio line-number item-number))

(defmethod next-position ((folio folio) line-number item-number)
  (if (= (line-length folio line-number) item-number)
      (values (1+ line-number) 0)
      (values line-number (1+ item-number))))

(defgeneric previous-position (folio line-number item-number))

(defmethod previous-position ((folio folio) line-number item-number)
  (if (zerop item-number)
      (values 0 (line-length folio (1- line-number)))
      (values line-number (1- item-number))))

(defgeneric eof-p (folio-stream))

(defmethod eof-p ((stream folio-stream))
  (let* ((folio (folio stream))
	 (last-line-number (1- (line-count (folio stream))))
	 (last-line-length (line-length folio last-line-number)))
    (and (= (current-line-number stream) last-line-number)
	 (= (current-item-number stream) last-line-length))))

(defgeneric forward (folio-stream))

(defmethod forward ((stream folio-stream))
  (with-accessors ((folio folio)
		   (current-line-number current-line-number)
		   (current-item-number current-item-number))
      stream
    (multiple-value-bind (l c)
	(next-position folio current-line-number current-item-number)
      (setf current-line-number l)
      (setf current-item-number c))))

(defgeneric backward (folio-stream))

(defmethod backward ((stream folio-stream))
  (with-accessors ((folio folio)
		   (current-line-number current-line-number)
		   (current-item-number current-item-number))
      stream
    (multiple-value-bind (l c)
	(previous-position folio current-line-number current-item-number)
      (setf current-line-number l)
      (setf current-item-number c))))

(defmethod trivial-gray-streams:stream-read-char ((stream folio-stream))
  (if (eof-p stream)
      :eof
      (with-accessors ((folio folio)
		       (current-line-number current-line-number)
		       (current-item-number current-item-number))
	  stream
	(prog1 (if (= (line-length folio current-line-number)
		      current-item-number)
		   #\Newline
		   (item folio current-line-number current-item-number))
	  (forward stream)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream folio-stream) char)
  (declare (ignore char))
  (backward stream))

;;; Reader customization.

(defvar *stack*)

(defun skip-whitespace (stream)
  (loop for char = (read-char stream nil nil)
	while (member char '(#\Space #\Tab #\Newline))
	finally (unread-char char stream)))

(defmethod sicl-reader:read-common :around
    ((input-stream folio-stream) eof-error-p eof-value)
  (declare (ignore eof-error-p eof-value))
  (skip-whitespace input-stream)
  (let ((*stack* (cons '() *stack*))
	(start-line (current-line-number input-stream))
	(start-column (current-item-number input-stream)))
    (let ((result (call-next-method)))
      (push (make-instance 'expression-parse-result
	      :expression result
	      :children (nreverse (first *stack*))
	      :start-line start-line
	      :start-column start-column
	      :end-line (current-line-number input-stream)
	      :end-column (current-item-number input-stream))
	    (second *stack*))
      result)))

(defmethod sicl-reader:call-reader-macro :around
    (function (input-stream folio-stream) char)
  (let ((start-line (current-line-number input-stream))
	(start-column (current-item-number input-stream)))
    (let ((result (multiple-value-list (call-next-method))))
      (when (null result)
	(push (make-instance 'no-expression-parse-result
		:children (nreverse (first *stack*))
		:start-line start-line
		:start-column start-column
		:end-line (current-line-number input-stream)
		:end-column (current-item-number input-stream))
	      (second *stack*)))
      (apply #'values result))))
