(cl:in-package #:climacs-syntax-common-lisp)

(defclass error-token ()
  ((%name :initarg :name :reader name)))

(defclass error-object ()
  ())

(defvar *parent-entry*)

(defun cached-parse-result (analyzer-stream)
  (let* ((analyzer (analyzer analyzer-stream))
	 (residue (residue analyzer))
	 (suffix (suffix analyzer)))
    (cond ((not (null residue))
	   (if (and (= (start-line (first residue))
		       (current-line analyzer-stream))
		    (= (start-column (first residue))
		       (current-column analyzer-stream)))
	       (first residue)
	       nil))
	  ((not (null suffix))
	   (if (and (= (start-line (first suffix))
		       (current-line analyzer-stream))
		    (= (start-column (first suffix))
		       (current-column analyzer-stream)))
	       (first suffix)
	       nil))
	  (t nil))))

(defun advance-stream-to-beyond-parse-result (analyzer-stream parse-result)
  (setf (current-line analyzer-stream)
	(+ (start-line parse-result)
	   (end-line parse-result)))
  (setf (current-column analyzer-stream)
	(end-column parse-result))
  (read-char input-stream nil nil))

;;; FIXME: handle characters with invalid consituent traits.
(defun read-common (&optional
		      (input-stream *standard-input*)
		      (eof-error-p t)
		      (eof-value nil))
  (let ((token (make-array 100
			   :element-type 'character
			   :adjustable t
			   :fill-pointer 0))
	(token-escapes (make-array 100
				   :adjustable t
				   :fill-pointer 0))
	(*backquote-allowed-p* *backquote-in-subforms-allowed-p*)
	(*backquote-in-subforms-allowed-p* nil)
	(current-entry nil))
    (tagbody
     step-1-start
       (let ((char (read-char input-stream nil nil)))
	 (cond ((null char)
		(if eof-error-p
		    (handler-case
			(throw 'end-of-list nil)
		      (control-error ()
			(error 'end-of-file :stream input-stream)))
		    (return-from read-common eof-value)))
	       ((eq (syntax-type char) :whitespace)
		(go step-1-start))
	       (t
		(unread-char char input-stream)
		(let ((result (cached-parse-result input-stream)))
		  (if (not (null result))
		      (progn
			(advance-stream-to-beyond-parse-result
			 input-stream result)
			(push entry (children *parent-entry*))
			(if (null (value entry))
			    (go step-1-start)
			    (return-from read-common (car (value entry)))))
		      (progn (setf current-entry
				   (make-instance 'entry
				     :start (clone-position input-stream)
				     :end (clone-position input-stream)))
			     (push current-entry (children *parent-entry*))
			     (forward input-stream))))))
	 (ecase (syntax-type char)
	   ((:terminating-macro :non-terminating-macro)
	    (let* ((*parent-entry* current-entry)
		   (values (multiple-value-list
			    (handler-case
				(funcall (get-macro-character char)
					 input-stream
					 char)
			      (condition ()
				(make-instance 'error-object))))))
	      (clobber-position (end current-entry) input-stream)
	      (setf (value current-entry) values)
	      (add-entry current-entry)
	      (if (null values)
		  (go step-1-start)
		  (return-from read-common (car values)))))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(clobber-position (end current-entry) input-stream)
		(add-entry current-entry)
		(let ((value (make-instance 'error-token :name token)))
		  (setf (value current-entry) (list value))
		  (return-from read-common value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-8-even-escapes)))
	   (:multiple-escape
	    (go step-9-odd-escapes))
	   (:constituent
	    (vector-push-extend char token)
	    (vector-push-extend nil token-escapes)
	    (go step-8-even-escapes))))
     step-8-even-escapes
       (let ((char (read-char input-stream nil nil)))
	 (when (null char)
	   (go step-10-terminate-token))
	 (ecase (syntax-type char)
	   ((:constituent :non-terminating-macro)
	    (vector-push-extend char token)
	    (vector-push-extend nil token-escapes)
	    (go step-8-even-escapes))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(clobber-position (end current-entry) input-stream)
		(add-entry current-entry)
		(let ((value (make-instance 'error-token :name token)))
		  (setf (value current-entry) (list value))
		  (return-from read-common value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-8-even-escapes)))
	   (:multiple-escape
	    (go step-9-odd-escapes))
	   (:terminating-macro
	    (unread-char char input-stream)
	    (go step-10-terminate-token))
	   (:whitespace
	    (when *preserve-whitespace*
	      (unread-char char input-stream))
	    (go step-10-terminate-token))))
     step-9-odd-escapes
       (let ((char (read-char input-stream nil nil)))
	 (when (null char)
	   (clobber-position (end current-entry) input-stream)
	   (add-entry current-entry)
	   (let ((value (make-instance 'error-token :name token)))
	     (setf (value current-entry) (list value))
	     (return-from read-common value)))
	 (ecase (syntax-type char)
	   ((:constituent :terminating-macro
	     :non-terminating-macro :whitespace)
	    (vector-push-extend char token)
	    (vector-push-extend t token-escapes)
	    (go step-9-odd-escapes))
	   (:single-escape
	    (let ((char (read-char input-stream nil nil)))
	      (when (null char)
		(clobber-position (end current-entry) input-stream)
		(add-entry current-entry)
		(let ((value (make-instance 'error-token :name token)))
		  (setf (value current-entry) (list value))
		  (return-from read-common value)))
	      (vector-push-extend char token)
	      (vector-push-extend t token-escapes)
	      (go step-9-odd-escapes)))
	   (:multiple-escape
	    (go step-8-even-escapes))))
     step-10-terminate-token
       (clobber-position (end current-entry) input-stream)
       (add-entry current-entry)
       (let ((value (if *read-suppress*
			nil
			(interpret-token current-entry
					 token
					 token-escapes
					 input-stream))))
	 (setf (value current-entry) (list value))
	 (return-from read-common value)))))
    
(defun custom-read (&optional
		      (input-stream *standard-input*)
		      (eof-error-p t)
		      (eof-value nil)
		      (recursive-p nil))
  (let ((*preserve-whitespace* recursive-p))
    (let ((value (read-common input-stream eof-error-p eof-value)))
      value)))

(defun read-from-tree (tree)
  (let ((readtable (copy-readtable)))
    (init-readtable readtable)
    (let* ((*readtable* readtable)
	   (stream (make-stream tree))
	   (*parent-entry* (make-instance 'entry
			     :start (clone-position stream)
			     :end (clone-position stream))))
      (loop for expression = (custom-read stream nil 'eof nil)
	    until (eq expression 'eof)))))
