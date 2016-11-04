(cl:in-package #:climacs-syntax-common-lisp)

(defclass analyzer-stream (folio-stream) ())

;;; Return true if and only if position A occurs strictly before
;;; position B in some buffer.
(defun position-less (line-number-a item-number-a line-number-b item-number-b)
  (or (< line-number-a line-number-b)
      (and (= line-number-a line-number-b)
	   (< item-number-a item-number-b))))

;;; Return true if and only if the start position of PARSE-RESULT is
;;; strictly less than the position indicated by LINE-NUMBER and
;;; ITEM-NUMBER.
(defun parse-result-starts-before-position-p
    (parse-result line-number item-number)
  (position-less (start-line parse-result) (start-column parse-result)
		 line-number item-number))

;;; Make sure that the first parse result that we consider recycling
;;; starts at or beyond the current stream position.  Parse results
;;; that start before the current stream position are either no longer
;;; valid, or have been recycled already, so we remove them.  Two
;;; places are considered for recycling, namely the list of residual
;;; parse results and the suffix.
(defun pop-to-stream-position (analyzer-stream)
  (let ((analyzer (folio analyzer-stream))
	(line-number (current-line-number analyzer-stream))
	(item-number (current-item-number analyzer-stream)))
    (loop while (and (not (null (residue analyzer)))
		     (parse-result-starts-before-position-p
		      (first (residue analyzer)) line-number item-number))
	  do (pop (residue analyzer)))
    (when (null (residue analyzer))
      (loop while (and (not (null (suffix analyzer)))
		       (parse-result-starts-before-position-p
			(first (suffix analyzer)) line-number item-number))
	    do (pop-from-suffix analyzer)))))

(defun cached-parse-result (analyzer-stream)
  (let* ((analyzer (folio analyzer-stream))
	 (residue (residue analyzer))
	 (suffix (suffix analyzer)))
    (cond ((not (null residue))
	   (if (and (= (start-line (first residue))
		       (current-line-number analyzer-stream))
		    (= (start-column (first residue))
		       (current-item-number analyzer-stream)))
	       (first residue)
	       nil))
	  ((not (null suffix))
	   (if (and (= (start-line (first suffix))
		       (current-line-number analyzer-stream))
		    (= (start-column (first suffix))
		       (current-item-number analyzer-stream)))
	       (first suffix)
	       nil))
	  (t nil))))

(defun advance-stream-to-beyond-parse-result (analyzer-stream parse-result)
  (setf (current-line-number analyzer-stream)
	(end-line parse-result))
  (setf (current-item-number analyzer-stream)
	(end-column parse-result))
  (read-char analyzer-stream nil nil))

(defmethod sicl-reader:read-common :around
    ((input-stream analyzer-stream) eof-error-p eof-value)
  (let ((parse-result (cached-parse-result input-stream)))
    (if (null parse-result)
	(call-next-method)
	(progn (push parse-result (first *stack*))
	       (advance-stream-to-beyond-parse-result input-stream parse-result)
	       (if (typep parse-result 'expression-parse-result)
		   (expression parse-result)
		   (sicl-reader:read-common input-stream eof-error-p eof-value))))))
