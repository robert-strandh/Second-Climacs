(cl:in-package #:climacs-syntax-common-lisp)

(defclass analyzer-stream ()
  ((%analyzer :initarg :analyzer :reader analyzer)
   (%current-line :initform 0 :accessor current-line)
   (%current-column :initform 0 :accessor current-column)))

(defun eof-p (analyzer-stream)
  (let* ((lines (lines (analyzer analyzer-stream)))
	 (count (flexichain:nb-elements lines)))
    (and (= (current-line analyzer-stream) (1- count))
	 (= (current-column analyzer-stream)
	    (let ((last-line (flexichain:element* lines (1- count))))
	      (length (contents last-line)))))))
