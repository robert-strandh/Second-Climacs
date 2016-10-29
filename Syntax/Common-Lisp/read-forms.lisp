(cl:in-package #:climacs-syntax-common-lisp)

(defun read-forms (analyzer-stream)
  (loop with analyzer = (analyzer analyzer-stream)
	do (if (or (null (suffix analyzer))
		   (and (= (current-line analyzer-stream)
			   (start-line (first (suffix analyzer))))
			(= (current-column analyzer-stream)
			   (start-column (first (suffix analyzer))))))
	       ;; The suffix is either empty or consists of a list of
	       ;; top-level parse results.
	       (return-from read-forms nil)
	       (let ((next (parse analyzer-stream)))
		 (loop until (or (null (residue analyzer))
				 (> (start-line (first (residue analyzer)))
				    (current-line analyzer-stream))
				 (and (= (start-line (first (residue analyzer)))
					 (current-line analyzer-stream))
				      (> (start-column (first (residue analyzer)))
					 (current-column analyzer-stream))))
		       do (pop-from-residue analyzer))
		 (when (null (residue analyzer))
		   (loop until (or (null (suffix analyzer))
				   (> (start-line (first (suffix analyzer)))
				      (current-line analyzer-stream))
				   (and (= (start-line (first (suffix analyzer)))
					   (current-line analyzer-stream))
					(> (start-column (first (suffix analyzer)))
					   (current-column analyzer-stream))))
			 do (pop-from-suffix analyzer)))))))
		   
