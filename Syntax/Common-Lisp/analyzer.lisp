(cl:in-package #:climacs-syntax-common-lisp)

(defclass parse-result ()
  ((%start-line :initarg :start-line :accessor start-line)
   (%end-line :initarg :end-line :accessor end-line)
   (%start-column :initarg :start-column :accessor start-column)
   (%end-column :initarg :end-column :accessor end-column)
   (%children :initarg :children :accessor children)
   (%expression :initarg expression :accessor expression)))

(defclass analyzer ()
  ((%prefix :initform '() :accessor prefix)
   (%suffix :initform '() :accessor suffix)
   (%residue :initform '() :accessor residue)
   (%worklist :initform '() :accessor worklist)
   ;; This slot contains a flexichain of the lines in the buffer.
   (%lines :initform (make-instance 'flexichain:standard-flexichain)
	   :reader lines)
   ;; This slot contains the counter that is maintained during the
   ;; execution of the update function.
   (%line-count :initform 0 :accessor line-count)
   ;; This slot is set to TRUE as soon as the first modification,
   ;; insertion, or deletion is encountered during the execution of
   ;; the update function.
   (%update-p :initform nil :accessor update-p)))

(defun suffix-to-prefix (analyzer)
  (with-accessors ((prefix prefix)
		   (suffix suffix))
      analyzer
    (assert (not (null suffix)))
    (let* ((first-cons-cell suffix)
	   (first-parse-result (first first-cons-cell)))
      (setf suffix (rest suffix))
      (unless (null suffix)
	;; Convert start-line of new first element of suffix
	;; to absolute line number by adding the absolute line
	;; number of the element we are removing.
	(incf (start-line (first suffix))
	      (start-line first-parse-result)))
      ;; Reuse the CONS cell rather than allocating a new one.
      (setf (rest first-cons-cell) prefix)
      (setf prefix first-cons-cell))))

(defun prefix-to-suffix (analyzer)
  (with-accessors ((prefix prefix)
		   (suffix suffix))
      analyzer
    (assert (not (null prefix)))
    (let* ((first-cons-cell prefix)
	   (first-parse-result (first first-cons-cell)))
      (setf prefix (rest prefix))
      (unless (null suffix)
	;; Convert start-line of current first element of suffix to
	;; relative line number by subtracting the absolute line
	;; number of the element we are adding.
	(decf (start-line (first suffix))
	      (start-line first-parse-result)))
      ;; Reuse the CONS cell rather than allocating a new one.
      (setf (rest first-cons-cell) suffix)
      (setf suffix first-cons-cell))))

(defun pop-from-worklist (analyzer)
  (with-accessors ((worklist worklist)) analyzer
    (assert (not (null worklist)))
    (let ((result (pop (first worklist))))
      (if (null (first worklist))
	  (pop worklist)
	  (incf (start-line (first (first worklist)))
		(start-line result)))
      result)))

(defun move-to-residue (analyzer)
  (push (pop-from-worklist analyzer)
	(residue analyzer)))

(defun finish-analysis (analyzer)
  (loop until (null (worklist analyzer))
	do (move-to-residue analyzer))
  (setf (residue analyzer)
	(nreverse (residue analyzer)))
  (setf (update-p analyzer) nil))

(defun pop-from-suffix (analyzer)
  (with-accessors ((suffix suffix)) analyzer
    (assert (not (null suffix)))
    (let ((result (pop suffix)))
      (unless (null suffix)
	(incf (start-line (first suffix))
	      (start-line result)))
      result)))

;;; This function is called by the three operations that handle
;;; modifications.  The first time this function is called, we must
;;; position the prefix and the suffix according to the number of
;;; lines initially skipped.
(defun ensure-update-initialized (analyzer)
  (unless (update-p analyzer)
    (setf (update-p analyzer) t)
    ;; As long as there are parse results on the prefix that
    ;; completely succeed the number of skipped lines, move them to
    ;; the suffix.
    (loop while (and (not (null (prefix analyzer)))
		     (>= (start-line (first (prefix analyzer)))
			 (line-count analyzer)))
	  do (prefix-to-suffix analyzer))
    ;; As long as there are parse results on the suffix that do not
    ;; completely succeed the number of skipped lines, move them to
    ;; the prefix.
    (loop while (and (not (null (suffix analyzer)))
		     (< (start-line (first (suffix analyzer)))
			(line-count analyzer)))
	  do (suffix-to-prefix analyzer))))

(defun handle-skip (analyzer count)
  (incf (line-count analyzer) count))

;;; Return true if and only if either there are no more parse results,
;;; or the first parse result starts at a line that is strictly
;;; greater than LINE-NUMBER.
(defun next-parse-result-is-beyond-line-p (analyzer line-number)
  (with-accessors ((suffix suffix) (worklist worklist)) analyzer
    (or (and (null worklist)
	     (or (null suffix)
		 (> (start-line (first suffix)) line-number)))
	(> (start-line (first (first worklist))) line-number))))

;;; Return true if and only if LINE-NUMBER is one of the lines of
;;; PARSE-RESULT.  The START-LINE of PARSE-RESULT is an absolute line
;;; number.
(defun line-is-inside-parse-result-p (parse-result line-number)
  (<= (start-line parse-result)
      line-number
      (+ (start-line parse-result) (end-line parse-result))))

;;; Add INCREMENT to the absolute line number of every parse result
;;; that is first on ever list in the worklist, and of the first parse
;;; result of the suffix, if any.
(defun adjust-worklist-and-suffix (analyzer increment)
  (loop for parse-results in (worklist analyzer)
	do (incf (start-line (first parse-results)) increment))
  (unless (null (suffix analyzer))
    (incf (start-line (first (suffix analyzer))) increment)))

;;; If the worklist is empty then move a parse result from the suffix
;;; to the worklist (in that case, it is known that the suffix is not
;;; empty).
(defun ensure-worklist-not-empty (analyzer)
  (with-accessors ((worklist worklist)) analyzer
    (when (null worklist)
      (push (list (pop-from-suffix analyzer))
	    worklist))))

(defun process-next-parse-result (analyzer line-number)
  (ensure-worklist-not-empty analyzer)
  (let ((parse-result (pop-from-worklist analyzer)))
    (if (line-is-inside-parse-result-p parse-result line-number)
	(let ((children (children parse-result)))
	  (unless (null children)
	    (incf (start-line (first children))
		  (start-line parse-result))
	    (push children (worklist analyzer))))
	(push parse-result (residue analyzer)))))

(defun handle-modified-line (analyzer line-number)
  (loop until (next-parse-result-is-beyond-line-p analyzer line-number)
	do (process-next-parse-result analyzer line-number)))

(defun handle-inserted-line (analyzer line-number)
  (loop until (next-parse-result-is-beyond-line-p analyzer (1- line-number))
	do (process-next-parse-result analyzer line-number))
  (adjust-worklist-and-suffix analyzer 1))

(defun handle-deleted-line (analyzer line-number)
  (loop until (next-parse-result-is-beyond-line-p analyzer line-number)
	do (process-next-parse-result analyzer line-number))
  (adjust-worklist-and-suffix analyzer -1))
