(cl:in-package #:climacs-syntax-common-lisp)

;;; We do not use the line object from Cluffer directly, because the
;;; contents of such a line may change after we have asked for it, so
;;; that we get a different contents each time we ask for it.  But we
;;; still need the line object from Cluffer, because that one is used
;;; as a comparison in the update protocol.  The solution is to store
;;; both the Cluffer line object and the contents as it was when we
;;; asked for it.
(defclass line ()
  ((%cluffer-line :initarg :cluffer-line :reader cluffer-line)
   (%contents :initarg :contents :accessor contents)))

(defclass analyzer (folio)
  ((%prefix :initform '() :accessor prefix)
   (%suffix :initform '() :accessor suffix)
   (%residue :initform '() :accessor residue)
   (%worklist :initform '() :accessor worklist)
   ;; This slot contains the buffer that is being analyzed.
   (%buffer :initarg :buffer :accessor buffer)
   ;; This slot contains a flexichain of the lines in the buffer.  A
   ;; line is represented by an instance of the LINE class defined
   ;; above.
   (%lines :initform (make-instance 'flexichain:standard-flexichain)
	   :reader lines)
   ;; The time stamp passed to and returned by the Cluffer update
   ;; protocol.
   (%time-stamp :initform nil :accessor time-stamp)
   ;; This slot contains the counter that is maintained during the
   ;; execution of the update function.
   (%line-counter :initform 0 :accessor line-counter)))

(defun pop-from-suffix (analyzer)
  (with-accessors ((suffix suffix)) analyzer
    (assert (not (null suffix)))
    (let ((result (pop suffix)))
      (unless (null suffix)
	(relative-to-absolute (first suffix) (start-line result)))
      result)))

(defun push-to-suffix (analyzer parse-result)
  (assert (not (relative-p parse-result)))
  (with-accessors ((suffix suffix))
      analyzer
    (unless (null suffix)
      (absolute-to-relative (first suffix) (start-line parse-result)))
    (push parse-result suffix)))

(defun suffix-to-prefix (analyzer)
  (push (pop-from-suffix analyzer) (prefix analyzer)))

(defun prefix-to-suffix (analyzer)
  (assert (not (null (prefix analyzer))))
  (push-to-suffix analyzer (pop (prefix analyzer))))

;;; Take a list of parse results with a relative position, and turn
;;; each parse result in the list into one with an absolution position
;;; according to the value of BASE.
(defun make-absolute (relative-parse-results base)
  (loop with offset = base
	for rest on relative-parse-results
	for relative-start = (start-line (first rest))
	do (setf offset (+ offset relative-start))
	   (setf (start-line (first rest)) offset)))

(defun move-to-residue (analyzer)
  (push (pop (worklist analyzer))
	(residue analyzer)))

(defun finish-analysis (analyzer)
  (loop until (null (worklist analyzer))
	do (move-to-residue analyzer))
  (setf (residue analyzer)
	(nreverse (residue analyzer))))

;;; This function is called by the three operations that handle
;;; modifications.  The first time this function is called, we must
;;; position the prefix and the suffix according to the number of
;;; lines initially skipped.
(defun ensure-update-initialized (analyzer)
  ;; As long as there are parse results on the prefix that do not
  ;; completely precede the number of skipped lines, move them to the
  ;; suffix.
  (loop while (and (not (null (prefix analyzer)))
		   (>= (end-line (first (prefix analyzer)))
		       (line-counter analyzer)))
	do (prefix-to-suffix analyzer))
  ;; As long as there are parse results on the suffix that completely
  ;; precede the number of skipped lines, move them to the prefix.
  (loop while (and (not (null (suffix analyzer)))
		   (< (end-line (first (suffix analyzer)))
		      (line-counter analyzer)))
	do (suffix-to-prefix analyzer)))

;;; Return true if and only if either there are no more parse results,
;;; or the first parse result starts at a line that is strictly
;;; greater than LINE-NUMBER.
(defun next-parse-result-is-beyond-line-p (analyzer line-number)
  (with-accessors ((suffix suffix) (worklist worklist)) analyzer
    (if (null worklist)
	(or (null suffix)
	    (> (start-line (first suffix)) line-number))
	(> (start-line (first worklist)) line-number))))

;;; Return true if and only if LINE-NUMBER is one of the lines of
;;; PARSE-RESULT.  The START-LINE of PARSE-RESULT is an absolute line
;;; number.
(defun line-is-inside-parse-result-p (parse-result line-number)
  (<= (start-line parse-result)
      line-number
      (+ (start-line parse-result) (end-line parse-result))))

;;; Add INCREMENT to the absolute line number of every parse result on
;;; the worklist, and of the first parse result of the suffix, if any.
(defun adjust-worklist-and-suffix (analyzer increment)
  (loop for parse-result in (worklist analyzer)
	do (incf (start-line parse-result) increment))
  (unless (null (suffix analyzer))
    (incf (start-line (first (suffix analyzer))) increment)))

;;; If the worklist is empty then move a parse result from the suffix
;;; to the worklist (in that case, it is known that the suffix is not
;;; empty).
(defun ensure-worklist-not-empty (analyzer)
  (with-accessors ((worklist worklist)) analyzer
    (when (null worklist)
      (push (pop-from-suffix analyzer)
	    worklist))))

(defun process-next-parse-result (analyzer line-number)
  (with-accessors ((worklist worklist)) analyzer
    (ensure-worklist-not-empty analyzer)
    (let ((parse-result (pop worklist)))
      (if (line-is-inside-parse-result-p parse-result line-number)
	  (let ((children (children parse-result)))
	    (make-absolute children (start-line parse-result))
	    (setf worklist (append children worklist)))
	  (push parse-result (residue analyzer))))))

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

;;; Take into account modifications to the buffer by destroying the
;;; parts of the cache that are no longer valid, while keeping parse
;;; results that are not affected by such modifications.
(defun scavenge (analyzer)
  (let ((analyzer-initialized-p nil))
    (with-accessors ((lines lines)
		     (line-counter line-counter))
	analyzer
      (setf line-counter 0)
      (flet ((remove-deleted-lines (line)
	       (loop for analyzer-line = (flexichain:element* lines line-counter)
		     for cluffer-line = (cluffer-line analyzer-line)
		     until (eq line cluffer-line)
		     do (flexichain:delete* lines line-counter)
			(handle-deleted-line analyzer line-counter)))
	     (ensure-analyzer-initialized ()
	       (unless analyzer-initialized-p
		 (setf analyzer-initialized-p t)
		 (ensure-update-initialized analyzer))))
	(flet ((skip (count)
		 (incf line-counter count))
	       (modify (line)
		 (ensure-analyzer-initialized)
		 (remove-deleted-lines line)
		 (handle-modified-line analyzer line-counter)
		 (incf line-counter))
	       (create (line)
		 (ensure-analyzer-initialized)
		 (let ((temp (make-instance 'line
			       :cluffer-line line
			       :contents (cluffer:items line))))
		   (flexichain:insert* lines line-counter temp))
		 (handle-inserted-line analyzer line-counter)
		 (incf line-counter))
	       (sync (line)
		 (remove-deleted-lines line)
		 (incf line-counter)))
	  (setf (time-stamp analyzer)
		(cluffer:update (buffer analyzer)
				(time-stamp analyzer)
				#'sync #'skip #'modify #'create)))))))

;;; Methods that make an instance of ANALYZER behave like an instance
;;; of FOLIO.

(defmethod line-count ((folio analyzer))
  (flexichain:nb-elements (lines folio)))

(defmethod line-length ((folio analyzer) line-number)
  (length (contents (flexichain:element* (lines folio) line-number))))

(defmethod item ((folio analyzer) line-number item-number)
  (aref (contents (flexichain:element* (lines folio) line-number))
	item-number))
