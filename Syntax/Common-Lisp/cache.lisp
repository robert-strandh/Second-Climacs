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

(defclass cache (flexichain-folio)
  ((%prefix :initform '() :accessor prefix)
   (%suffix :initform '() :accessor suffix)
   (%residue :initform '() :accessor residue)
   (%worklist :initform '() :accessor worklist)
   ;; The time stamp passed to and returned by the Cluffer update
   ;; protocol.
   (%time-stamp :initform nil :accessor time-stamp)
   ;; This slot contains the counter that is maintained during the
   ;; execution of the update function.
   (%line-counter :initform 0 :accessor line-counter)))

(defun pop-from-suffix (cache)
  (with-accessors ((suffix suffix)) cache
    (assert (not (null suffix)))
    (let ((result (pop suffix)))
      (unless (null suffix)
	(relative-to-absolute (first suffix) (start-line result)))
      result)))

(defun push-to-suffix (cache parse-result)
  (assert (not (relative-p parse-result)))
  (with-accessors ((suffix suffix))
      cache
    (if (null suffix)
        (setf (max-line-width-list parse-result)
              (max-line-width parse-result))
        (progn
          (setf (max-line-width-list parse-result)
                (max (max-line-width-list (first suffix))
                     (max-line-width parse-result)))
          (absolute-to-relative (first suffix) (start-line parse-result))))
    (push parse-result suffix)))

(defun pop-from-prefix (cache)
  (pop (prefix cache)))

(defgeneric push-to-prefix (cache parse-result))

(defmethod push-to-prefix ((cache cache) (parse-result parse-result))
  (with-accessors ((prefix prefix))
      cache
    (setf (max-line-width-list parse-result)
          (if (null prefix)
              (max-line-width parse-result)
              (max (max-line-width-list (first prefix))
                   (max-line-width parse-result))))
    (push parse-result prefix)))

(defun pop-from-worklist (cache)
  (pop (worklist cache)))

(defun push-to-worklist (cache parse-result)
  (push parse-result (worklist cache)))

(defun pop-from-residue (cache)
  (pop (residue cache)))

(defun push-to-residue (cache parse-result)
  (push parse-result (residue cache)))

(defun suffix-to-prefix (cache)
  (push-to-prefix cache (pop-from-suffix cache)))

(defun prefix-to-suffix (cache)
  (assert (not (null (prefix cache))))
  (push-to-suffix cache (pop-from-prefix cache)))

(defun move-to-residue (cache)
  (push-to-residue cache (pop-from-worklist cache)))

(defun finish-scavenge (cache)
  (loop until (null (worklist cache))
	do (move-to-residue cache))
  (setf (residue cache)
	(nreverse (residue cache))))

;;; This function is called by the three operations that handle
;;; modifications.  The first time this function is called, we must
;;; position the prefix and the suffix according to the number of
;;; lines initially skipped.
(defun ensure-update-initialized (cache)
  ;; As long as there are parse results on the prefix that do not
  ;; completely precede the number of skipped lines, move them to the
  ;; suffix.
  (loop while (and (not (null (prefix cache)))
		   (>= (end-line (first (prefix cache)))
		       (line-counter cache)))
	do (prefix-to-suffix cache))
  ;; As long as there are parse results on the suffix that completely
  ;; precede the number of skipped lines, move them to the prefix.
  (loop while (and (not (null (suffix cache)))
		   (< (end-line (first (suffix cache)))
		      (line-counter cache)))
	do (suffix-to-prefix cache)))

;;; Return true if and only if either there are no more parse results,
;;; or the first parse result starts at a line that is strictly
;;; greater than LINE-NUMBER.
(defun next-parse-result-is-beyond-line-p (cache line-number)
  (with-accessors ((suffix suffix) (worklist worklist)) cache
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
      (+ (start-line parse-result) (height parse-result))))

;;; Add INCREMENT to the absolute line number of every parse result on
;;; the worklist, and of the first parse result of the suffix, if any.
(defun adjust-worklist-and-suffix (cache increment)
  (loop for parse-result in (worklist cache)
	do (incf (start-line parse-result) increment))
  (unless (null (suffix cache))
    (incf (start-line (first (suffix cache))) increment)))

;;; If the worklist is empty then move a parse result from the suffix
;;; to the worklist (in that case, it is known that the suffix is not
;;; empty).
(defun ensure-worklist-not-empty (cache)
  (with-accessors ((worklist worklist)) cache
    (when (null worklist)
      (push-to-worklist cache (pop-from-suffix cache)))))

;;; When this function is called, there is at least one parse result,
;;; either on the work list or on the suffix that must be processed,
;;; i.e., that parse result either entirely precedes LINE-NUMBER (so
;;; that it should be moved to the residue), or it straddles the line
;;; with that line number, so that it must be taken apart.
(defun process-next-parse-result (cache line-number)
  (with-accessors ((worklist worklist)) cache
    (ensure-worklist-not-empty cache)
    (let ((parse-result (pop-from-worklist cache)))
      (if (line-is-inside-parse-result-p parse-result line-number)
	  (let ((children (children parse-result)))
	    (make-absolute children (start-line parse-result))
	    (setf worklist (append children worklist)))
	  (push-to-residue cache parse-result)))))

(defun handle-modified-line (cache line-number)
  (let ((line (flexichain:element* (contents cache) line-number)))
    (setf (contents line)
          (cluffer:items (cluffer-line line))))
  (loop until (next-parse-result-is-beyond-line-p cache line-number)
	do (process-next-parse-result cache line-number)))

(defun handle-inserted-line (cache line-number)
  (loop until (next-parse-result-is-beyond-line-p cache (1- line-number))
	do (process-next-parse-result cache line-number))
  (adjust-worklist-and-suffix cache 1))

(defun handle-deleted-line (cache line-number)
  (loop until (next-parse-result-is-beyond-line-p cache line-number)
	do (process-next-parse-result cache line-number))
  (adjust-worklist-and-suffix cache -1))

;;; Take into account modifications to the buffer by destroying the
;;; parts of the cache that are no longer valid, while keeping parse
;;; results that are not affected by such modifications.
(defun scavenge (cache buffer)
  (let ((cache-initialized-p nil))
    (with-accessors ((lines contents)
		     (line-counter line-counter))
	cache
      (setf line-counter 0)
      (flet ((remove-deleted-lines (line)
	       (loop for cache-line = (flexichain:element* lines line-counter)
		     for cluffer-line = (cluffer-line cache-line)
		     until (eq line cluffer-line)
		     do (flexichain:delete* lines line-counter)
			(handle-deleted-line cache line-counter)))
	     (ensure-cache-initialized ()
	       (unless cache-initialized-p
		 (setf cache-initialized-p t)
		 (ensure-update-initialized cache))))
	(flet ((skip (count)
		 (incf line-counter count))
	       (modify (line)
		 (ensure-cache-initialized)
		 (remove-deleted-lines line)
		 (handle-modified-line cache line-counter)
		 (incf line-counter))
	       (create (line)
		 (ensure-cache-initialized)
		 (let ((temp (make-instance 'line
			       :cluffer-line line
			       :contents (cluffer:items line))))
		   (flexichain:insert* lines line-counter temp))
		 (handle-inserted-line cache line-counter)
		 (incf line-counter))
	       (sync (line)
		 (remove-deleted-lines line)
		 (incf line-counter)))
	  (setf (time-stamp cache)
		(cluffer:update buffer
				(time-stamp cache)
				#'sync #'skip #'modify #'create))))))
  (finish-scavenge cache))

(defmethod max-line-width ((cache cache))
  (with-accessors ((prefix prefix) (suffix suffix)) cache
    (max (if (null prefix) 0 (max-line-width-list (first prefix)))
         (if (null suffix) 0 (max-line-width-list (first suffix))))))

;;; Methods that make an instance of CACHE behave like an instance
;;; of FOLIO.

(defmethod line-length ((folio cache) line-number)
  (length (contents (flexichain:element* (contents folio) line-number))))

(defmethod item ((folio cache) line-number item-number)
  (aref (contents (flexichain:element* (contents folio) line-number))
	item-number))
