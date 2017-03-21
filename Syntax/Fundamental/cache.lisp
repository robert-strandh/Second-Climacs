(cl:in-package #:climacs-syntax-fundamental)

(defgeneric list-length (list-or-entry)
  (:method ((list null))
    0)
  (:method ((list cons))
    (list-length (first list))))

(defclass entry ()
  (;; This slot contains the Cluffer line object so that we can
   ;; find it when we process a SYNC operation.
   (%line :initarg :line :reader line)
   ;; This slot contains the contents of the line as it was when we
   ;; last updated the cache.
   (%contents :initarg :contents :accessor contents)
   ;; This slot contains the number of entries in the list (prefix or
   ;; suffix) that this entry is a member of.
   (%list-length :initarg list-length :accessor list-length)))

(defclass cache ()
  ((%prefix :initform () :accessor prefix)
   (%suffix :initform () :accessor suffix)
   (%time-stamp :initform nil :accessor time-stamp)))

(defgeneric push-to-prefix (cache entry)
  (:method ((cache cache) (entry entry))
    (with-accessors ((prefix prefix)) cache
      (setf (list-length entry)
            (1+ (list-length prefix)))
      (push entry prefix))))

(defgeneric pop-from-prefix (cache)
  (:method ((cache cache))
    (with-accessors ((prefix prefix)) cache
      (assert (not (null prefix)))
      (pop prefix))))

(defgeneric push-to-suffix (cache entry)
  (:method ((cache cache) (entry entry))
    (with-accessors ((suffix suffix)) cache
      (setf (list-length entry)
            (1+ (list-length suffix)))
      (push entry suffix))))

(defgeneric pop-from-suffix (cache)
  (:method ((cache cache))
    (with-accessors ((suffix suffix)) cache
      (assert (not (null suffix)))
      (pop suffix))))

(defgeneric prefix-to-suffix (cache)
  (:method ((cache cache))
    (push-to-suffix cache (pop-from-prefix cache))))

(defgeneric suffix-to-prefix (cache)
  (:method ((cache cache))
    (push-to-prefix cache (pop-from-suffix cache))))

(defun adjust-prefix-and-suffix (cache line-number)
  (with-accessors ((prefix prefix) (suffix suffix)) cache
    (loop until (<= (list-length prefix) line-number)
          do (prefix-to-suffix cache))
    (loop until (>= (list-length prefix) line-number)
          do (suffix-to-prefix cache))))

(defun scavenge (cache buffer)
  (let ((line-counter 0))
    (with-accessors ((prefix prefix) (suffix suffix)) cache
      (flet ((remove-deleted-lines (line)
               (loop for first = (first suffix)
                     until (eq line (line first))
                     do (pop-from-suffix cache))))
	(flet ((skip (count)
		 (incf line-counter count))
	       (modify (line)
                 (adjust-prefix-and-suffix cache line-counter)
		 (remove-deleted-lines line)
                 (setf (contents (first suffix))
                       (cluffer:items line))
                 (suffix-to-prefix cache)
		 (incf line-counter))
	       (create (line)
                 (adjust-prefix-and-suffix cache line-counter)
		 (let ((temp (make-instance 'entry
			       :line line
			       :contents (cluffer:items line))))
                   (push-to-prefix temp))
		 (incf line-counter))
	       (sync (line)
                 (adjust-prefix-and-suffix cache line-counter)
		 (remove-deleted-lines line)
                 (suffix-to-prefix cache)
		 (incf line-counter)))
	  (setf (time-stamp cache)
		(cluffer:update buffer
				(time-stamp cache)
				#'sync #'skip #'modify #'create)))))))
