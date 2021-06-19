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
   ;; last updated the analyzer.
   (%contents :initarg  :contents
              :type     string
              :accessor contents)
   ;; This slot contains the number of entries in the list (prefix or
   ;; suffix) that this entry is a member of.
   (%list-length :initarg list-length :accessor list-length)))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream)
    (format stream "list-length ~a contents ~s"
            (list-length object)
            (contents object))))

(defclass analyzer (climacs2-base:analyzer)
  ((%prefix :initform () :accessor prefix)
   (%suffix :initform () :accessor suffix)
   (%time-stamp :initform nil :accessor time-stamp)))

(defgeneric push-to-prefix (analyzer entry)
  (:method ((analyzer analyzer) (entry entry))
    (with-accessors ((prefix prefix)) analyzer
      (setf (list-length entry)
            (1+ (list-length prefix)))
      (push entry prefix))))

(defgeneric pop-from-prefix (analyzer)
  (:method ((analyzer analyzer))
    (with-accessors ((prefix prefix)) analyzer
      (assert (not (null prefix)))
      (pop prefix))))

(defgeneric push-to-suffix (analyzer entry)
  (:method ((analyzer analyzer) (entry entry))
    (with-accessors ((suffix suffix)) analyzer
      (setf (list-length entry)
            (1+ (list-length suffix)))
      (push entry suffix))))

(defgeneric pop-from-suffix (analyzer)
  (:method ((analyzer analyzer))
    (with-accessors ((suffix suffix)) analyzer
      (assert (not (null suffix)))
      (pop suffix))))

(defgeneric prefix-to-suffix (analyzer)
  (:method ((analyzer analyzer))
    (push-to-suffix analyzer (pop-from-prefix analyzer))))

(defgeneric suffix-to-prefix (analyzer)
  (:method ((analyzer analyzer))
    (push-to-prefix analyzer (pop-from-suffix analyzer))))

(defun adjust-prefix-and-suffix (analyzer line-number)
  (with-accessors ((prefix prefix) (suffix suffix)) analyzer
    (loop until (<= (list-length prefix) line-number)
          do (prefix-to-suffix analyzer))
    (loop until (>= (list-length prefix) line-number)
          do (suffix-to-prefix analyzer))))

(defun scavenge (analyzer buffer)
  (let ((line-counter 0))
    (with-accessors ((prefix prefix) (suffix suffix)) analyzer
      (flet ((remove-deleted-lines (line)
               (loop for first = (first suffix)
                     until (eq line (line first))
                     do (pop-from-suffix analyzer))))
        (flet ((skip (count)
                 (incf line-counter count))
               (modify (line)
                 (adjust-prefix-and-suffix analyzer line-counter)
                 (remove-deleted-lines line)
                 (setf (contents (first suffix))
                       (cluffer:items line))
                 (suffix-to-prefix analyzer)
                 (incf line-counter))
               (create (line)
                 (adjust-prefix-and-suffix analyzer line-counter)
                 (let ((temp (make-instance 'entry
                               :line line
                               :contents (cluffer:items line))))
                   (push-to-prefix analyzer temp))
                 (incf line-counter))
               (sync (line)
                 (adjust-prefix-and-suffix analyzer line-counter)
                 (remove-deleted-lines line)
                 (suffix-to-prefix analyzer)
                 (incf line-counter)))
          (setf (time-stamp analyzer)
                (cluffer:update buffer
                                (time-stamp analyzer)
                                #'sync #'skip #'modify #'create)))))))
