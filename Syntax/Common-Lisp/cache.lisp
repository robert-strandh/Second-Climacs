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
   (%contents :initarg  :contents
              :type     string
              :accessor contents)))

(defmethod print-object ((object line) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (contents object))))

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

(defgeneric pop-from-suffix (cache)
  (:method ((cache cache))
    (with-accessors ((suffix suffix)) cache
      (assert (not (null suffix)))
      (let ((result (pop suffix)))
        (unless (null suffix)
          (relative-to-absolute (first suffix) (start-line result)))
        result))))

(defgeneric push-to-suffix (cache wad)
  (:method ((cache cache) (wad wad))
    (assert (not (relative-p wad)))
    (with-accessors ((suffix suffix)) cache
      (unless (null suffix)
        (absolute-to-relative (first suffix) (start-line wad)))
      (push wad suffix))))

(defgeneric pop-from-prefix (cache)
  (:method ((cache cache))
    (pop (prefix cache))))

(defgeneric push-to-prefix (cache wad)
  (:method ((cache cache) (wad wad))
    (push wad (prefix cache))))

(defun pop-from-worklist (cache)
  (pop (worklist cache)))

(defun push-to-worklist (cache wad)
  (push wad (worklist cache)))

(defun pop-from-residue (cache)
  (pop (residue cache)))

(defun push-to-residue (cache wad)
  (push wad (residue cache)))

(defgeneric suffix-to-prefix (cache)
  (:method ((cache cache))
    (push-to-prefix cache (pop-from-suffix cache))))

(defgeneric prefix-to-suffix (cache)
  (:method ((cache cache))
    (assert (not (null (prefix cache))))
    (push-to-suffix cache (pop-from-prefix cache))))

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
(defun next-wad-is-beyond-line-p (cache line-number)
  (with-accessors ((suffix suffix) (worklist worklist)) cache
    (if (null worklist)
        (or (null suffix)
            (> (start-line (first suffix)) line-number))
        (> (start-line (first worklist)) line-number))))

;;; Return true if and only if LINE-NUMBER is one of the lines of WAD.
;;; The START-LINE of WAD is an absolute line number.
(defun line-is-inside-wad-p (wad line-number)
  (<= (start-line wad)
      line-number
      (+ (start-line wad) (height wad))))

;;; Add INCREMENT to the absolute line number of every parse result on
;;; the worklist, and of the first parse result of the suffix, if any.
(defun adjust-worklist-and-suffix (cache increment)
  (loop for wad in (worklist cache)
        do (incf (start-line wad) increment))
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
(defun process-next-wad (cache line-number)
  (with-accessors ((worklist worklist)) cache
    (ensure-worklist-not-empty cache)
    (let ((wad (pop-from-worklist cache)))
      (if (line-is-inside-wad-p wad line-number)
          (let ((children (children wad)))
            (make-absolute children (start-line wad))
            (setf worklist (append children worklist)))
          (push-to-residue cache wad)))))

(defun handle-modified-line (cache line-number)
  (let* ((line         (flexichain:element* (contents cache) line-number))
         (cluffer-line (cluffer-line line))
         (string       (coerce (cluffer:items cluffer-line) 'string)))
    (setf (contents line) string))
  (loop until (next-wad-is-beyond-line-p cache line-number)
        do (process-next-wad cache line-number)))

(defun handle-inserted-line (cache line-number)
  (loop until (next-wad-is-beyond-line-p cache (1- line-number))
        do (process-next-wad cache line-number))
  (adjust-worklist-and-suffix cache 1))

(defun handle-deleted-line (cache line-number)
  (loop until (next-wad-is-beyond-line-p cache line-number)
        do (process-next-wad cache line-number))
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
      (labels ((ensure-cache-initialized ()
                 (unless cache-initialized-p
                   (setf cache-initialized-p t)
                   (ensure-update-initialized cache)))
               ;; Line deletion
               (delete-cache-line ()
                 (flexichain:delete* lines line-counter)
                 (handle-deleted-line cache line-counter))
               (remove-deleted-lines (line)
                 ;; Look at cache lines starting at LINE-COUNTER. Delete
                 ;; all cache lines that do not have LINE as their
                 ;; associated cluffer line. Those lines correspond to
                 ;; deleted lines between the previously processed line
                 ;; and LINE.
                 (loop for cache-line = (flexichain:element* lines line-counter)
                       for cluffer-line = (cluffer-line cache-line)
                       until (eq line cluffer-line)
                       do (delete-cache-line)))
               ;; Handlers for Cluffer's update protocol events.
               (skip (count)
                 (incf line-counter count))
               (modify (line)
                 (ensure-cache-initialized)
                 (remove-deleted-lines line)
                 (handle-modified-line cache line-counter)
                 (incf line-counter))
               (create (line)
                 (ensure-cache-initialized)
                 (let* ((string (coerce (cluffer:items line) 'string))
                        (temp   (make-instance 'line :cluffer-line line
                                                     :contents     string)))
                   (flexichain:insert* lines line-counter temp))
                 (handle-inserted-line cache line-counter)
                 (incf line-counter))
               (sync (line)
                 (remove-deleted-lines line)
                 (incf line-counter)))
        ;; Run update protocol. The handler functions defined above
        ;; change the cache lines and the worklist so that they
        ;; correspond to the new buffer state.
        (setf (time-stamp cache)
              (cluffer:update buffer
                              (time-stamp cache)
                              #'sync #'skip #'modify #'create))
        ;; Remove trailing cache lines after the last
        ;; skipped/modified/... cache line, that no longer correspond
        ;; to existing lines in the cluffer buffer.
        (loop while (< line-counter (flexichain:nb-elements lines))
              do (delete-cache-line)))))
  (finish-scavenge cache))

;;; Methods that make an instance of CACHE behave like an instance
;;; of FOLIO.

(defmethod line-length ((folio cache) line-number)
  (length (contents (flexichain:element* (contents folio) line-number))))

(defmethod item ((folio cache) line-number item-number)
  (aref (contents (flexichain:element* (contents folio) line-number))
        item-number))

(defmethod line-contents ((folio cache) line-number)
  (contents (flexichain:element* (contents folio) line-number)))
