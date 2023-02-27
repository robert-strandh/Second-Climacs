(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun collect-words (wad-descriptors buffer)
  (mapcar (lambda (word-wad-descriptor)
            (coerce (items word-wad-descriptor buffer) 'string))
          (alexandria:mappend #'children wad-descriptors)))

(defun fill-paragraph-using-wad-descriptors (wad-descriptors buffer cursor
                                             &rest args &key prefix
                                                             suffix
                                                             per-line-prefix)
  (declare (ignore prefix suffix per-line-prefix))
  (let* ((first               (first wad-descriptors))
         (start-line-number   (start-line-number first))
         (start-column-number (start-column-number first))
         (last                (first (last wad-descriptors)))
         (end-line-number     (end-line-number last))
         (end-column-number   (end-column-number last))
         (words               (collect-words wad-descriptors buffer)))
    (let ((end-cursor (make-instance 'base:standard-cursor :buffer buffer)))
      (base:set-cursor-positions cursor start-line-number start-column-number)
      (cluffer:attach-cursor
       end-cursor (cluffer:find-line buffer end-line-number) end-column-number)
      (apply #'base:fill-words cursor end-cursor words args)
      (cluffer:detach-cursor end-cursor))))

(defun fill-semicolon-comment-using-wad-descriptor (wad-descriptors buffer cursor)
  (let* ((first               (first wad-descriptors))
         (start-column-number (start-column-number first))
         (semicolon-count     (semicolon-count (wad first))))
    (let ((prefix          (format nil "~V,,,';<~>" semicolon-count))
          (per-line-prefix (format nil "~V<~>~V,,,';<~>" start-column-number semicolon-count)))
      (fill-paragraph-using-wad-descriptors
       wad-descriptors buffer cursor :prefix prefix :per-line-prefix per-line-prefix))))

(defun fill-paragraph-candidate-p (wad-descriptor)
  (and (not (null wad-descriptor))
       (typep (wad wad-descriptor) 'semicolon-comment-wad)))

(defun fill-paragraph-top-level (cache wad-descriptor buffer cursor)
  (unless (null wad-descriptor)
    (push-to-suffix cache (pop-from-prefix cache)))
  ;; Now, the wad indicated by the cursor is always the first wad on
  ;; the suffix.
  (with-accessors ((prefix prefix) (suffix suffix)) cache
    (loop until (or (null prefix)
                    (not (typep (first prefix) 'semicolon-comment-wad))
                    (< (start-line (first prefix))
                       (1- (start-line (first suffix))))
                    (/= (semicolon-count (first prefix))
                        (semicolon-count (first suffix))))
          do (push-to-suffix cache (pop-from-prefix cache)))
    ;; Now, all the wads we are interested in are on the suffix.
    (let ((wad-descriptors
            (list (make-wad-descriptor-from-wad (first suffix)))))
      (push-to-prefix cache (pop-from-suffix cache))
      (loop until (or (null suffix)
                      (not (typep (first suffix) 'semicolon-comment-wad))
                      (> (start-line (first suffix))
                         (1+ (start-line (first prefix))))
                      (/= (semicolon-count (first prefix))
                          (semicolon-count (first suffix))))
            do (push (make-wad-descriptor-from-wad (first suffix))
                     wad-descriptors)
               (push-to-prefix cache (pop-from-suffix cache)))
      (fill-semicolon-comment-using-wad-descriptor
       (reverse wad-descriptors) buffer cursor))))

(defun fill-paragraph-non-top-level (wad-descriptor buffer cursor)
  (let ((start wad-descriptor))
    ;; Find the first semicolon comment wad to be involved in this
    ;; operation.
    (loop for previous = (previous-sibling start)
          while (and (fill-paragraph-candidate-p previous)
                     (= (1- (start-line-number start))
                        (start-line-number previous)))
          do (setf start previous))
    ;; Collect the wad descriptors involved.
    (let ((wad-descriptors (list start)))
      (loop for current = (first wad-descriptors)
            for next = (next-sibling current)
            while (and (fill-paragraph-candidate-p next)
                       (= (1+ (start-line-number current))
                          (start-line-number next)))
            do (push next wad-descriptors))
      (fill-semicolon-comment-using-wad-descriptor
       (reverse wad-descriptors) buffer cursor))))

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (compute-wad-descriptors cache cursor)
    (declare (ignore previous))
    (let ((cursor-line-number (cluffer:line-number cursor)))
      (when (or ;; If the cursor is inside an atomic wad, but that wad
                ;; is not a semicolon comment wad, then it is an
                ;; error.
                (and (not (null current))
                     (not (typep (wad current) 'comment-wad)))
                ;; The other possibility for an error is that the
                ;; current wad is NIL, and either the next wad is not
                ;; a semicolon comment wad, or it is a semicolon
                ;; comment wad, but it is on a different line from the
                ;; cursor.
                (and (null current)
                     (or (not (fill-paragraph-candidate-p next))
                         (not (= cursor-line-number
                                 (start-line-number next))))))
        (error 'not-in-comment)))
    (let ((buffer (cluffer:buffer cursor)))
      (cond ((and current (typep (wad current) 'block-comment-wad))
             (fill-paragraph-using-wad-descriptors
              (list current) buffer cursor :prefix          "#|"
                                           :per-line-prefix "  "
                                           :suffix          "|#"))
            ((null parent)
             (fill-paragraph-top-level cache current buffer cursor))
            ((null current)
             (fill-paragraph-non-top-level next buffer cursor))
            (t
             (fill-paragraph-non-top-level current buffer cursor))))))
