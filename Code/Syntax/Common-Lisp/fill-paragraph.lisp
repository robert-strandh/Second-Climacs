(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun collect-words (wad-descriptors buffer)
  (mapcar (lambda (word-wad-descriptor)
            (coerce (ip:items word-wad-descriptor buffer) 'string))
          (alexandria:mappend #'ip:children wad-descriptors)))

(defun fill-paragraph-using-wad-descriptors (wad-descriptors buffer cursor
                                             &rest args &key prefix
                                                             suffix
                                                             per-line-prefix)
  (declare (ignore prefix suffix per-line-prefix))
  (let* ((first               (first wad-descriptors))
         (start-line-number   (ip:start-line-number first))
         (start-column-number (ip:start-column-number first))
         (last                (first (last wad-descriptors)))
         (end-line-number     (ip:end-line-number last))
         (end-column-number   (ip:end-column-number last))
         (words               (collect-words wad-descriptors buffer)))
    (let ((end-cursor (make-instance 'base:standard-cursor :buffer buffer)))
      (base:set-cursor-positions cursor start-line-number start-column-number)
      (cluffer:attach-cursor
       end-cursor (cluffer:find-line buffer end-line-number) end-column-number)
      (apply #'base:fill-words cursor end-cursor words args)
      (cluffer:detach-cursor end-cursor))))

(defun fill-semicolon-comment-using-wad-descriptor
    (wad-descriptors buffer cursor)
  (let* ((first               (first wad-descriptors))
         (start-column-number (ip:start-column-number first))
         (semicolon-count     (ip:semicolon-count (ip:wad first))))
    (let ((prefix          (format nil "~V,,,';<~>" semicolon-count))
          (per-line-prefix (format nil "~V<~>~V,,,';<~>"
                                   start-column-number semicolon-count)))
      (fill-paragraph-using-wad-descriptors
       wad-descriptors buffer cursor
       :prefix prefix :per-line-prefix per-line-prefix))))

(defun fill-paragraph-candidate-p (wad-descriptor)
  (and (not (null wad-descriptor))
       (typep (ip:wad wad-descriptor) 'ip:semicolon-comment-wad)))

;;; When this function is called, either the cursor is in a top-level
;;; semicolon comment wad so that the current wad is not NIL, or it is
;;; located before a top-level semicolon wad on the same line as the
;;; cursor, so that the current wad is nil, but the next wad is a
;;; top-level semicolon wad.

(defun fill-paragraph-top-level (cache current next buffer cursor)
  ;; Either CURRENT is not NIL, meaning the cursor is inside the wad
  ;; described by CURRENT and CURRENT is a semicolon wad, or CURRENT
  ;; is NIL meaning the cursor is located before the wad described by
  ;; NEXT on the same line as NEXT.  So either CURRENT (if CURRENT is
  ;; not NIL) or NEXT (if CURRENT is NIL) is the a wad descriptor to
  ;; start with.
  (let ((start-wad (ip:wad (if (null current) next current))))
    ;; Loop until start-wad is the first semicolon wad in the block.
    (loop for left-sibling = (ip:left-sibling start-wad)
          until (or (null left-sibling)
                    (not (typep left-sibling 'ip:semicolon-comment-wad))
                    (< (ip:start-line left-sibling)
                       (1- (ip:start-line start-wad)))
                    (/= (ip:semicolon-count left-sibling)
                        (ip:semicolon-count start-wad)))
          do (setf start-wad left-sibling))
    ;; Now collect wad descriptors of all the wads in the comment
    ;; block.
    (let ((wad-descriptors
            (loop for wad = start-wad then (ip:right-sibling wad)
                  for next = (ip:right-sibling wad) then (ip:right-sibling next)
                  collect (ip:make-wad-descriptor-from-wad cache wad)
                  until (or (null next)
                            (not (typep next 'ip:semicolon-comment-wad))
                            (> (ip:start-line next)
                               (1+ (ip:start-line wad)))
                            (/= (ip:semicolon-count next)
                                (ip:semicolon-count wad))))))
      (fill-semicolon-comment-using-wad-descriptor
       wad-descriptors buffer cursor))))

(defun fill-paragraph-non-top-level (wad-descriptor buffer cursor)
  (let ((start wad-descriptor))
    ;; Find the first semicolon comment wad to be involved in this
    ;; operation.
    (loop for previous = (ip:previous-sibling start)
          while (and (fill-paragraph-candidate-p previous)
                     (= (1- (ip:start-line-number start))
                        (ip:start-line-number previous)))
          do (setf start previous))
    ;; Collect the wad descriptors involved.
    (let ((wad-descriptors (list start)))
      (loop for current = (first wad-descriptors)
            for next = (ip:next-sibling current)
            while (and (fill-paragraph-candidate-p next)
                       (= (1+ (ip:start-line-number current))
                          (ip:start-line-number next)))
            do (push next wad-descriptors))
      (fill-semicolon-comment-using-wad-descriptor
       (reverse wad-descriptors) buffer cursor))))

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (ip:compute-wad-descriptors cache cursor)
    (declare (ignore previous))
    (let ((cursor-line-number (cluffer:line-number cursor)))
      (when (or ;; If the cursor is inside an atomic wad, but that wad
                ;; is not a semicolon comment wad, then it is an
                ;; error.
                (and (not (null current))
                     (not (typep (ip:wad current) 'ip:comment-wad)))
                ;; The other possibility for an error is that the
                ;; current wad is NIL, and either the next wad is not
                ;; a semicolon comment wad, or it is a semicolon
                ;; comment wad, but it is on a different line from the
                ;; cursor.
                (and (null current)
                     (or (not (fill-paragraph-candidate-p next))
                         (not (= cursor-line-number
                                 (ip:start-line-number next))))))
        (error 'not-in-comment)))
    (let ((buffer (cluffer:buffer cursor)))
      (cond ((and current (typep (ip:wad current) 'ip:block-comment-wad))
             (fill-paragraph-using-wad-descriptors
              (list current) buffer cursor :prefix          "#|"
                                           :per-line-prefix "  "
                                           :suffix          "|#"))
            ((null parent)
             (fill-paragraph-top-level cache current next buffer cursor))
            ((null current)
             (fill-paragraph-non-top-level next buffer cursor))
            (t
             (fill-paragraph-non-top-level current buffer cursor))))))
