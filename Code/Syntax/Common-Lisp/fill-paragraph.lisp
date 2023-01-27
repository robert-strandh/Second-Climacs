(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun fill-paragraph-candidate-p (wad-descriptor)
  (and (not (null wad-descriptor))
       (typep (wad wad-descriptor) 'semicolon-comment-wad)))

(defun fill-paragraph-top-level (cache wad-descriptor)
  (unless (null wad-descriptor)
    (push-to-suffix cache (pop-from-prefix cache)))
  ;; Now, the wad indicated by the cursor is always the first wad on
  ;; the suffix.
  (with-accessors ((prefix prefix) (suffix suffix)) cache
    (loop until (or (null prefix)
                    (not (typep (first prefix) 'semicolon-comment-wad))
                    (< (start-line (first prefix))
                       (1- (start-line (first suffix)))))
          do (push-to-suffix cache (pop-from-prefix cache)))
    ;; Now, all the wads we are interested in are on the suffix.
    (let ((wad-descriptors
            (list (make-wad-descriptor-from-wad (first suffix)))))
      (push-to-prefix cache (pop-from-suffix cache))
      (loop until (or (null suffix)
                      (not (typep (first suffix) 'semicolon-comment-wad))
                      (> (start-line (first suffix))
                         (1+ (start-line (first prefix)))))
            do (push (make-wad-descriptor-from-wad (first suffix))
                     wad-descriptors)
               (push-to-prefix cache (pop-from-suffix cache)))
      (loop with ignore = (format *trace-output* "~%")
            for wad-descriptor in wad-descriptors
            do (format *trace-output* "~s~%" wad-descriptor)))))

(defun fill-paragraph-non-top-level (wad-descriptor)
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
      (loop with ignore = (format *trace-output* "~%")
            for wad-descriptor in wad-descriptors
            do (format *trace-output* "~s~%" wad-descriptor)))))

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (compute-wad-descriptors cache cursor)
    (declare (ignore previous))
    (let ((cursor-line-number (cluffer:line-number cursor)))
      (when (or
             ;; If the cursor is inside an atomic wad, but that wad is
             ;; not a semicolon comment wad, then it is an error.
             (and (not (null current))
                  (not (typep (wad current) 'semicolon-comment-wad)))
             ;; The other possibility for an error is that the current
             ;; wad is NIL, and either the next wad is not a semicolon
             ;; comment wad, or it is a semicolon comment wad, but it is
             ;; on a different line from the cursor.
             (and (null current)
                  (or (not (fill-paragraph-candidate-p next))
                      (not (= cursor-line-number
                              (start-line-number next))))))
        (error 'not-in-comment)))
    (if (null parent)
        (fill-paragraph-top-level cache current)
        (if (null current)
            (fill-paragraph-non-top-level next)
            (fill-paragraph-non-top-level current)))))
