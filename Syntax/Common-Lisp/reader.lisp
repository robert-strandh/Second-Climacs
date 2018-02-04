(cl:in-package #:climacs-syntax-common-lisp)

(defmethod reader:read-common
    ((input-stream analyzer) eof-error-p eof-value)
  (let ((*stack* (cons '() *stack*))
        (start-line (current-line-number input-stream))
        (start-column (current-item-number input-stream)))
    (handler-case
        (tagbody
         step-1-start
           (let ((char (read-char input-stream nil nil)))
             (when (null char)
               (error 'end-of-file :stream input-stream))
             (unread-char char input-stream)
             (let ((wad (cached-wad input-stream)))
               (if (null wad)
                   (setf char (read-char input-stream nil nil))
                   (progn
                     (push-wad wad)
                     (advance-stream-to-beyond-wad input-stream wad)
                     (etypecase wad
                       (expression-wad
                        (return-from reader:read-common
                          (expression wad)))
                       (eof-wad
                        (return-from reader:read-common nil))
                       (no-expression-wad
                        (go step-1-start))))))
             (case (readtable::syntax-type reader:*readtable* char)
               (:whitespace
                (setf start-line (current-line-number input-stream))
                (setf start-column (current-item-number input-stream))
                (go step-1-start))
               ((:terminating-macro :non-terminating-macro)
                (let ((values (multiple-value-list
                               (reader:call-reader-macro
                                (get-macro-character char)
                                input-stream
                                char))))
                  (if (null values)
                      (progn
                        (push-wad
                         (make-wad
                             (if (eql char #\;)
                                 'comment-wad
                                 'no-expression-wad)
                           :max-line-width (compute-max-line-width
                                            input-stream
                                            start-line
                                            (current-line-number input-stream)
                                            (first *stack*))
                           :children (make-relative (nreverse (first *stack*))
                                      start-line)
                           :start-line start-line
                           :start-column start-column
                           :height (- (current-line-number input-stream) start-line)
                           :end-column (current-item-number input-stream)
                           :relative-p nil))
                        (setf start-line (current-line-number input-stream))
                        (setf start-column (current-item-number input-stream))
                        (go step-1-start))
                      (progn
                        (push-wad
                         (make-wad 'expression-wad
                           :expression (first values)
                           :max-line-width (compute-max-line-width
                                            input-stream
                                            start-line
                                            (current-line-number input-stream)
                                            (first *stack*))
                           :children (make-relative (nreverse (first *stack*))
                                                    start-line)
                           :start-line start-line
                           :start-column start-column
                           :height (- (current-line-number input-stream)
                                      start-line)
                           :end-column (current-item-number input-stream)
                           :relative-p nil))
                        (return-from reader:read-common (car values))))))
               (t
                (unread-char char input-stream)
                (let ((token (reader:read-token input-stream
                                                eof-error-p
                                                eof-value)))
                  (push-wad
                   (make-wad 'expression-wad
                     :expression token
                     :max-line-width (compute-max-line-width
                                      input-stream
                                      start-line
                                      (current-line-number input-stream)
                                      (first *stack*))
                     :children (make-relative (nreverse (first *stack*))
                                              start-line)
                     :start-line start-line
                     :start-column start-column
                     :height (- (current-line-number input-stream)
                                start-line)
                     :end-column (current-item-number input-stream)
                     :relative-p nil))
                  (return-from reader:read-common token))))))
      (end-of-file ()
        (push (make-wad 'eof-wad
                :max-line-width (compute-max-line-width
                                 input-stream
                                 start-line
                                 (current-line-number input-stream)
                                 (first *stack*))
                :children (make-relative (nreverse (first *stack*))
                                         start-line)
                :start-line start-line
                :start-column start-column
                :height (- (current-line-number input-stream)
                           start-line)
                :end-column (current-item-number input-stream)
                :relative-p nil)
              (second *stack*))
        (error 'end-of-file)))))
