(cl:in-package #:climacs-syntax-common-lisp)

(defmethod sicl-reader:read-common
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
             (let ((parse-result (cached-parse-result input-stream)))
               (if (null parse-result)
                   (setf char (read-char input-stream nil nil))
                   (progn
                     (push-parse-result parse-result)
                     (advance-stream-to-beyond-parse-result input-stream parse-result)
                     (etypecase parse-result
                       (expression-parse-result
                        (return-from sicl-reader:read-common
                          (expression parse-result)))
                       (eof-parse-result
                        (return-from sicl-reader:read-common nil))
                       (no-expression-parse-result
                        (go step-1-start))))))
             (case (sicl-reader:syntax-type char)
               (:whitespace
                (setf start-line (current-line-number input-stream))
                (setf start-column (current-item-number input-stream))
                (go step-1-start))
               ((:terminating-macro :non-terminating-macro)
                (let ((values (multiple-value-list
                               (sicl-reader:call-reader-macro
                                (get-macro-character char)
                                input-stream
                                char))))
                  (if (null values)
                      (progn
                        (push-parse-result
                         (make-parse-result
                             (if (eql char #\;)
                                 'comment-parse-result
                                 'no-expression-parse-result)
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
                        (push-parse-result
                         (make-parse-result 'expression-parse-result
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
                        (return-from sicl-reader:read-common (car values))))))
               (t
                (unread-char char input-stream)
                (let ((token (sicl-reader:read-token input-stream
                                                     eof-error-p
                                                     eof-value)))
                  (push-parse-result
                   (make-parse-result 'expression-parse-result
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
                  (return-from sicl-reader:read-common token))))))
      (end-of-file ()
        (push (make-parse-result 'eof-parse-result
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
