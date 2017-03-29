(cl:in-package #:climacs-syntax-common-lisp)

(defmethod sicl-reader:read-common
    ((input-stream analyzer) eof-error-p eof-value)
  (let ((*stack* (cons '() *stack*))
        (start-line (current-line-number input-stream))
        (start-column (current-item-number input-stream)))
    (tagbody
     step-1-start
       (let ((char (read-char input-stream nil nil)))
         (when (null char)
           (push-parse-result
            (make-instance 'eof-parse-result
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
           (return-from sicl-reader:read-common eof-value))
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
                     (make-instance 'no-expression-parse-result
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
                    (make-instance 'expression-parse-result
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
                      :relative-p nil)
                    (return-from sicl-reader:read-common (car values))))))
           (t
            (unread-char char input-stream)
            (return-from sicl-reader:read-common
              (sicl-reader:read-token input-stream eof-error-p eof-value))))))))
