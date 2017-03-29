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
           (if eof-error-p
               (error 'end-of-file :stream input-stream)
               (return-from sicl-reader:read-common eof-value)))
         (case (sicl-reader:syntax-type char)
           (:whitespace
            (go step-1-start))
           ((:terminating-macro :non-terminating-macro)
            (let ((values (multiple-value-list
                           (sicl-reader:call-reader-macro
                            (get-macro-character char)
                            input-stream
                            char))))
              (if (null values)
                  (go step-1-start)
                  (return-from sicl-reader:read-common (car values)))))
           (t
            (unread-char char input-stream)
            (return-from sicl-reader:read-common
              (sicl-reader:read-token input-stream eof-error-p eof-value))))))))
