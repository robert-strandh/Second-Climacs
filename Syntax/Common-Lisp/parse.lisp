(cl:in-package #:climacs-syntax-common-lisp)

(defun parse-and-cache (analyzer)
  (let ((*stack* (list '()))
        (reader:*preserve-whitespace* t)
        (start-line (current-line-number analyzer))
        (start-column (current-item-number analyzer)))
    (handler-case (reader:read-preserving-whitespace analyzer)
      (end-of-file ()
        nil)
      (error ()
        (setf (first *stack*)
              (list (make-wad 'error-wad
                      :max-line-width (compute-max-line-width
                                       analyzer
                                       start-line
                                       (current-line-number analyzer)
                                       (first *stack*))
                      :children (make-relative (nreverse (first *stack*))
                                               start-line)
                      :start-line start-line
                      :start-column start-column
                      :height (- (current-line-number analyzer) start-line)
                      :end-column (current-item-number analyzer)
                      :relative-p nil)))))
    (loop for wad in (reverse (first *stack*))
          do (compute-child-indentations wad nil)
             (push-to-prefix (folio analyzer) wad))))
