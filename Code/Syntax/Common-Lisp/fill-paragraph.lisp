(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition not-in-comment (base:climacs-error)
  ()
  (:report "Not in comment"))

(defun fill-paragraph (cache cursor)
  (multiple-value-bind (current parent previous next)
      (compute-wad-descriptors cache cursor)
    (declare (ignore parent))
    (flet ((candidate-p (wad-descriptor)
             (and (not (null wad-descriptor))
                  (typep (wad wad-descriptor) 'semicolon-comment-wad))))
      (let ((first
              (if (not (candidate-p current))
                  (if (not (candidate-p previous))
                      (if (not (candidate-p next))
                          (error 'not-in-comment)
                          next)
                      (loop for wd = previous then (previous-sibling wd)
                            while (candidate-p (previous-sibling wd))
                            finally (return wd)))
                  (loop for wd = current then (previous-sibling wd)
                        while (candidate-p (previous-sibling wd))
                        finally (return wd)))))
        (let ((wad-descriptors
                (loop for wd = first then (next-sibling wd)
                      while (candidate-p wd)
                      collect wd)))
          (loop with ignore = (format *trace-output* "~%")
                for wad-descriptor in wad-descriptors
                do (format *trace-output* "~s: ~s~%"
                           wad-descriptor (wad wad-descriptor))))))))
