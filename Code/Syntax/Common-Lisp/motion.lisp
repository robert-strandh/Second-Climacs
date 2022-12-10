(cl:in-package #:second-climacs-syntax-common-lisp)

(define-condition already-at-top-level (base:climacs-error)
  ()
  (:report "Already at top level"))

(defun up-expression (cache cursor)
  (multiple-value-bind (line-number column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (find-wads-containing-position cache line-number column-number)))
      (if (null lines-and-wads)
          (error 'already-at-top-level)
          (destructuring-bind (new-line-number . wad)
              (first lines-and-wads)
            (base:set-cursor-positions
             cursor new-line-number (start-column wad)))))))
