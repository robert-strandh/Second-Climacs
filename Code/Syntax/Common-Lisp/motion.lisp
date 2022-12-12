(cl:in-package #:second-climacs-syntax-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command UP-EXPRESSION.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; FORWARD-EXPRESSION.

(define-condition no-following-expression (base:climacs-error)
  ()
  (:report "No following expression"))

(defun forward-top-level-expression-in-prefix (prefix cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (loop for (wad previous) on prefix
          when (or (null previous)
                   (position-is-after-wad-p
                    previous cursor-line-number cursor-column-number))
            do (base:set-cursor-positions
                cursor
                (+ (start-line wad) (height wad))
                (end-column wad))
               (return))))

(defun forward-top-level-expression-in-suffix (suffix cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((first (first suffix)))
      (if (position-is-before-wad-p
           first cursor-line-number cursor-column-number)
          ;; We found our wad to forward over.
          (base:set-cursor-positions
           cursor
           (+ (start-line first) (height first)) (end-column first))
          ;; If not, we must traverse the relative wads in the rest of
          ;; the suffix.
          (loop for reference = (start-line first)
                  then (+ reference (start-line wad))
                for wad in (rest suffix)
                when (position-is-before-wad-p
                      wad
                      (- cursor-line-number reference)
                      cursor-column-number)
                  do (base:set-cursor-positions
                      cursor
                      (+ reference (start-line wad) (height wad))
                      (end-column wad))
                     (return)
                finally ;; We come here when every wad has been
                        ;; examined and no wad starts after the
                        ;; cursor, so there is no wad to forward over.
                        (error 'no-following-expression))))))

(defun forward-top-level-expression (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((prefix (prefix cache))
          (suffix (suffix cache)))
      (cond ((null prefix)
             (if (null suffix)
                 (error 'no-following-expression)
                 (forward-top-level-expression-in-suffix suffix cursor)))
            ((null suffix)
             (forward-top-level-expression-in-prefix prefix cursor))
            ((position-is-after-wad-p
              (first prefix) cursor-line-number cursor-column-number)
             (forward-top-level-expression-in-suffix suffix cursor))
            (t
             (forward-top-level-expression-in-prefix prefix cursor))))))

(defun forward-non-top-level-expression (parent-wad line-number cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (loop for reference = line-number
            then (+ reference (start-line child))
          for child in (children parent-wad)
          when (position-is-before-wad-p
                child
                (- cursor-line-number reference)
                cursor-column-number)
            do (base:set-cursor-positions
                cursor
                (+ reference (start-line child) (height child))
                (end-column child))
               (return)
          finally ;; We come here when every child has been examined and
                  ;; no child starts after the cursor, so there is no
                  ;; child to forward over.
                  (error 'no-following-expression))))

(defun forward-expression (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (find-wads-containing-position
             cache cursor-line-number cursor-column-number)))
      (if (null lines-and-wads)
          (forward-top-level-expression cache cursor)
          (destructuring-bind (new-line-number . wad)
              (first lines-and-wads)
            (forward-non-top-level-expression wad new-line-number cursor))))))
