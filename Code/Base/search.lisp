(cl:in-package #:second-climacs-base)

(defun exchange-cursor-positions (cursor1 cursor2)
  (let ((line1 (cluffer:line cursor1))
        (position1 (cluffer:cursor-position cursor1))
        (line2 (cluffer:line cursor2))
        (position2 (cluffer:cursor-position cursor2)))
    (cluffer:detach-cursor cursor1)
    (cluffer:detach-cursor cursor2)
    (cluffer:attach-cursor cursor1 line2 position2)
    (cluffer:attach-cursor cursor2 line1 position1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; SEARCH-FORWARD.

(defun search-forward (cursor string)
  ;; Clone CURSOR.
  (let* ((line (cluffer:line cursor))
         (position (cluffer:cursor-position cursor))
         (search-cursor (make-instance 'standard-cursor
                          :buffer (buffer cursor))))
    (cluffer:attach-cursor search-cursor line position)
    (loop with match-count = 0
          do (cond ((= match-count (length string))
                    ;; We have a complete match of STRING.
                    (exchange-cursor-positions cursor search-cursor)
                    (cluffer:detach-cursor search-cursor)
                    (return t))
                   ((cluffer:end-of-buffer-p search-cursor)
                    ;; We reached the end of the buffer without any
                    ;; match.
                    (cluffer:detach-cursor search-cursor)
                    (return nil))
                   ((eql (item-after-cursor search-cursor)
                         (char string match-count))
                    ;; We have a match of the next character in
                    ;; STRING.
                    (incf match-count)
                    (forward-item search-cursor))
                   ((zerop match-count)
                    ;; We do not have a match, and we have not matched
                    ;; anything so far.
                    (forward-item search-cursor))
                   (t
                    ;; We do not have a match, but we have already
                    ;; matched a prefix of STRING.
                    (loop repeat (1- match-count)
                          do (backward-item search-cursor))
                    (setf match-count 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; SEARCH-BACKWARD.

(defun search-backward (cursor string)
  ;; Clone CURSOR.
  (let* ((line (cluffer:line cursor))
         (position (cluffer:cursor-position cursor))
         (search-cursor (make-instance 'standard-cursor
                          :buffer (buffer cursor))))
    (cluffer:attach-cursor search-cursor line position)
    (loop with match-count = 0
          do (cond ((= match-count (length string))
                    ;; We have a complete match of STRING.
                    (exchange-cursor-positions cursor search-cursor)
                    (cluffer:detach-cursor search-cursor)
                    (return t))
                   ((cluffer:beginning-of-buffer-p search-cursor)
                    ;; We reached the beginning of the buffer without
                    ;; any match.
                    (cluffer:detach-cursor search-cursor)
                    (return nil))
                   ((eql (item-before-cursor search-cursor)
                         (char string (- (length string) match-count 1)))
                    ;; We have a match of the next character in
                    ;; STRING.
                    (incf match-count)
                    (backward-item search-cursor))
                   ((zerop match-count)
                    ;; We do not have a match, and we have not matched
                    ;; anything so far.
                    (backward-item search-cursor))
                   (t
                    ;; We do not have a match, but we have already
                    ;; matched a suffix of STRING.
                    (loop repeat (1- match-count)
                          do (forward-item search-cursor))
                    (setf match-count 0))))))
