(cl:in-package #:second-climacs-base)

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
