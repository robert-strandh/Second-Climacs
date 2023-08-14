(cl:in-package #:second-climacs-incremental-parsing)

(defun whitespacep (character)
  (member character '(#\Space #\Newline)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\; #\( #\) #\" #\-)))

;;; Return the line number and the column number of CURSOR as two
;;; values.
(defun cursor-positions (cursor)
  (values (cluffer:line-number cursor)
          (cluffer:cursor-position cursor)))

;;; Set the line number and the column number of CURSOR.
(defun set-cursor-positions (cursor line-number column-number)
  (let ((buffer (cluffer:buffer cursor)))
    (when (cluffer:cursor-attached-p cursor)
      (cluffer:detach-cursor cursor))
    (cluffer:attach-cursor
      cursor
      (cluffer:find-line buffer line-number)
      column-number)))
