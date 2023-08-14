(cl:in-package #:second-climacs-incremental-parsing)

(defun whitespacep (character)
  (member character '(#\Space #\Newline)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\; #\( #\) #\" #\-)))
