(cl:in-package #:second-climacs-base)

;;; A STANDARD-BUFFER is a buffer that is restricted to contain a
;;; Cluffer buffer.

(defclass standard-buffer (text.editing.search:search-state-mixin
                           e:multiple-site-mixin
                           e:site-mixin
                           cluffer-standard-buffer:buffer
                           buffer)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAKE-EMPTY-STANDARD-BUFFER.

(defun make-empty-standard-buffer ()
  (let* ((line (make-instance 'cluffer-standard-line::closed-line :first-line-p t :last-line-p t))
         (buffer (make-instance 'standard-buffer :initial-line line)))
    buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on FILL-BUFFER-FROM-STREAM.

(defmethod fill-buffer-from-stream ((cursor cluffer:cursor) stream)
  (loop for char = (read-char stream nil nil)
        until (null char)
        do (text.editing::insert-item cursor char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some useful helper functions.

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
