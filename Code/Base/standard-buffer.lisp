(cl:in-package #:second-climacs-base)

;;; A STANDARD-BUFFER is a buffer that is restricted to contain a
;;; Cluffer buffer.

(defclass standard-buffer (edit.search:search-state-mixin
                           edit:multiple-site-mixin
                           edit:site-mixin
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
        do (edit::insert-item cursor char)))
