(cl:in-package #:second-climacs-base)

;;; A STANDARD-BUFFER is a buffer that is restricted to contain a
;;; Cluffer buffer.

(defclass standard-buffer (buffer)
  ((%cluffer-buffer :initarg :cluffer-buffer :reader cluffer-buffer)
   ;; We emulate Emacs a bit, in that we have a distinguished cursor
   ;; called the mark, and that is used (together with the cursor) to
   ;; define what a REGION of the buffer is.  When the slot contains
   ;; NIL, it means that the marks is not set.
   (%mark :initform nil :accessor mark)))

;;; A STANDARD-CURSOR is a subclass of a Cluffer RIGHT-STICKY-CURSOR.
;;; It contains a reference to the standard buffer.

(defclass standard-cursor (cluffer-standard-line:right-sticky-cursor)
  ((%buffer :initarg :buffer :reader buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on INSERT-ITEM.

(defmethod insert-item ((cursor cluffer:cursor) item)
  (cluffer-emacs:insert-item cursor item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on DELETE-ITEM.

(defmethod delete-item ((cursor cluffer:cursor))
  (cluffer-emacs:delete-item cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ERASE-ITEM.

(defmethod erase-item ((cursor cluffer:cursor))
  (cluffer-emacs:erase-item cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on FORWARD-ITEM.

(defmethod forward-item ((cursor cluffer:cursor))
  (cluffer-emacs:forward-item cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on BACKWARD-ITEM.

(defmethod backward-item ((cursor cluffer:cursor))
  (cluffer-emacs:backward-item cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on BEGINNING-OF-LINE.

(defmethod beginning-of-line ((cursor cluffer:cursor))
  (cluffer:beginning-of-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on END-OF-LINE.

(defmethod end-of-line ((cursor cluffer:cursor))
  (cluffer:end-of-line cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ITEM-BEFORE-CURSOR.

(defmethod item-before-cursor ((cursor cluffer:cursor))
  (cluffer-emacs:item-before-cursor cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on ITEM-AFTER-CURSOR.

(defmethod item-after-cursor ((cursor cluffer:cursor))
  (cluffer-emacs:item-after-cursor cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAKE-EMPTY-STANDARD-BUFFER.

(defun make-empty-standard-buffer-and-cursor ()
  (let* ((line (make-instance 'cluffer-standard-line:closed-line))
         (buffer (make-instance 'cluffer-standard-buffer:buffer
                                :initial-line line))
         (standard-buffer (make-instance 'standard-buffer
                                         :cluffer-buffer buffer))
         (cursor (make-instance 'standard-cursor :buffer standard-buffer)))
    (cluffer:attach-cursor cursor line)
    (values standard-buffer cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on FILL-BUFFER-FROM-STREAM.

(defmethod fill-buffer-from-stream ((cursor cluffer:cursor) stream)
  (loop for char = (read-char stream nil nil)
        until (null char)
        do (insert-item cursor char)))

;;; Return true if the mark is set in the buffer of CURSOR.
(defun mark-is-set-p (cursor)
  (not (null (mark (buffer cursor)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command SET-THE-MARK.

(defun set-the-mark (cursor)
  (let ((buffer (buffer cursor)))
    (setf (mark buffer)
          (make-instance 'standard-cursor
            :buffer buffer
            :line (cluffer:line cursor)
            :cursor-position (cluffer:cursor-position cursor)))))

;;; Unset the mark in the buffer of CURSOR.  If the mark is already
;;; unset, then do nothing.
(defun unset-the-mark (cursor)
  (when (mark-is-set-p cursor)
    (cluffer:detach-cursor (mark (buffer cursor)))
    (setf (mark (buffer cursor)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; EXCHANGE-CURSOR-AND-MARK.

(defun exchange-cursor-and-mark (cursor)
  (unless (mark-is-set-p cursor)
    (error 'mark-not-set))
  (let* ((cursor-line (cluffer:line cursor))
         (cursor-position (cluffer:cursor-position cursor))
         (buffer (buffer cursor))
         (mark (mark buffer))
         (mark-line (cluffer:line mark))
         (mark-position (cluffer:cursor-position mark)))
    (cluffer:detach-cursor cursor)
    (cluffer:detach-cursor mark)
    (cluffer:attach-cursor cursor mark-line mark-position)
    (cluffer:attach-cursor mark cursor-line cursor-position)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; KILL-REGION.

;;; Return true if and only if CURSOR1 is positioned before CURSOR2.
(defun cursor-< (cursor1 cursor2)
  (or (< (cluffer:line-number (cluffer:line cursor1))
         (cluffer:line-number (cluffer:line cursor2)))
      (and (= (cluffer:line-number (cluffer:line cursor1))
              (cluffer:line-number (cluffer:line cursor2)))
           (< (cluffer:cursor-position cursor1)
              (cluffer:cursor-position cursor2)))))

(defun cursor-= (cursor1 cursor2)
  (and (not (cursor-less cursor1 cursor2))
       (not (cursor-less cursor2 cursor1))))

(defun kill-region (cursor)
  (unless (mark-is-set-p cursor)
    (error 'mark-not-set))
  (let ((mark (mark (buffer cursor))))
    (when (cursor-< mark cursor)
      (exchange-cursor-and-mark cursor))
    (loop until (cursor-= cursor mark)
          do (cluffer-emacs:delete-item cursor))
    (unset-the-mark cursor)))
