(cl:in-package #:second-climacs-base)

;;; A STANDARD-BUFFER is a buffer that is restricted to contain a
;;; Cluffer buffer.

(defclass standard-buffer (buffer)
  ((%cluffer-buffer :initarg :cluffer-buffer :reader cluffer-buffer)
   ;; We emulate Emacs a bit, in that we have a distinguished cursor
   ;; called the mark, and that is used (together with the cursor) to
   ;; define what a REGION of the buffer is. When the slot contains NIL,
   ;; it means that the marks is not set.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command SET-THE-MARK.

(defun set-the-mark (cursor)
  (let* ((buffer (buffer cursor))
         (line (cluffer:line cursor))
         (position (cluffer:cursor-position cursor))
         (mark (make-instance 'standard-cursor
                 :buffer buffer)))
    (setf (mark buffer) mark)
    (cluffer:attach-cursor mark line position)))

;;; Unset the mark in the buffer of CURSOR.  If the mark is already
;;; unset, then do nothing.
(defun unset-the-mark (cursor)
  (when (mark-is-set-p cursor)
    (cluffer:detach-cursor (mark (buffer cursor)))
    (setf (mark (buffer cursor)) nil)))

;;; Return the mark of the buffer of CURSOR.
(defun find-mark (cursor)
  (mark (buffer cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; EXCHANGE-CURSOR-AND-MARK.

(defun exchange-cursor-and-mark (cursor)
  (unless (mark-is-set-p cursor)
    (error 'mark-not-set))
  (let ((mark (find-mark cursor)))
    (multiple-value-bind (cursor-line-number cursor-column-number)
        (cursor-positions cursor)
      (multiple-value-bind (mark-line-number mark-column-number)
          (cursor-positions mark)
        (set-cursor-positions
          cursor mark-line-number mark-column-number)
        (set-cursor-positions
          mark cursor-line-number cursor-column-number)))))

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
  (and (not (cursor-< cursor1 cursor2))
       (not (cursor-< cursor2 cursor1))))

(defun move-region-to-vector (cursor1 cursor2)
  (assert (or (cursor-< cursor1 cursor2) (cursor-= cursor1 cursor2)))
  (loop with result = (make-array 0 :adjustable t :fill-pointer t)
        until (cursor-= cursor1 cursor2)
        do (vector-push-extend (item-after-cursor cursor1) result)
           (delete-item cursor1)
        finally (return result)))

(defun kill-region (cursor)
  (unless (mark-is-set-p cursor)
    (error 'mark-not-set))
  (let ((mark (mark (buffer cursor))))
    (when (cursor-< mark cursor)
      (exchange-cursor-and-mark cursor))
    (add-kill-ring-region)
    (add-kill-ring-line)
    (loop until (cursor-= cursor mark)
          do (let ((item (item-after-cursor cursor)))
               (if (eql item #\Newline)
                   (add-kill-ring-line)
                   (add-kill-ring-item item)))
             (cluffer-emacs:delete-item cursor))
    (unset-the-mark cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command UNKILL

(defun unkill (cursor)
  (if (kill-ring-is-empty-p)
      (error 'kill-ring-is-empty)
      (flet ((insert-line (line)
               (loop for item across line
                     do (insert-item cursor item))))
        (let ((last (last-element *kill-ring*)))
          (loop for i from 0 below (1- (length last))
                do (insert-line (aref last i))
                   (insert-item cursor #\Newline))
          (insert-line (last-element last))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These functions implement the essence of the commands
;;; NEXT-LINE and PREVIOUS-LINE.

(defvar *target-column*)

(defun next-line (cursor)
  (let ((buffer (cluffer:buffer cursor))
        (line-number (cluffer:line-number (cluffer:line cursor))))
    (if (= line-number (1- (cluffer:line-count buffer)))
        (error 'cluffer:end-of-buffer)
        (let* ((next-line (cluffer:find-line buffer (1+ line-number)))
               (item-count (cluffer:item-count next-line)))
          (unless (member (car (esa:previous-command (esa:current-window)))
                          '(com-next-line com-previous-line))
            (setf *target-column* (cluffer:cursor-position cursor)))
          (cluffer:detach-cursor cursor)
          (cluffer:attach-cursor cursor
                                 next-line
                                 (min item-count *target-column*))))))

(defun previous-line (cursor)
  (let ((buffer (cluffer:buffer cursor))
        (line-number (cluffer:line-number (cluffer:line cursor))))
    (if (zerop line-number)
        (error 'cluffer:beginning-of-buffer)
        (let* ((previous-line (cluffer:find-line buffer (1- line-number)))
               (item-count (cluffer:item-count previous-line)))
          (unless (member (car (esa:previous-command (esa:current-window)))
                          '(com-next-line com-previous-line))
            (setf *target-column* (cluffer:cursor-position cursor)))
          (cluffer:detach-cursor cursor)
          (cluffer:attach-cursor cursor
                                 previous-line
                                 (min item-count *target-column*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These functions implement the essence of the commands
;;; BEGINNING-OF-BUFFER and END-OF-BUFFER.

(defun beginning-of-buffer (cursor)
  (let* ((buffer (cluffer:buffer cursor))
         (first-line (cluffer:find-line buffer 0)))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor first-line)))

(defun end-of-buffer (cursor)
  (let* ((buffer (cluffer:buffer cursor))
         (line-count (cluffer:line-count buffer))
         (last-line (cluffer:find-line buffer (1- line-count)))
         (item-count (cluffer:item-count last-line)))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor last-line item-count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These functions implement the essence of the commands
;;; FORWARD-WORD and BACKWARD-WORD.

(defun apply-to-cursor-until (cursor action cursor-predicate item-key item-predicate)
  (loop until (or (funcall cursor-predicate cursor)
                  (funcall item-predicate (funcall item-key cursor)))
        do (funcall action cursor)))

(defun %act-over-word (cursor action cursor-predicate
                       &key (item-key #'item-after-cursor))
  (apply-to-cursor-until cursor action cursor-predicate item-key
                         #'alphanumericp)
  (apply-to-cursor-until cursor action cursor-predicate item-key
                         (lambda (item)
                           (or (not (characterp item))
                               (not (alphanumericp item))))))

(defun forward-word (cursor)
  (%act-over-word cursor #'forward-item #'cluffer:end-of-buffer-p))

(defun backward-word (cursor)
  (%act-over-word cursor #'backward-item #'cluffer:beginning-of-buffer-p
                  :item-key #'item-before-cursor))

(defun delete-word (cursor)
  (%act-over-word cursor #'delete-item #'cluffer:end-of-buffer-p))

(defun erase-word (cursor)
  (%act-over-word cursor #'erase-item #'cluffer:beginning-of-buffer-p
                  :item-key #'item-before-cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BACK-TO-INDENTATION.

(defun back-to-indentation (cursor)
  (cluffer:beginning-of-line cursor)
  (loop until (or (cluffer:end-of-line-p cursor)
                  (not (member (cluffer:item-after-cursor cursor)
                               '(#\Space #\Tab #\Backspace #\Page))))
        do (cluffer:forward-item cursor)))

(defun delete-indentation (cursor)
  ;; Join current and previous lines.
  (beginning-of-line cursor)
  (erase-item cursor)
  ;; Replace all whitespace on the joined line before and after the
  ;; cursor with a single space.
  (loop for item = (item-before-cursor cursor)
        while (member item '(#\Space #\Tab))
        do (erase-item cursor))
  (loop for item = (item-after-cursor cursor)
        while (member item '(#\Space #\Tab))
        do (delete-item cursor))
  (insert-item cursor #\Space)
  (backward-item cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filling

(defun whitespacep (character)
  (member character '(#\Space #\Newline)))

(defun punctuationp (character)
  (member character '(#\. #\? #\! #\: #\, #\; #\( #\) #\" #\-)))

(defparameter *fill-column* 72)

(defun insert-items (cursor items)
  (map nil (alexandria:curry #'insert-item cursor) items))

(defun insert-words-fill (cursor words &key prefix
                                            suffix
                                            per-line-prefix
                                            (fill-column    *fill-column*))
  (flet ((insert-word (word)
           (unless (and (= (length word) 1)
                        (let ((first (aref word 0)))
                          (or (whitespacep first) (punctuationp first))))
             (insert-item cursor #\Space))
           (insert-items cursor word)))
    (loop for word in words
          for firstp = t then nil
          do (cond (firstp
                    (insert-items cursor prefix))
                   ((>= (+ (cluffer:cursor-position cursor) (length word))
                        fill-column)
                    (insert-item cursor #\Newline)
                    (insert-items cursor per-line-prefix)))
             (insert-word word))
    (insert-item cursor #\Newline)
    (insert-items cursor suffix)))

(defun fill-words (start-cursor end-cursor words
                   &rest args &key prefix suffix per-line-prefix fill-column)
  (declare (ignore suffix per-line-prefix fill-column))
  (multiple-value-bind (start-line-number start-column-number)
      (cursor-positions start-cursor)
    (loop until (cursor-= start-cursor end-cursor)
          do (delete-item start-cursor))
    (apply #'insert-words-fill start-cursor words args)
    (let ((final-column (+ start-column-number (length prefix))))
      (set-cursor-positions start-cursor start-line-number final-column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; DELETE-TRAILING-SPACES.

(defun delete-trailing-spaces (cursor)
  (let* ((buffer (buffer cursor))
         (cluffer-buffer (cluffer-buffer buffer))
         (new-cursor (make-instance 'standard-cursor
                       :buffer buffer))
         (line (cluffer:find-line cluffer-buffer 0)))
    (cluffer:attach-cursor new-cursor line 0)
    (end-of-line new-cursor)
    (loop until (cluffer:end-of-buffer-p new-cursor)
          do (loop while (and (not (cluffer:beginning-of-line-p new-cursor))
                              (eql (item-before-cursor new-cursor) #\Space))
                   do (erase-item new-cursor))
             (beginning-of-line new-cursor)
             (next-line new-cursor)
             (end-of-line new-cursor))
    (cluffer:detach-cursor new-cursor)))
