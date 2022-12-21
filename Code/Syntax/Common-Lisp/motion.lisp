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
;;; Helper function.  It makes sure that every wad in the prefix
;;; precedes the cursor and that every wad in the suffix follows the
;;; cursor.  It is assumed that the cursor is positioned at the top
;;; level, so that it is not inside any wad.

(defun adjust-prefix-and-suffix-to-surround-cursor (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (loop until (null (prefix cache))
          while (position-less-or-equal
                 cursor-line-number
                 cursor-column-number
                 (start-line (first (prefix cache)))
                 (start-column (first (prefix cache))))
          do (push-to-suffix cache (pop-from-prefix cache)))
    (loop until (null (suffix cache))
          while (position-less
                 (start-line (first (suffix cache)))
                 (start-column (first (suffix cache)))
                 cursor-line-number
                 cursor-column-number)
          do (push-to-prefix cache (pop-from-suffix cache)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; FORWARD-EXPRESSION.

(define-condition no-following-expression (base:climacs-error)
  ()
  (:report "No following expression"))

(defun forward-top-level-expression (cache cursor)
  (adjust-prefix-and-suffix-to-surround-cursor cache cursor)
  (if (null (suffix cache))
      (error 'no-following-expression)
      (let ((wad (first (suffix cache))))
        (base:set-cursor-positions
         cursor
         (+ (start-line wad) (height wad))
         (end-column wad)))))

(defun cursor-is-inside-atomic-wad-p (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (find-wads-containing-position
             cache cursor-line-number cursor-column-number)))
      (and (not (null lines-and-wads))
           (let ((first-wad (cdr (first lines-and-wads))))
             (or (typep first-wad 'no-expression-wad)
                 (atom (expression first-wad))))))))

(defun forward-non-top-level-expression (parent-wad line-number cursor)
  (if (or (typep parent-wad 'no-expression-wad)
          (atom (expression parent-wad)))
      ;; We position the cursor at the end of the wad.
      (base:set-cursor-positions
       cursor
       (+ line-number (height parent-wad))
       (end-column parent-wad))
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
                      (error 'no-following-expression)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BACKWARD-EXPRESSION.

(define-condition no-preceding-expression (base:climacs-error)
  ()
  (:report "No preceding expression"))

(defun backward-top-level-expression (cache cursor)
  (adjust-prefix-and-suffix-to-surround-cursor cache cursor)
  (if (null (prefix cache))
      (error 'no-preceding-expression)
      (let ((wad (first (prefix cache))))
        (base:set-cursor-positions
         cursor (start-line wad) (start-column wad)))))

(defun backward-non-top-level-expression (parent-wad line-number cursor)
  (if (or (typep parent-wad 'no-expression-wad)
          (atom (expression parent-wad)))
      ;; We position the cursor at the beginning of the wad.
      (base:set-cursor-positions
       cursor line-number (start-column parent-wad))
      (multiple-value-bind (cursor-line-number cursor-column-number)
          (base:cursor-positions cursor)
        (let ((predecessor nil)
              (predecessor-line-number nil))
          (block found
            (mapwad (lambda (wad absolute-line-number)
                      (when (position-less-or-equal
                             cursor-line-number cursor-column-number
                             absolute-line-number (start-column wad))
                        ;; We found the first wad that follows the
                        ;; cursor position.
                        (return-from found))
                      ;; WAD is is still before the cursor, so prepare
                      ;; for a new iteration.
                      (setf predecessor wad
                            predecessor-line-number absolute-line-number))
                    (children parent-wad)
                    line-number))
          (if (null predecessor)
              (error 'no-preceding-expression)
              (base:set-cursor-positions
               cursor
               predecessor-line-number
               (start-column predecessor)))))))

(defun backward-expression (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (find-wads-containing-position
             cache cursor-line-number cursor-column-number)))
      (if (null lines-and-wads)
          (backward-top-level-expression cache cursor)
          (destructuring-bind (new-line-number . wad)
              (first lines-and-wads)
            (backward-non-top-level-expression wad new-line-number cursor))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; EXCHANGE-EXPRESSIONS.

(defun new-cursor ()
  (make-instance 'cluffer-standard-line:right-sticky-cursor))

(defun copy-cursor-positions (from-cursor to-cursor)
  (cluffer:attach-cursor
   to-cursor
   (cluffer:line from-cursor)
   (cluffer:cursor-position from-cursor)))

(defun detach-attached-cursors (c1 c2 c3 c4)
  (flet ((detach-if-attached (cursor)
           (when (cluffer:cursor-attached-p cursor)
             (cluffer:detach-cursor cursor))))
    (detach-if-attached c1)
    (detach-if-attached c2)
    (detach-if-attached c3)
    (detach-if-attached c4)))

(defun exchange-expressions (cache cursor)
  ;; If the cursor is inside an atomic wad, we first move
  ;; forward over that expression.
  (when (cursor-is-inside-atomic-wad-p cache cursor)
    (forward-expression cache cursor))
  (let ((c1 (new-cursor))
        (c2 (new-cursor))
        (c3 (new-cursor))
        (c4 (new-cursor)))
    (flet ((ensure-cursors-detached ()
             (detach-attached-cursors c1 c2 c3 c4)))
      ;; We first try to position C1 before the preceding expression.
      (copy-cursor-positions cursor c1)
      ;; Try to move backward over the preceding expression.  It is
      ;; possible that this operation might fail if there is no
      ;; preceding expression.  If that happens, we handle the error,
      ;; restore everything, and re-signal the condition.
      (handler-case (backward-expression cache c1)
        (no-preceding-expression (condition)
          (ensure-cursors-detached)
          (error condition)))
      ;; Next, we try to position C4 at the end of the following
      ;; expression.
      (copy-cursor-positions cursor c4)
      ;; Try to move forward over the next expression.  It is possible
      ;; that this operation might fail if there is no next
      ;; expression.  If that happens, we handle the error, restore
      ;; everything, and re-signal the condition.
      (handler-case (forward-expression cache c4)
        (no-following-expression (condition)
          (ensure-cursors-detached)
          (error condition)))
      ;; Come here if everything went well.  Before positioning C2 and
      ;; C3, we make sure that CURSOR is not exactly at the end of the
      ;; preceding expression not exactly at the beginning of the
      ;; following expression.  That way, we can freely insert at
      ;; those positions without risking that CURSOR will move as
      ;; well.
      (base:insert-item cursor #\Space)
      (base:insert-item cursor #\Space)
      (base:backward-item cursor)
      ;; We put C2 at the end of the expression we moved backward
      ;; over.
      (copy-cursor-positions c1 c2)
      (forward-expression cache c2)
      ;; We put C3 at the beginning of the expression we moved forward
      ;; over.
      (copy-cursor-positions c4 c3)
      (backward-expression cache c3)
      ;; Swap the contents of the two regions.
      (let ((preceding-expression (base:move-region-to-vector c1 c2))
            (following-expression (base:move-region-to-vector c3 c4)))
        (loop for item across preceding-expression
              do (base:insert-item c3 item))
        (loop for item across following-expression
              do (base:insert-item c1 item)))
      ;; Remove the additional spaces we inserted.
      (base:erase-item cursor)
      (base:delete-item cursor))))
