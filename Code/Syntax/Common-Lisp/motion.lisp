(cl:in-package #:second-climacs-syntax-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command UP-EXPRESSION.

(define-condition already-at-top-level (base:climacs-error)
  ()
  (:report "Already at top level"))

(defun up-expression (cache cursor)
  (multiple-value-bind (current parent previous next)
      (ip:compute-wad-descriptors cache cursor)
    (declare (ignore previous next))
    (if (null current)
        (if (null parent)
            (error 'already-at-top-level)
            (base:set-cursor-positions
             cursor (ip:start-line-number parent) (ip:start-column-number parent)))
        (base:set-cursor-positions
         cursor (ip:start-line-number current) (ip:start-column-number current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; FORWARD-EXPRESSION.

(define-condition no-following-expression (base:climacs-error)
  ()
  (:report "No following expression"))

(defun cursor-is-inside-atomic-wad-p (cache cursor)
  (multiple-value-bind (cursor-line-number cursor-column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (ip:find-wads-containing-position
             cache cursor-line-number cursor-column-number)))
      (and (not (null lines-and-wads))
           (let ((first-wad (cdr (first lines-and-wads))))
             (or (typep first-wad 'ip:no-expression-wad)
                 (atom (ip:expression first-wad))))))))

(defun forward-expression (cache cursor)
  (multiple-value-bind (current parent previous next)
      (ip:compute-wad-descriptors cache cursor)
    (declare (ignore parent previous))
    (if (null current)
        (if (null next)
            (error 'no-following-expression)
            (base:set-cursor-positions
             cursor (ip:end-line-number next) (ip:end-column-number next)))
        (base:set-cursor-positions
         cursor (ip:end-line-number current) (ip:end-column-number current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BACKWARD-EXPRESSION.

(define-condition no-preceding-expression (base:climacs-error)
  ()
  (:report "No preceding expression"))

(defun backward-expression (cache cursor)
  (multiple-value-bind (current parent previous next)
      (ip:compute-wad-descriptors cache cursor)
    (declare (ignore parent next))
    (if (null current)
        (if (null previous)
            (error 'no-following-expression)
            (base:set-cursor-positions
             cursor (ip:start-line-number previous) (ip:start-column-number previous)))
        (base:set-cursor-positions
         cursor (ip:start-line-number current) (ip:start-column-number current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; EXCHANGE-EXPRESSIONS.

(defun new-cursor (buffer)
  (make-instance 'base:standard-cursor :buffer buffer))

(defun copy-cursor-positions (from-cursor to-cursor)
  (cluffer:attach-cursor
   to-cursor
   (cluffer:line from-cursor)
   (cluffer:cursor-position from-cursor)))

(defun detach-attached-cursors (&rest cursors)
  (flet ((detach-if-attached (cursor)
           (when (cluffer:cursor-attached-p cursor)
             (cluffer:detach-cursor cursor))))
    (mapc #'detach-if-attached cursors)))

(defun exchange-expressions (cache cursor)
  ;; If the cursor is inside an atomic wad, we first move
  ;; forward over that expression.
  (when (cursor-is-inside-atomic-wad-p cache cursor)
    (forward-expression cache cursor))
  (let ((c1 (new-cursor (base:buffer cursor)))
        (c2 (new-cursor (base:buffer cursor)))
        (c3 (new-cursor (base:buffer cursor)))
        (c4 (new-cursor (base:buffer cursor))))
    (unwind-protect
         (progn
           ;; We first try to position C1 before the preceding expression.
           (copy-cursor-positions cursor c1)
           ;; Try to move backward over the preceding expression.  It is
           ;; possible that this operation might fail if there is no
           ;; preceding expression.  If that happens, we handle the error,
           ;; restore everything, and re-signal the condition.
           (backward-expression cache c1)
           ;; Next, we try to position C4 at the end of the following
           ;; expression.
           (copy-cursor-positions cursor c4)
           ;; Try to move forward over the next expression.  It is possible
           ;; that this operation might fail if there is no next
           ;; expression.  If that happens, we handle the error, restore
           ;; everything, and re-signal the condition.
           (forward-expression cache c4)
           ;; Come here if everything went well.
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
                   do (base:insert-item c1 item))))
      (detach-attached-cursors c1 c2 c3 c4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BEGINNING-OF-TOP-LEVEL-EXPRESSIONS.

(defun beginning-of-top-level-expression (cache cursor)
  (multiple-value-bind (line-number column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (ip:find-wads-containing-position cache line-number column-number)))
      (if (null lines-and-wads)
          (backward-expression cache cursor)
          (destructuring-bind (new-line-number . wad)
              (first (last lines-and-wads))
            (base:set-cursor-positions
             cursor new-line-number (ip:start-column wad)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; END-OF-TOP-LEVEL-EXPRESSIONS.

(defun end-of-top-level-expression (cache cursor)
  (multiple-value-bind (line-number column-number)
      (base:cursor-positions cursor)
    (let ((lines-and-wads
            (ip:find-wads-containing-position cache line-number column-number)))
      (if (null lines-and-wads)
          (forward-expression cache cursor)
          (destructuring-bind (new-line-number . wad)
              (first (last lines-and-wads))
            (base:set-cursor-positions
             cursor
             (+ new-line-number (ip:height wad))
             (ip:end-column wad)))))))
