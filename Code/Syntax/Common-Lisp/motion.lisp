(cl:in-package #:second-climacs-syntax-common-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command UP-EXPRESSION.

(define-condition already-at-top-level (base:climacs-error)
  ()
  (:report "Already at top level"))

(defun up-expression (cache cursor)
  (let* ((current (multiple-value-call #'ip:find-wads-containing-position cache
                    (base:cursor-positions cursor)))
         (parent  (ip:parent current)))
    (if (null current)
        (if (null parent)
            (error 'already-at-top-level)
            (base:set-cursor-positions
             cursor (ip:absolute-start-line parent) (ip:start-column parent)))
        (base:set-cursor-positions
         cursor (ip:absolute-start-line current) (ip:start-column current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; FORWARD-EXPRESSION.

(define-condition no-following-expression (base:climacs-error)
  ()
  (:report "No following expression"))

(defun cursor-is-inside-atomic-wad-p (cache cursor)
  (let ((lines-and-wads
          (multiple-value-call #'ip:find-wads-containing-position cache
            (base:cursor-positions cursor))))
    (and (not (null lines-and-wads))
         (let ((first-wad (cdr (first lines-and-wads))))
           (or (typep first-wad 'ip:no-expression-wad)
               (atom (ip:expression first-wad)))))))

(defun forward-expression (cache cursor)
  (let* ((current (multiple-value-call #'ip:find-wads-containing-position cache
                    (base:cursor-positions cursor)))
         (next    (ip:right-sibling current)))
    (if (null current)
        (if (null next)
            (error 'no-following-expression)
            (base:set-cursor-positions
             cursor (ip:end-line next) (ip:end-column next)))
        (base:set-cursor-positions
         cursor (ip:end-line current) (ip:end-column current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BACKWARD-EXPRESSION.

(define-condition no-preceding-expression (base:climacs-error)
  ()
  (:report "No preceding expression"))

(defun backward-expression (cache cursor)
  (let* ((current  (multiple-value-call #'ip:find-wads-containing-position cache
                     (base:cursor-positions cursor)))
         (previous (ip:left-sibling current)))
    (if (null current)
        (if (null previous)
            (error 'no-following-expression)
            (base:set-cursor-positions
             cursor (ip:absolute-start-line previous) (ip:start-column previous)))
        (base:set-cursor-positions
         cursor (ip:absolute-start-line current) (ip:start-column current)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; BEGINNING-OF-TOP-LEVEL-EXPRESSIONS.

(defun beginning-of-top-level-expression (cache cursor)
  (let ((lines-and-wads
          (multiple-value-call #'ip:find-wads-containing-position cache
            (base:cursor-positions cursor))))
    (if (null lines-and-wads)
        (backward-expression cache cursor)
        (destructuring-bind (new-line-number . wad)
            (first (last lines-and-wads))
          (base:set-cursor-positions
           cursor new-line-number (ip:start-column wad))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This function implements the essence of the command
;;; END-OF-TOP-LEVEL-EXPRESSIONS.

(defun end-of-top-level-expression (cache cursor)
  (let ((lines-and-wads
          (multiple-value-call #'ip:find-wads-containing-position cache
            (base:cursor-positions cursor))))
    (if (null lines-and-wads)
        (forward-expression cache cursor)
        (destructuring-bind (new-line-number . wad)
            (first (last lines-and-wads))
          (base:set-cursor-positions
           cursor
           (+ new-line-number (ip:height wad))
           (ip:end-column wad))))))
