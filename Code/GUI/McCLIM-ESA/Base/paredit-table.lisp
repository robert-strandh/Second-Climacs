(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table paredit-table)

(defmacro define-paredit-command (name (&rest extra-parameters) &body body)
  `(define-buffer-command (,name paredit-table) ,extra-parameters
     ,@body))

;;; Delimiter pairs

(define-paredit-command com-insert-delimiter-pair
    ((opening 'character))
  (edit:perform buffer 'edit:insert-delimiter-pair opening))
(define-command-specializations (paredit-table com-insert-delimiter-pair)
  (com-insert-parentheses-pair (#\() #\())

(define-paredit-command com-move-past-closing-delimiter
    ((closing 'character))
  (edit:perform buffer 'edit:move-past-closing-delimiter closing))
(bind-key 'paredit-table #\) 'com-move-past-closing-delimiter #\))

(define-paredit-command
    com-move-past-closing-delimiter-or-insert-delimiter-pair
    ((opening 'character))
  (edit:perform
   buffer 'edit:move-past-closing-delimiter-or-insert-delimiter-pair opening))
(define-command-specializations
    (paredit-table com-move-past-closing-delimiter-or-insert-delimiter-pair)
  (com-move-past-closing-double-quote-or-insert-double-quote-pair
   (#\") #\")
  (com-move-past-closing-vertical-line-or-insert-vertical-line-pair
   (#\|) #\|))

(define-paredit-command com-delete-delimiter-pair-or-item
    (&key (direction 'direction))
  (edit:perform
   buffer 'edit:delete-delimiter-pair-or-item direction
   :if-not-empty (lambda (cursor obstacle)
                   (ecase obstacle
                     (:outside
                      (edit:move cursor edit:item direction))
                     (:inside
                      (let ((direction (edit:opposite-direction direction)))
                        (edit:delete cursor edit:item direction)))))))
(define-command-specializations
    (paredit-table com-delete-delimiter-pair-or-item)
  (com-delete-delimiter-pair-or-item-forward
   (:direction :forward) (#\d :control))
  (com-delete-delimiter-pair-or-item-backward
   (:direction :backward) #\Backspace))

(define-paredit-command com-delete-semi-line-or-expression
    (&key (direction 'direction))
  (edit:perform buffer 'edit.exp:delete-semi-line-or-expressions direction))
(define-command-specializations
    (paredit-table com-delete-semi-line-or-expression)
  (com-delete-semi-line-or-expression-forward
   (:direction :forward) (#\k :control))
  #+later (com-delete-semi-line-or-expression-backward
   (:direction :backward)))

(define-paredit-command com-surround-with-delimiter-pair
    (&key (unit      'unit)
          (direction 'direction)
          (opening   'character)
          (count     '(integer 1) :default 1))
  (edit:perform buffer 'edit:surround-with-delimiter-pair
                unit direction opening :count count))
(define-command-specializations (paredit-table com-surround-with-delimiter-pair)
  (com-surrround-expression-with-left-parenthesis
   (:unit edit.exp:expression :direction :forward :opening #\() (#\( :meta))
  (com-surrround-expression-with-double-quote
   (:unit edit.exp:expression :direction :forward :opening #\") (#\" :meta))
  (com-surrround-expression-with-vertical-line
   (:unit edit.exp:expression :direction :forward :opening #\|) (#\| :meta)))
