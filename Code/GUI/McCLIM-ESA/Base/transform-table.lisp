(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table transform-table)

(defmacro define-transform-command (name (&rest extra-parameters) &body body)
  `(define-buffer-command (,name transform-table) ,extra-parameters
     ,@body))

;;; Changing case

(define-transform-command com-change-case
    (&key (unit      'unit)
          (direction 'direction)
          (case      '(member :down :up :capital)))
  (edit:perform buffer 'edit:change-case unit direction case))
(define-command-specializations (transform-table com-change-case)
  ;; Buffer structure units
  (com-downcase-item-forward         (:unit edit:item           :direction :forward  :case :down))
  (com-downcase-item-backward        (:unit edit:item           :direction :backward :case :down))
  (com-upcase-item-forward           (:unit edit:item           :direction :forward  :case :up))
  (com-upcase-item-backward          (:unit edit:item           :direction :backward :case :up))
  (com-capitalize-item-forward       (:unit edit:item           :direction :forward  :case :capital))
  (com-capitalize-item-backward      (:unit edit:item           :direction :backward :case :capital))
  ;; Prose units
  (com-downcase-word-forward         (:unit edit:word           :direction :forward  :case :down)    (#\l :meta))
  (com-downcase-word-backward        (:unit edit:word           :direction :backward :case :down))
  (com-upcase-word-forward           (:unit edit:word           :direction :forward  :case :up)      (#\u :meta))
  (com-upcase-word-backward          (:unit edit:word           :direction :backward :case :up))
  (com-capitalize-word-forward       (:unit edit:word           :direction :forward  :case :capital) (#\c :meta))
  (com-capitalize-word-backward      (:unit edit:word           :direction :backward :case :capital))

  (com-downcase-paragraph-forward    (:unit edit:paragraph      :direction :forward  :case :down))
  (com-upcase-paragraph-forward      (:unit edit:paragraph      :direction :forward  :case :up))
  (com-capitalize-paragraph-forward  (:unit edit:paragraph      :direction :forward  :case :capital))
  ;; Expression
  (com-downcase-expression-forward   (:unit edit.exp:expression :direction :forward  :case :down))
  (com-upcase-expression-forward     (:unit edit.exp:expression :direction :forward  :case :up))
  (com-capitalize-expression-forward (:unit edit.exp:expression :direction :forward  :case :capital)))

;;; Transposing

(define-transform-command com-transpose (&key (unit      'unit)
                                              (direction 'direction))
  (edit:perform buffer 'edit:transpose unit direction))
(define-command-specializations (transform-table com-transpose)
  ;; Prose units
  (com-transpose-items (:unit edit:item :direction :forward) (#\t :control))
  (com-transpose-words (:unit edit:word :direction :forward) (#\t :meta))
  (com-transpose-lines (:unit edit:line :direction :forward))
  ;; Expression
  (com-transpose-expressions
   (:unit edit.exp:expression :direction :forward) (#\t :control :meta)))

;;; Structure editing

(define-transform-command com-raise (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit.exp:raise unit direction))
(define-command-specializations (transform-table com-raise)
  (com-raise-expression-forward
   (:unit edit.exp:expression :direction :forward) (#\r :meta))
  (com-raise-expression-backward
   (:unit edit.exp:expression :direction :backward)))

(define-transform-command com-splice (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit.exp:splice unit direction))
(define-command-specializations (transform-table com-splice)
  (com-splice-expression-forward
   (:unit edit.exp:expression :direction :forward) (#\s :meta))
  (com-splice-expression-backward
   (:unit edit.exp:expression :direction :backward)))

(define-transform-command com-split (&key (unit 'unit))
  (edit:perform buffer 'edit.exp:split unit))
(define-command-specializations (transform-table com-split)
  (com-split-expression
   (:unit edit.exp:expression) (#\S :meta))
  (com-split-toplevel-expression
   (:unit edit.exp:toplevel-expression) (#\S :meta :control)))

(define-transform-command com-join (&key (unit 'unit))
  (edit:perform buffer 'edit.exp:join unit))
(define-command-specializations (transform-table com-join)
  (com-join-expressions (:unit edit.exp:expression) (#\J :meta)))

(define-transform-command com-eject-expression (&key (unit      'unit)
                                                     (direction 'direction))
  (edit:perform buffer 'edit.exp:eject unit direction))
(define-command-specializations (transform-table com-eject-expression)
  (com-eject-expression-forward
   (:unit edit.exp:expression :direction :forward) (:left :control))
  (com-eject-expression-backward
   (:unit edit.exp:expression :direction :backward)))

(define-transform-command com-absorb-expression (&key (unit      'unit)
                                                      (direction 'direction))
  (edit:perform buffer 'edit.exp:absorb unit direction))
(define-command-specializations (transform-table com-absorb-expression)
  (com-absorb-expression-forward
   (:unit edit.exp:expression :direction :forward) (:right :control))
  (com-absorb-expression-backward
   (:unit edit.exp:expression :direction :backward)))

;;; Align

(defun align-cursors (buffer &key (fill-element #\Space))
  (let ((max-column 0))
    (edit:map-sites (lambda (site)
                      (alexandria:maxf
                       max-column
                       (cluffer:cursor-position (edit:point site))))
                    buffer)
    (edit:map-sites
     (lambda (site)
       (let* ((point  (edit:point site))
              (column (cluffer:cursor-position point))
              (count  (- max-column column))
              (string (make-string count :initial-element fill-element)))
         (edit:insert-items point string)))
     buffer)))

(define-buffer-command (com-align-cursors transform-table)
    (&key (fill-element 'character :default #\Space))
  (align-cursors buffer :fill-element fill-element))

;;; Refactoring

;;; Silly little demo

(defun extract-as-function (cursor unit direction name)
  ;; TODO find free variables, turn into parameter and arguments
  (edit:with-cloned-cursor (insert cursor)
    (let ((expression (edit:items cursor unit direction)))
      (edit:move insert edit.exp:toplevel-expression direction)
      (edit:insert-items insert #.(format nil "~2%"))
      (edit:insert-items insert (format nil "(defun ~A ()~%" name))
      (edit:insert-items insert expression)
      (edit:insert-items insert (format nil ")~%"))
      ;;
      (setf (edit:items cursor unit direction) (format nil "(~A)" name)))))

(define-transform-command com-extract-as-function
    ((name 'string :prompt "function name")
     &key (unit      'unit      :default edit.exp:region-or-expression)
          (direction 'direction :default :forward))
  (edit:perform buffer 'extract-as-function unit direction name))
