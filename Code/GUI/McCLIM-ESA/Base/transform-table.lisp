(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table transform-table)

;;; Changing case

(define-buffer-command (com-change-case transform-table)
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

(define-buffer-command (com-transpose transform-table)
    (&key (unit      'unit)
          (direction 'direction))
  (edit:perform buffer 'edit:transpose unit direction))

(define-command-specializations (transform-table com-transpose)
  ;; Prose units
  (com-transpose-items (:unit edit:item :direction :forward) (#\t :control))
  (com-transpose-words (:unit edit:word :direction :forward) (#\t :meta))
  (com-transpose-lines (:unit edit:line :direction :forward))
  ;; Expression
  (com-transpose-expressions (:unit edit.exp:expression :direction :forward) (#\t :control :meta)))

;;; Structure editing operations

(define-buffer-command (com-raise transform-table)
    (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit.exp:raise unit direction))
(define-command-specializations (transform-table com-raise)
  (com-raise-expression-forward  (:unit edit.exp:expression :direction :forward)  (#\r :meta))
  (com-raise-expression-backward (:unit edit.exp:expression :direction :backward)))

(define-buffer-command (com-splice transform-table)
    (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit.exp:splice unit direction))
(define-command-specializations (transform-table com-splice)
  (com-splice-expression-forward  (:unit edit.exp:expression :direction :forward)  (#\s :meta))
  (com-splice-expression-backward (:unit edit.exp:expression :direction :backward)))

(define-buffer-command (com-split transform-table) (&key (unit 'unit))
  (edit:perform buffer 'edit.exp:split unit))
(define-command-specializations (transform-table com-split)
  (com-split-expression          (:unit edit.exp:expression)          (#\S :meta))
  (com-split-toplevel-expression (:unit edit.exp:toplevel-expression) (#\S :meta :control)))

(define-buffer-command (com-join transform-table) (&key (unit 'unit))
  (edit:perform buffer 'edit.exp:join unit))
(define-command-specialization (transform-table com-join-expressions com-join (#\J :meta))
  :unit edit.exp:expression)
