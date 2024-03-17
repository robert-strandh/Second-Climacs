(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table motion-table)

(defmacro define-motion-command (name (&rest extra-parameters) &body body)
  `(define-buffer-command (,name motion-table) ,extra-parameters
     ,@body))

;;; View

(clim:define-command (com-scroll :command-table motion-table)
    ((amount 'integer))
  (let ((pane   (esa:current-window))
        (buffer (current-buffer)))
    (scroll pane amount 0)
    (unless (> (edit:site-count buffer) 1)
      (move-cursor-to-viewport pane))))
(define-command-specializations (motion-table com-scroll)
  (com-scroll-down ( 10) (#\v :control))
  (com-scroll-up   (-10) (#\v :meta)))

;;; Basic movement

(define-motion-command com-move (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit:move unit direction))

(define-command-specializations (motion-table com-move)
  ;; Buffer structure units
  (com-forward-item                 (:unit edit:item            :direction :forward)   (#\f :control))
  (com-backward-item                (:unit edit:item            :direction :backward)  (#\b :control))
  (com-end-of-line                  (:unit edit:line-boundary   :direction :forward)   (#\e :control))
  (com-beginning-of-line            (:unit edit:line-boundary   :direction :backward)  (#\a :control))
  (com-next-line                    (:unit edit:line            :direction :forward)   (#\n :control))
  (com-previous-line                (:unit edit:line            :direction :backward)  (#\p :control))
  (com-end-of-buffer                (:unit edit:buffer-boundary :direction :forward)   (#\> :meta))
  (com-beginning-of-buffer          (:unit edit:buffer-boundary :direction :backward)  (#\< :meta))
  ;; Prose units
  (com-forward-word                 (:unit edit:word            :direction :forward)   (#\f :meta))
  (com-backward-word                (:unit edit:word            :direction :backward)  (#\b :meta))
  (com-forward-sentence             (:unit edit:sentence        :direction :forward)   (#\e :meta))
  (com-backward-sentence            (:unit edit:sentence        :direction :backward)  (#\a :meta))
  (com-forward-paragraph            (:unit edit:paragraph       :direction :forward)   (#\} :meta))
  (com-backward-paragraph           (:unit edit:paragraph       :direction :backward)  (#\{ :meta)))

(define-motion-command com-back-to-indentation ()
  (edit:perform buffer 'edit:back-to-indentation))
(bind-key 'motion-table '(#\m :meta) 'com-back-to-indentation)

(define-motion-command com-set-the-mark ()
  (edit:perform buffer 'edit:set-mark-or-toggle-active))
(bind-key 'motion-table '(#\Space :control) 'com-set-the-mark)

(define-motion-command com-exchange-cursor-and-mark ()
  (edit:perform buffer 'edit:exchange-point-and-mark))
(esa:set-key `(com-exchange-cursor-and-mark)
             'motion-table
             '((#\x :control) (#\x :control)))

(define-motion-command com-mark-object (&key (unit      'unit)
                                             (direction 'direction))
  (edit:perform buffer 'edit:mark-object unit direction))
(define-command-specializations (motion-table com-mark-object)
  (com-mark-word       (:unit edit:word       :direction :forward) (#\@ :meta))
  (com-mark-sentence   (:unit edit:sentence   :direction :forward))
  (com-mark-paragraph  (:unit edit:paragraph  :direction :forward) (#\h :meta)))

;;; TODO different table

(define-motion-command com-add-cursor (&key (unit      'unit)
                                            (direction 'direction))
  (edit:push-site-relative buffer unit direction))
(bind-key 'motion-table '(#\+ :meta) 'com-add-cursor :unit edit:line :direction :forward)

(clim:define-command (com-add-cursor-at-pointer :command-table motion-table)
    (&key (buffer buffer :default (current-buffer))
          (event  t))
  (let* ((sheet         (clim:event-sheet event))
         (x             (clim:pointer-event-x event))
         (y             (clim:pointer-event-y event))
         (text-style    (clim:pane-text-style sheet))
         (line-number   (floor y (clim:text-style-height text-style sheet)))
         (column-number (floor x (clim:text-style-width text-style sheet))))
    (edit:push-site-at buffer line-number column-number)))

(clim:define-gesture-name :add-cursor :pointer-button-press
  (:left :shift :control))

(clim:define-presentation-to-command-translator blank-area->com-add-cursor-at-pointer
    (clim:blank-area com-add-cursor-at-pointer motion-table :gesture :add-cursor)
    (event)
  (list :event event))

(clim:define-command (com-move-to-pointer :command-table motion-table)
    (&key (buffer buffer :default (current-buffer))
          (event  t))
  (let* ((sheet         (clim:event-sheet event))
         (x             (clim:pointer-event-x event))
         (y             (clim:pointer-event-y event))
         (text-style    (clim:pane-text-style sheet))
         (line-number   (floor y (clim:text-style-height text-style sheet)))
         (column-number (floor x (clim:text-style-width text-style sheet)))
         (site          (edit:site buffer)))
    (edit:perform site 'edit:move-cursor-to-line line-number column-number)))

(clim:define-presentation-to-command-translator blank-area->com-move-to-pointer
    (clim:blank-area com-move-to-pointer motion-table :gesture :select)
    (event)
  (list :event event))

(define-motion-command com-remove-cursor ()
  (edit:pop-site buffer))
(bind-key 'motion-table '(#\- :meta) 'com-remove-cursor)

(define-motion-command com-rotate-cursors (&key (direction 'direction))
  (edit:rotate-sites buffer direction))
(bind-key 'motion-table '(#\= :meta) 'com-rotate-cursors :direction :forward)

(define-motion-command com-remove-other-cursors ()
  (edit:remove-other-sites buffer))

;;; TODO move to some other command table
