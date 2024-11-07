(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table delete-table)

(defmacro define-delete-command (name (&rest extra-parameters) &body body)
  `(define-buffer-command (,name delete-table) ,extra-parameters
     ,@body))

(define-delete-command com-delete (&key (unit 'unit) (direction 'direction))
  (edit:perform buffer 'edit:delete unit direction))

(define-command-specializations (delete-table com-delete)
  (com-kill-region      (:unit edit:region-or-item :direction :forward)  (#\w :control))
  ;; Buffer structure units
  (com-delete-item      (:unit edit:item           :direction :forward)  (#\d :control))
  (com-erase-item       (:unit edit:item           :direction :backward) #\Backspace)
  (com-kill-line        (:unit edit:semi-line      :direction :forward)  (#\k :control))
  ;; TODO line backward is C-0 C-k in Emacs
  (com-erase-buffer     (:unit edit:buffer         :direction :forward))
  ;; Prose units
  (com-delete-word      (:unit edit:word           :direction :forward)  (#\d :meta))
  (com-erase-word       (:unit edit:word           :direction :backward) (#\Backspace :meta))
  (com-delete-sentence  (:unit edit:sentence       :direction :forward))
  (com-erase-sentence   (:unit edit:sentence       :direction :backward))
  (com-delete-paragraph (:unit edit:paragraph      :direction :forward))
  (com-erase-paragraph  (:unit edit:paragraph      :direction :backward))
  ;; Expression
  (com-delete-expression-forward  (:unit edit.exp:expression :direction :forward)  (#\k :control :meta))
  (com-delete-expression-backward (:unit edit.exp:expression :direction :backward) (#\Backspace :control :meta)))

(define-delete-command com-delete-indentation ()
  (edit:perform buffer 'edit:delete-indentation))
(bind-key 'delete-table '(#\^ :meta) 'com-delete-indentation)

(define-delete-command com-delete-trailing-spaces ()
  (edit:perform buffer 'edit:delete-trailing-whitespace))

(define-delete-command com-fixup-whitespace ()
  (edit:perform buffer 'edit:fixup-whitespace))

;;; Copying and inserting

(define-delete-command com-copy ()
  (edit:perform buffer 'edit:copy edit:region :forward))
(bind-key 'delete-table '(#\w :meta) 'com-copy)

(define-delete-command com-unkill ()
  (edit:perform buffer 'edit:yank :forward))
(bind-key 'delete-table '(#\y :control) 'com-unkill)
