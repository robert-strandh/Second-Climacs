(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table delete-table)

(clim:define-command
    (com-delete-item :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:delete-item cursor)))

(esa:set-key `(com-delete-item)
	     'delete-table
	     '((#\d :control)))

(clim:define-command
    (com-erase-item :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:erase-item cursor)))

(esa:set-key `(com-erase-item)
	     'delete-table
	     '((#\Backspace)))

(clim:define-command (com-delete-word :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:delete-word cursor)))

(esa:set-key `(com-delete-word)
             'delete-table
             '((#\d :meta)))

(clim:define-command (com-erase-word :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (handler-bind ((error #'invoke-debugger)) (base:erase-word cursor))))

(esa:set-key `(com-erase-word)
             'delete-table
             '((#\Backspace :meta)))

(clim:define-command
    (com-kill-line :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (if (cluffer:end-of-line-p cursor)
	(base:delete-item cursor)
	(loop until (cluffer:end-of-line-p cursor)
	      do (base:delete-item cursor)))))

(esa:set-key `(com-kill-line)
	     'delete-table
	     '((#\k :control)))

(clim:define-command
    (com-kill-region :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:kill-region cursor)))

(esa:set-key `(com-kill-region)
	     'delete-table
	     '((#\w :control)))

(clim:define-command
    (com-unkill :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:unkill cursor)))

(esa:set-key `(com-unkill)
             'delete-table
             '((#\y :control)))

(clim:define-command (com-delete-indentation
                      :name          t
                      :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:delete-indentation cursor)))

(esa:set-key `(com-delete-indentation) 'delete-table '((#\^ :meta)))
