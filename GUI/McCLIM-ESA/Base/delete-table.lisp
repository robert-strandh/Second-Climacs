(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table delete-table)

(clim:define-command
    (com-delete-item :name t :command-table delete-table)
    ()
  (with-current-cursor (cursor)
    (base:delete-item cursor)
    (setf (esa-buffer:needs-saving (base:buffer cursor)) t)))

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
