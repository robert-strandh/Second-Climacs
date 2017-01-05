(cl:in-package #:climacs-esa-gui)

(clim:define-command-table delete-table)

(clim:define-command
    (com-delete-item :name t :command-table delete-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:delete-item cursor)))

(esa:set-key `(com-delete-item #\Newline)
	     'delete-table
	     '((#\d :control)))

(clim:define-command
    (com-erase-item :name t :command-table delete-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:erase-item cursor)))

(esa:set-key `(com-erase-item #\Newline)
	     'delete-table
	     '((#\Backspace)))
