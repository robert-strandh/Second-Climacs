(cl:in-package #:climacs-esa-gui)

(clim:define-command-table motion-table)

(clim:define-command
    (com-forward-item :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:forward-item cursor)))

(esa:set-key `(com-forward-item)
	     'motion-table
	     '((#\f :control)))
