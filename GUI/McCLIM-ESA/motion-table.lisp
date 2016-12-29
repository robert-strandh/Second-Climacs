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

(clim:define-command
    (com-backward-item :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:backward-item cursor)))

(esa:set-key `(com-backward-item)
	     'motion-table
	     '((#\b :control)))

(clim:define-command
    (com-beginning-of-line :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:beginning-of-line cursor)))

(esa:set-key `(com-beginning-of-line)
	     'motion-table
	     '((#\a :control)))
