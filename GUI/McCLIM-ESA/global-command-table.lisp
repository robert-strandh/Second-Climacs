(cl:in-package #:climacs-esa-gui)

(clim:define-command-table global-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table))

(clim:define-command (com-inspect :name t :command-table global-table) ()
  (clouseau:inspector clim:*application-frame*))

(clim:define-command
    (com-common-lisp-mode :name t :command-table global-table)
    ()
  (let* ((current-window (esa:current-window))
	 (clim-view (clim:stream-default-view current-window))
	 (climacs-view (climacs-view clim-view)))
    (climacs-syntax-common-lisp:set-common-lisp-mode climacs-view)))
