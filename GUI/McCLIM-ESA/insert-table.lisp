(cl:in-package #:climacs-esa-gui)

(clim:define-command-table ascii-insert-table)

(clim:define-command
    (com-insert-item :name t :command-table ascii-insert-table)
    ((item t))
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:insert-item cursor item)))

(loop for i from 32 to 126
      for char = (code-char i)
      do (esa:set-key `(com-insert-item ,char)
		      'ascii-insert-table
		      `((,char))))

(esa:set-key `(com-insert-item #\Newline)
	     'ascii-insert-table
	     '((#\Newline)))
