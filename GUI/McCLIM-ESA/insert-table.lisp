(cl:in-package #:climacs-esa-gui)

(clim:define-command-table ascii-insert-table)

(clim:define-command
    (com-insert-item :name t :command-table ascii-insert-table)
    ((item t))
  (climacs2-base:insert-item
   (climacs2-base:cursor (climacs2-base:view (esa:current-window)))
   item))

(loop for i from 32 to 126
      for char = (code-char i)
      do (esa:set-key `(com-insert-item ,char)
		      'ascii-insert-table
		      `((,char))))
