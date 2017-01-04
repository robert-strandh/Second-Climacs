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

(clim:define-command
    (com-end-of-line :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (climacs2-base:end-of-line cursor)))

(esa:set-key `(com-end-of-line)
	     'motion-table
	     '((#\e :control)))

(defvar *target-column*)

(clim:define-command
    (com-next-line :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view))
	 (buffer (cluffer:buffer cursor))
	 (line-number (cluffer:line-number (cluffer:line cursor))))
    (if (= line-number (1- (cluffer:line-count buffer)))
	(error 'cluffer:end-of-buffer)
	(let* ((next-line (cluffer:find-line buffer (1+ line-number)))
	       (item-count (cluffer:item-count next-line)))
	  (format *trace-output* "~s"
		  (esa:previous-command (esa:current-window)))
	  (unless (member (car (esa:previous-command (esa:current-window)))
			  '(com-next-line com-previous-line))
	    (setf *target-column* (cluffer:cursor-position cursor)))
	  (cluffer:detach-cursor cursor)
	  (cluffer:attach-cursor cursor
				 next-line
				 (min item-count *target-column*))))))

(esa:set-key '(com-next-line)
	     'motion-table
	     '((#\n :control)))
