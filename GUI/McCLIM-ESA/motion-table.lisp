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

(clim:define-command
    (com-previous-line :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view))
	 (buffer (cluffer:buffer cursor))
	 (line-number (cluffer:line-number (cluffer:line cursor))))
    (if (zerop line-number)
	(error 'cluffer:beginning-of-buffer)
	(let* ((previous-line (cluffer:find-line buffer (1- line-number)))
	       (item-count (cluffer:item-count previous-line)))
	  (unless (member (car (esa:previous-command (esa:current-window)))
			  '(com-next-line com-previous-line))
	    (setf *target-column* (cluffer:cursor-position cursor)))
	  (cluffer:detach-cursor cursor)
	  (cluffer:attach-cursor cursor
				 previous-line
				 (min item-count *target-column*))))))

(esa:set-key '(com-previous-line)
	     'motion-table
	     '((#\p :control)))

(clim:define-command
    (com-beginning-of-buffer :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view))
	 (buffer (cluffer:buffer cursor))
	 (first-line (cluffer:find-line buffer 0)))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor first-line)))

(esa:set-key '(com-beginning-of-buffer)
	     'motion-table
	     '((#\< :meta)))

(clim:define-command
    (com-end-of-buffer :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view))
	 (buffer (cluffer:buffer cursor))
	 (line-count (cluffer:line-count buffer))
	 (last-line (cluffer:find-line buffer (1- line-count)))
	 (item-count (cluffer:item-count last-line)))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor last-line item-count)))

(esa:set-key '(com-end-of-buffer)
	     'motion-table
	     '((#\> :meta)))

(clim:define-command
    (com-forward-word :name t :command-table motion-table)
    ()
  (let* ((clim-view (clim:stream-default-view (esa:current-window)))
	 (climacs-view (climacs-view clim-view))
	 (cursor (climacs2-base:cursor climacs-view)))
    (loop until (or (cluffer:end-of-buffer-p cursor)
		    (alphanumericp (climacs2-base:item-after-cursor cursor)))
	  do (climacs2-base:forward-item cursor))
    (loop until (or (cluffer:end-of-line-p cursor)
		    (not (characterp (climacs2-base:item-after-cursor cursor)))
		    (not (alphanumericp (climacs2-base:item-after-cursor cursor))))
	  do (climacs2-base:forward-item cursor))))

(esa:set-key `(com-forward-word)
	     'motion-table
	     '((#\f :meta)))
