(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table motion-table)

(clim:define-command
    (com-forward-item :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:forward-item cursor)))

(esa:set-key `(com-forward-item)
	     'motion-table
	     '((#\f :control)))

(clim:define-command
    (com-backward-item :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:backward-item cursor)))

(esa:set-key `(com-backward-item)
	     'motion-table
	     '((#\b :control)))

(clim:define-command
    (com-beginning-of-line :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:beginning-of-line cursor)))

(esa:set-key `(com-beginning-of-line)
	     'motion-table
	     '((#\a :control)))

(clim:define-command
    (com-end-of-line :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:end-of-line cursor)))

(esa:set-key `(com-end-of-line)
	     'motion-table
	     '((#\e :control)))

(defvar *target-column*)

(clim:define-command
    (com-next-line :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:next-line cursor)))

(esa:set-key '(com-next-line)
	     'motion-table
	     '((#\n :control)))

(clim:define-command
    (com-previous-line :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:previous-line cursor)))

(esa:set-key '(com-previous-line)
	     'motion-table
	     '((#\p :control)))

(clim:define-command
    (com-beginning-of-buffer :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:beginning-of-buffer cursor)))

(esa:set-key '(com-beginning-of-buffer)
	     'motion-table
	     '((#\< :meta)))

(clim:define-command
    (com-end-of-buffer :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:end-of-buffer cursor)))

(esa:set-key '(com-end-of-buffer)
	     'motion-table
	     '((#\> :meta)))

(clim:define-command
    (com-forward-word :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:forward-word cursor)))

(esa:set-key `(com-forward-word)
	     'motion-table
	     '((#\f :meta)))

(clim:define-command
    (com-backward-word :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:backward-word cursor)))

(esa:set-key `(com-backward-word)
	     'motion-table
	     '((#\b :meta)))

(clim:define-command
    (com-back-to-indentation :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:back-to-indentation cursor)))

(esa:set-key `(com-back-to-indentation)
	     'motion-table
	     '((#\m :meta)))

(clim:define-command
    (com-set-the-mark :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:set-the-mark cursor)))

(esa:set-key `(com-set-the-mark)
	     'motion-table
	     '((#\Space :control)))

(clim:define-command
    (com-exchange-cursor-and-mark :name t :command-table motion-table)
    ()
  (with-current-cursor (cursor)
    (base:exchange-cursor-and-mark cursor)))

(esa:set-key `(com-exchange-cursor-and-mark)
	     'motion-table
	     '((#\x :control) (#\x :control)))

(clim:define-command
    (com-search-forward :name t :command-table motion-table)
    ((string 'string))
  (with-current-cursor (cursor)
    (unless (base:search-forward cursor string)
      (esa:display-message "No match"))))

(clim:define-command
    (com-search-backward :name t :command-table motion-table)
    ((string 'string))
  (with-current-cursor (cursor)
    (unless (base:search-backward cursor string)
      (esa:display-message "No match"))))
