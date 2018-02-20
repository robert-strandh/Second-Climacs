(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command
    (com-indent-line :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (second-climacs-base:analyzer climacs-view))
         (cache (climacs-syntax-common-lisp:folio analyzer))
         (climacs-buffer (climacs2-base:buffer analyzer))
         (cluffer-buffer (climacs2-base:cluffer-buffer climacs-buffer)))
    (climacs-syntax-common-lisp:scavenge cache cluffer-buffer)
    (climacs-syntax-common-lisp:read-forms analyzer)))

(esa:set-key `(com-indent-line)
	     'common-lisp-table
	     '((#\i :control)))
