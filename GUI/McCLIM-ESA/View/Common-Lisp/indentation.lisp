(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command
    (com-indent-line :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (climacs-syntax-common-lisp:folio analyzer))
         (climacs-buffer (base:buffer analyzer))
         (cluffer-buffer (base:cluffer-buffer climacs-buffer)))
    (climacs-syntax-common-lisp:scavenge cache cluffer-buffer)
    (climacs-syntax-common-lisp:read-forms analyzer)
    (second-climacs-clim-base::with-current-cursor (cursor)
      (climacs-syntax-common-lisp:indent-line
       analyzer (cluffer:line-number cursor)))))

(esa:set-key `(com-indent-line)
	     'common-lisp-table
	     '((#\i :control)))
