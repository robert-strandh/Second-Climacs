(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command
    (com-indent-line :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (cl-syntax:folio analyzer))
         (climacs-buffer (base:buffer analyzer))
         (cluffer-buffer (base:cluffer-buffer climacs-buffer)))
    (cl-syntax:scavenge cache cluffer-buffer)
    (cl-syntax:read-forms analyzer)
    (clim-base:with-current-cursor (cursor)
      (cl-syntax:indent-line cache cursor))))

(esa:set-key `(com-indent-line)
	     'common-lisp-table
	     '((#\i :control)))
