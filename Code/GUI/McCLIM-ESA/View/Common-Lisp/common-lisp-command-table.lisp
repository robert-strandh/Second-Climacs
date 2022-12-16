(cl:in-package #:second-climacs-clim-view-common-lisp)

(clim:define-command-table common-lisp-table
  :inherit-from
  (clim-base:global-table
   clim-base:ascii-insert-table
   clim-base:delete-table
   clim-base:motion-table))

(clim:define-command
    (com-up-expression :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (cl-syntax:folio analyzer)))
    (base:update-view climacs-view)
    (clim-base:with-current-cursor (cursor)
      (cl-syntax:up-expression cache cursor))))

(esa:set-key `(com-up-expression)
	     'common-lisp-table
	     '((#\u :meta :control)))

(clim:define-command
    (com-forward-expression :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (cl-syntax:folio analyzer)))
    (base:update-view climacs-view)
    (clim-base:with-current-cursor (cursor)
      (cl-syntax:forward-expression cache cursor))))

(esa:set-key `(com-forward-expression)
	     'common-lisp-table
	     '((#\f :meta :control)))

(clim:define-command
    (com-backward-expression :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (cl-syntax:folio analyzer)))
    (base:update-view climacs-view)
    (clim-base:with-current-cursor (cursor)
      (cl-syntax:backward-expression cache cursor))))

(esa:set-key `(com-backward-expression)
	     'common-lisp-table
	     '((#\b :meta :control)))

(clim:define-command
    (com-mark-expression :name t :command-table common-lisp-table)
    ()
  (let* ((view (clim:stream-default-view (esa:current-window)))
         (climacs-view (clim-base:climacs-view view))
         (analyzer (base:analyzer climacs-view))
         (cache (cl-syntax:folio analyzer)))
    (base:update-view climacs-view)
    (clim-base:with-current-cursor (cursor)
      (cl-syntax:mark-expression cache cursor))))

(esa:set-key `(com-mark-expression)
	     'common-lisp-table
	     '((#\Space :meta :control)))
