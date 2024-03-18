(cl:defpackage #:second-climacs-clim-base
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:edit               #:text.editing)
   (#:edit.search        #:text.editing.search)
   (#:ip                 #:incrementalist)

   (#:base               #:second-climacs-base)
   (#:fundamental-syntax #:second-climacs-syntax-fundamental)
   (#:cl-syntax          #:second-climacs-syntax-common-lisp))

  (:export
   #:climacs
   #:text-pane
   #:global-table
   #:motion-table
   #:ascii-insert-table
   #:insert-table
   #:delete-table
   #:make-climacs-clim-view
   #:climacs-clim-view
   #:climacs-view
   #:update-view
   #:command-table
   #:left-gutter
   #:with-current-cursor)

  (:export
   #:move-viewport-to-cursor
   #:move-cursor-to-viewport))
