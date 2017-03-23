(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-clim-base
  (:use #:common-lisp)
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
   #:command-table))
