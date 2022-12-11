(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-clim-base
  (:use #:common-lisp)
  (:local-nicknames
   (#:base #:second-climacs-base)
   (#:cl-syntax #:second-climacs-syntax-common-lisp)
   (#:fundamental-syntax #:climacs-syntax-fundamental))
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
   #:with-current-cursor-and-view
   #:with-current-cursor))
