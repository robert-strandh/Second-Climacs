(cl:in-package #:second-climacs-clim-common-lisp-view)

(clim:define-command-table common-lisp-table
  :inherit-from
  (second-climacs-clim-base:global-table
   second-climacs-clim-base:ascii-insert-table
   second-climacs-clim-base:delete-table
   second-climacs-clim-base:motion-table))
