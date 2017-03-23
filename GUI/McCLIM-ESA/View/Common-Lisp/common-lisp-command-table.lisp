(cl:in-package #:second-climacs-clim-common-lisp-view)

(clim:define-command-table common-lisp-table
  :inherit-from
  (climacs-esa-gui:global-table
   climacs-esa-gui:ascii-insert-table
   climacs-esa-gui:delete-table
   climacs-esa-gui:motion-table))
