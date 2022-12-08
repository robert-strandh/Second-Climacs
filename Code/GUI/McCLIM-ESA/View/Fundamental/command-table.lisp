(cl:in-package #:second-climacs-clim-view-fundamental)

(clim:define-command-table fundamental-table
  :inherit-from
  (clim-base:global-table
   clim-base:ascii-insert-table
   clim-base:delete-table
   clim-base:motion-table))
