(cl:in-package #:climacs-clim-view-fundamental)

(clim:define-command-table fundamental-table
  :inherit-from
  (second-climacs-clim-base:global-table
   second-climacs-clim-base:ascii-insert-table
   second-climacs-clim-base:delete-table
   second-climacs-clim-base:motion-table))
