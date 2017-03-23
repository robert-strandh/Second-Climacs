(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table fundamental-table
  :inherit-from
  (global-table
   ascii-insert-table
   delete-table
   motion-table))
