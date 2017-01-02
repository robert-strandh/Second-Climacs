(cl:in-package #:climacs-esa-gui)

(clim:define-command-table fundamental-table
  :inherit-from
  (global-table
   ascii-insert-table
   motion-table))
