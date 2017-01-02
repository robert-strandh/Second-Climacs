(cl:in-package #:climacs-esa-gui)

(clim:define-command-table fundamental-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table
   ascii-insert-table
   motion-table))
