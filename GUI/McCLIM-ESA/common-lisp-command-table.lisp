(cl:in-package #:climacs-esa-gui)

(clim:define-command-table common-lisp-table
  :inherit-from
  (global-table
   ascii-insert-table
   delete-table
   motion-table))
