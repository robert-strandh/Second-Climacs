(cl:in-package #:second-climacs-clim-base)

(clim:define-command-table global-table
  :inherit-from
  (esa:global-esa-table
   esa-io:esa-io-table))

(clim:define-command (com-inspect :name t :command-table global-table) ()
  (clouseau:inspect clim:*application-frame* :new-process t))
