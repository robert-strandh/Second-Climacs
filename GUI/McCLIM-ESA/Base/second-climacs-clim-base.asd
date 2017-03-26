(cl:in-package #:asdf-user)

(defsystem :second-climacs-clim-base
  :depends-on (:mcclim
               :second-climacs-base
               :clouseau
               :stealth-mixin)
  :serial t
  :components
  ((:file "packages")
   (:file "climacs-clim-view")
   (:file "esa-buffer")
   (:file "view-name")
   (:file "text-pane")
   (:file "with-current-cursor")
   (:file "insert-table")
   (:file "delete-table")
   (:file "motion-table")
   (:file "global-command-table")))
