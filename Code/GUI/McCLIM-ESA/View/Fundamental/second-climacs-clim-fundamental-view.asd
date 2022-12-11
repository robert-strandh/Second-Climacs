(defsystem "second-climacs-clim-fundamental-view"
  :depends-on ("second-climacs-syntax-fundamental"
               "second-climacs-clim-base"
               "stealth-mixin")
  :serial t
  :components ((:file "packages")
               (:file "view")
               (:file "command-table")))
