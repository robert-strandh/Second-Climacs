(defsystem "second-climacs-clim-base"
  :depends-on ("mcclim"
               "second-climacs-base"
               "clouseau"
               "stealth-mixin"
               "second-climacs-syntax-fundamental"
               "second-climacs-syntax-common-lisp")
  :serial t
  :components ((:file "packages")
               (:file "climacs-clim-view")
               (:file "esa-buffer")
               (:file "text-pane")
               (:file "with-current-cursor")
               (:file "insert-table")
               (:file "delete-table")
               (:file "motion-table")
               (:file "global-command-table")))
