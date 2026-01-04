(defsystem "second-climacs-clim-base"
  :depends-on ("mcclim"
               "second-climacs-base"
               "clouseau"
               "stealth-mixin"
               "second-climacs-syntax-fundamental"
               "second-climacs-syntax-common-lisp"
               "esclados-standard-key-bindings"
               "esclados-command-table-manipulation")
  :serial t
  :components ((:file "packages")
               (:file "climacs-clim-view")
               (:file "esa-buffer")
               (:file "text-pane")
               (:file "with-current-cursor")
               (:file "cursor-visibility")
               ;; Command tables
               (:file "macros")
               (:file "insert-table")
               (:file "delete-table")
               (:file "motion-table")
               (:file "search-table")
               (:file "transform-table")
               (:file "paredit-table")
               (:file "debug-table")
               (:file "global-command-table")))
