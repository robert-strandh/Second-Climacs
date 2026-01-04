(defsystem "second-climacs-clim-common-lisp-view"
  :depends-on ("second-climacs-clim-base"
               "second-climacs-syntax-common-lisp"
               "stealth-mixin"
               "esclados-command-table-manipulation")
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "drawing-utilities")
               (:file "drawing-decorations")
               (:file "drawing-wads")
               (:file "common-lisp-view")
               (:file "common-lisp-command-table")
               #+(or) (:file "indentation")))
