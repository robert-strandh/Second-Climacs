(defsystem "second-climacs-syntax-fundamental"
  :depends-on ("cluffer"
               "second-climacs-base")
  :serial t
  :components ((:file "packages")
               (:file "analyzer")
               (:file "view")))
