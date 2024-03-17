(defsystem "second-climacs-base"
  :depends-on ("cluffer"

               "text.editing"

               "esa-mcclim")
  :serial t
  :components ((:file "packages")
               (:file "buffer")
               (:file "standard-buffer")
               (:file "analyzer")
               (:file "null-analyzer")
               (:file "view")
               (:file "view-class")
               (:file "view-name")
               (:file "fundamental-view")
               (:file "window")
               (:file "application")
               (:file "conditions")))
