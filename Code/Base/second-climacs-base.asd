(defsystem "second-climacs-base"
  :depends-on ("cluffer"
               "cluffer-emacs-compatibility"
               "esa-mcclim")
  :serial t
  :components ((:file "packages")
               (:file "kill-ring")
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
               (:file "search")
               (:file "conditions")))
