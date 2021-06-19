(defsystem "climacs-show-fundamental"
  :depends-on ("climatis"
               "climacs-show"
               "climacs-syntax-fundamental")
  :components ((:file "packages")
               (:file "fundamental" :depends-on ("packages"))))
