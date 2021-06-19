(defsystem "climacs-show"
  :depends-on ("climacs-syntax-fundamental")
  :components ((:file "packages")
               (:file "show" :depends-on ("packages"))))
