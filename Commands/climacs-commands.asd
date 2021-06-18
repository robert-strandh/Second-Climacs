(defsystem "climacs-commands"
  :depends-on ("climatis"
               "clueless")
  :components ((:file "packages")
               (:file "commands" :depends-on ("packages"))))
