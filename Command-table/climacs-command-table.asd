(defsystem "climacs-command-table"
  :depends-on ("ducling")
  :serial t
  :components ((:file "packages")
               (:file "command-table")))
