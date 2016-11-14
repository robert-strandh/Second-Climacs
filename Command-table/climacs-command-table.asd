(cl:in-package #:asdf-user)

(defsystem :climacs-command-table
  :depends-on (:ducling)
  :serial t
  :components
  ((:file "packages")))

