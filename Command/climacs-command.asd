(cl:in-package #:asdf-user)

(defsystem :climacs-command
  :serial t
  :components
  ((:file "packages")
   (:file "command")))
