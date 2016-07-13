(cl:in-package #:asdf-user)

(defsystem :climacs-command
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "command")))
