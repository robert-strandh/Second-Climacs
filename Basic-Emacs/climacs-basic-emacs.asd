(cl:in-package #:asdf-user)

(defsystem :climacs-basic-emacs
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")
   (:file "basic-emacs")))
