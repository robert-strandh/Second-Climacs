(cl:in-package #:asdf-user)

(defsystem :climacs-basic-emacs
  :depends-on (:climacs-buffer)
  :serial t
  :components
  ((:file "packages")
   (:file "basic-emacs")))
