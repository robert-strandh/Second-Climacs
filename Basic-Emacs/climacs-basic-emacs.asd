(cl:in-package #:asdf-user)

(defsystem :climacs-basic-emacs
  :depends-on (:climacs-buffer)
  :components
  ((:file "packages")
   (:file "basic-emacs" :depends-on ("packages"))))
