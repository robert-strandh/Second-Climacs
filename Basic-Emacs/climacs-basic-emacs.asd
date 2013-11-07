(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-basic-emacs
  :depends-on (:climacs-buffer)
  :components
  ((:file "packages")
   (:file "basic-emacs" :depends-on ("packages"))))
