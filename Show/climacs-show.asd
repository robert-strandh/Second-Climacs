(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-show
  :depends-on (:climacs-syntax-fundamental)
  :components
  ((:file "packages")
   (:file "show" :depends-on ("packages"))))
