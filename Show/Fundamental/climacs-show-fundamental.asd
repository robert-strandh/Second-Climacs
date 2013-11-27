(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-show-fundamental
  :depends-on (:climacs-show :climacs-syntax-fundamental)
  :components
  ((:file "packages")
   (:file "fundamental" :depends-on ("packages"))))
