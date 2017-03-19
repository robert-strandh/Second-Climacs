(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-fundamental
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")))

