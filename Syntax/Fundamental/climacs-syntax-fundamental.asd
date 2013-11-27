(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-fundamental
  :depends-on (:climacs-buffer)
  :components
  ((:file "packages")
   (:file "fundamental" :depends-on ("packages"))))
