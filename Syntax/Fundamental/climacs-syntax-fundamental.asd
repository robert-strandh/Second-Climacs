(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-fundamental
  :depends-on (:cluffer
               :chrono-tree)
  :components
  ((:file "packages")
   (:file "fundamental" :depends-on ("packages"))))
