(cl:in-package #:common-lisp-user)

(asdf:defsystem :splay-tree
  :components
  ((:file "packages")
   (:file "splay-tree" :depends-on ("packages"))))
