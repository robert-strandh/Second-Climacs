(cl:in-package #:common-lisp-user)

(asdf:defsystem :chrono-tree
  :depends-on (:splay-tree)
  :components
  ((:file "packages")
   (:file "chrono-tree" :depends-on ("packages"))))
