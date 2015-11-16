(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-buffer
  :depends-on (:splay-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "update-protocol")
   (:file "line")))
