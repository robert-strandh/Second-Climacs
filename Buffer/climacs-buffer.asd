(cl:in-package #:asdf-user)

(defsystem :climacs-buffer
  :depends-on (:splay-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "update-protocol")
   (:file "line")))
