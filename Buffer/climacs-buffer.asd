(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-buffer
  :depends-on (:splay-tree)
  :components
  ((:file "packages")
   (:file "buffer" :depends-on ("packages"))
   (:file "line" :depends-on ("packages" "buffer"))))
