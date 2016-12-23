(cl:in-package #:asdf-user)

(defsystem :second-climacs-base
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "analyzer")))
