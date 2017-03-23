(cl:in-package #:asdf-user)

(defsystem :climacs-syntax-fundamental
  :depends-on (:cluffer)
  :serial t
  :components
  ((:file "packages")
   (:file "analyzer")))

