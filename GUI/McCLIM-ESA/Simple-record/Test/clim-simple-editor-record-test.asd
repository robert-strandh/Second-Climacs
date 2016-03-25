(cl:in-package #:asdf-user)

(defsystem clim-simple-editor-record-test
  :depends-on (:mcclim :clueless)
  :serial t
  :components
  ((:file "packages")
   (:file "record-test")))
