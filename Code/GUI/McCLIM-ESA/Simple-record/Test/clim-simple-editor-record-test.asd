(defsystem "clim-simple-editor-record-test"
  :depends-on ("mcclim"
               "clim-simple-editor-record"
               "clueless")
  :serial t
  :components ((:file "packages")
               (:file "record-test")))
