(defsystem "eclector-test"
  :depends-on ("eclector")
  :serial t
  :components ((:file "packages")
               (:file "eclector-test")))
