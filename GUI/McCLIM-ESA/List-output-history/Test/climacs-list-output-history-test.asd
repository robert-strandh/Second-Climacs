(cl:in-package #:asdf-user)

(defsystem :climacs-list-output-history-test
  :depends-on (:mcclim
	       :climacs-list-output-history
	       :clueless)
  :serial t
  :components
  ((:file "packages")
   (:file "history-test")))
