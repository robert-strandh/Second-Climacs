(cl:in-package #:asdf-user)

(defsystem :climacs-flexichain-output-history-test
  :depends-on (:mcclim
	       :climacs-flexichain-output-history
	       :clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "history-test")))
