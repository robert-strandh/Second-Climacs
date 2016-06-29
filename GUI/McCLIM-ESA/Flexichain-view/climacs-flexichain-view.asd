(cl:in-package #:asdf-user)

(defsystem :climacs-flexichain-view
  :depends-on (:cluffer
	       :flexichain)
  :serial t
  :components
  ((:file "packages")
   (:file "view")))
