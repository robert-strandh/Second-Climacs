(cl:in-package #:asdf-user)

(defsystem :climacs-flexichain-output-history
  :depends-on (:mcclim :flexichain)
  :serial t
  :components
  ((:file "packages")
   (:file "flexichain-output-history")))
