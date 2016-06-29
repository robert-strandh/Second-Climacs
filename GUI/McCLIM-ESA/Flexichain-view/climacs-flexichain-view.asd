(cl:in-package #:asdf-user)

(defsystem :climacs-flexichain-view
  :depends-on (:flexichain)
  :serial t
  :components
  ((:file "packages")))
