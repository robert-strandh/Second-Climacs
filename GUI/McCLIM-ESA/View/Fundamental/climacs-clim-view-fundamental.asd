(cl:in-package #:asdf-user)

(defsystem :climacs-clim-view-fundamental
  :depends-on (:climacs-syntax-fundamental)
  :serial t
  :components
  ((:file "packages")))
