(cl:in-package #:asdf-user)

(defsystem :climacs-clim-view-fundamental
  :depends-on (:climacs-syntax-fundamental
               :second-climacs-clim-base
               :stealth-mixin)
  :serial t
  :components
  ((:file "packages")
   (:file "view")))
