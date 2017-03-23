(cl:in-package #:asdf-user)

(defsystem second-climacs-clim
  :depends-on (:second-climacs-clim-base
               :second-climacs-clim-fundamental-view
               :second-climacs-clim-common-lisp-view)
  :serial t
  :components
  ((:file "gui")
   (:file "io")))
