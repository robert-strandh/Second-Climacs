(cl:in-package #:asdf-user)

(defsystem second-climacs-clim-fundamental-view
  :depends-on (:second-climacs-clim-base
               :climacs-flexichain-output-history)
  :serial t
  :components
  ((:file "fundamental-view")
   (:file "fundamental-command-table")))
