(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:mcclim
	       :cluffer
	       :second-climacs-base
	       :climacs-flexichain-output-history
	       :clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "climacs-clim-view")
   (:file "text-pane")
   (:file "fundamental-view")
   (:file "insert-table")
   (:file "motion-table")
   (:file "gui")
   (:file "io")))
