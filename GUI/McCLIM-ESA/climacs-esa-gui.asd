(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:mcclim
	       :cluffer
	       :second-climacs-base
	       :climacs-basic-emacs
	       :climacs-flexichain-output-history
	       :clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "climacs-clim-view")
   (:file "text-pane")
   (:file "view")
   (:file "fundamental-view")
   (:file "insert-table")
   (:file "gui")
   (:file "io")))
