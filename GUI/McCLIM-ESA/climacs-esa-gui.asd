(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:mcclim
	       :cluffer
	       :climacs-basic-emacs
	       :climacs-flexichain-output-history
	       :clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "climacs-clim-view")
   (:file "text-pane")
   (:file "fundamental-view")
   (:file "gui")
   (:file "io")
   (:file "insert-table")))
