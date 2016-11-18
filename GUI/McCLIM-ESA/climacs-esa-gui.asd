(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:esa-mcclim
	       :climacs-view
	       :cluffer
	       :climacs-syntax-fundamental
	       :climacs-show-fundamental
	       :climacs-commands
	       :climacs-basic-emacs)
  :serial t
  :components
  ((:file "packages")
   (:file "text-pane")
   (:file "gui")))
