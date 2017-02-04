(cl:in-package #:asdf-user)

(defsystem :climacs-climatis-gui
  :depends-on (:climatis
	       :cluffer
	       :climacs-view
	       :climacs-syntax-fundamental
	       :climacs-show-fundamental
	       :climacs-commands)
  :serial t
  :components
  ((:file "packages")
   (:file "gui")))
