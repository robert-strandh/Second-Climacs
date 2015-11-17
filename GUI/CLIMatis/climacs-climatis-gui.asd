(cl:in-package #:asdf-user)

(defsystem :climacs-climatis-gui
  :depends-on (:climatis
	       :climacs-view
	       :climacs-buffer
	       :climacs-syntax-fundamental
	       :climacs-show-fundamental
	       :climacs-commands
	       :climacs-basic-emacs)
  :serial t
  :components
  ((:file "packages")
   (:file "gui")))
