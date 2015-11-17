(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:esa
	       :climacs-view
	       :climacs-buffer
	       :climacs-syntax-fundamental
	       :climacs-show-fundamental
	       :climacs-commands
	       :climacs-basic-emacs)
  :serial t
  :components
  ((:file "packages")))
