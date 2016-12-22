(cl:in-package #:asdf-user)

(defsystem :climacs-esa-gui
  :depends-on (:mcclim
	       :cluffer
	       :climacs-basic-emacs)
  :serial t
  :components
  ((:file "packages")
   (:file "text-pane")
   (:file "gui")
   (:file "io")))
