(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-commands
  :depends-on (:climatis
	       :clueless
	       :climacs-basic-emacs)
  :components
  ((:file "packages")
   (:file "commands" :depends-on ("packages"))))

