(cl:in-package #:common-lisp-user)

(asdf:defsystem #:climacs-view
  :depends-on (:climacs-buffer
	       :climacs-syntax-fundamental
	       :climacs-show-fundamental)
  :components
  ((:file "packages")
   (:file "view" :depends-on ("packages"))))
