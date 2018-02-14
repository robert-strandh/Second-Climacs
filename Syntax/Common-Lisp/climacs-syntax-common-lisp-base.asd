(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-common-lisp-base
  :depends-on (:trivial-gray-streams
               :concrete-syntax-tree
	       :cluffer
	       :flexichain
	       :sicl-reader-simple)
  :serial t
  :components
  ((:file "packages")
   (:file "wad")))
