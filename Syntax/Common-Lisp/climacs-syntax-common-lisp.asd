(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-common-lisp
  :depends-on (:trivial-gray-streams
               :concrete-syntax-tree
	       :cluffer
	       :flexichain
	       :sicl-reader-simple)
  :serial t
  :components
  ((:file "packages")
   (:file "wad")
   (:file "folio")
   (:file "token")
   (:file "folio-stream")
   (:file "flexichain-folio")
   (:file "cache")
   (:file "more-variables")
   (:file "additional-conditions")
   (:file "analyzer")
   (:file "reader")
   (:file "parse")
   (:file "read-forms")
   (:file "view")
   (:file "set-mode")
   (:file "wad-to-cst")
   (:file "indentation")))
