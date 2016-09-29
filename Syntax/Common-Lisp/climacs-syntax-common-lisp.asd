(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-common-lisp
  :depends-on (:trivial-gray-streams
	       :cluffer
	       :flexichain
	       :sicl-reader-simple)
  :serial t
  :components
  ((:file "packages")
   (:file "parse-result")
   (:file "folio")
   (:file "folio-stream")
   (:file "flexichain-folio")
   (:file "analyzer")
   (:file "more-variables")
   (:file "additional-conditions")
   (:file "read-forms")
   (:file "analyzer-stream")))
