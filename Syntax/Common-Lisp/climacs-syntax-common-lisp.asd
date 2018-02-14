(cl:in-package #:asdf-user)

(defsystem :climacs-syntax-common-lisp
  :depends-on (:climacs-syntax-common-lisp-base)
  :serial t
  :components
  ((:file "indentation-support")
   (:file "indentation")
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
   (:file "wad-to-cst")))
