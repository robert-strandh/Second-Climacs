(defsystem "climacs-syntax-common-lisp"
  :depends-on ("second-climacs-base"

               "second-climacs-clim-base"

               "climacs-syntax-common-lisp-base"
               "climacs-syntax-common-lisp-indentation")
  :serial t
  :components ((:file "folio")
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
               (:file "set-mode")))
