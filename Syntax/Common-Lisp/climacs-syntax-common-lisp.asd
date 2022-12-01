(defsystem "climacs-syntax-common-lisp"
  :depends-on ("second-climacs-base"

               "second-climacs-clim-base"

               "climacs-syntax-common-lisp-base"
               "climacs-syntax-common-lisp-indentation")
  :serial t
  :components ((:file "folio")
               (:file "folio-stream")
               (:file "flexichain-folio")
               (:file "cache")
               (:file "analyzer")

               (:file "token")
               (:file "client")
               (:file "parse")
               (:file "read-forms")
               (:file "view")
               (:file "set-mode")
               (:file "find-wad-beginning-line")
               (:file "find-wad-containing-position")))
