(defsystem "second-climacs-syntax-common-lisp"
  :depends-on ("second-climacs-base"
               "second-climacs-syntax-common-lisp-base"
               "second-climacs-syntax-common-lisp-indentation"
               "spell" ; for spell checking in comments
               "concrete-syntax-tree"
               "utilities.print-tree") ; for debugging
  :serial t
  :components ((:file "buffer-stream")
               (:file "cache")
               (:file "analyzer")
               (:file "token")
               (:file "client")
               (:file "parse")
               (:file "read-forms")
               (:file "update-cache")
               (:file "view")
               (:file "set-mode")
               (:file "wad-descriptor")
               (:file "mapwad")
               (:file "find-wad-beginning-line")
               (:file "find-wad-containing-position")
               (:file "motion")
               (:file "regions")
               (:file "fill-paragraph")
               (:file "debug")))
