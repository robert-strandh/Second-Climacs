(defsystem "climacs-syntax-common-lisp-base"
  :depends-on ("trivial-gray-streams"
               "cluffer"
               "flexichain"
               "sicl-reader-simple")
  :serial t
  :components ((:file "packages")
               (:file "wad")))
