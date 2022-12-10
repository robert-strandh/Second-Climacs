(defsystem "second-climacs-syntax-common-lisp-base"
  :depends-on ("trivial-gray-streams"
               "cluffer"
               "flexichain"
               "eclector")
  :serial t
  :components ((:file "packages")
               (:file "wad")))
