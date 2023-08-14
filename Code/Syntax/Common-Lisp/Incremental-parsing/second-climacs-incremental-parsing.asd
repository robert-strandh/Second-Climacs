(cl:in-package #:asdf-user)

(defsystem "second-climacs-incremental-parsing"
  :depends-on ("trivial-gray-streams"
               "cluffer"
               "flexichain"
               "eclector")
  :serial t
  :components
  ((:file "packages")
   (:file "wad")
   (:file "buffer-stream")
   (:file "cache")))
