(cl:in-package #:asdf-user)

(defsystem "second-climacs-buffer-compilation"
  :depends-on ("second-climacs-syntax-common-lisp"
               "common-boot"
               "common-boot-macros"
               "trucler-native")
  :serial t
  :components
  ((:file "packages")
   (:file "trucler-extra")
   (:file "wad-to-cst")))
