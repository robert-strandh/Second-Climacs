(defsystem "second-climacs-buffer-compilation"
  :depends-on ("second-climacs-syntax-common-lisp"
               "common-boot"
               "common-boot-macros")
  :serial t
  :components
  ((:file "wad-to-cst")))
