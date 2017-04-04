(cl:in-package #:asdf-user)

(defsystem second-climacs-clim-common-lisp-view
  :depends-on (:second-climacs-clim-base
               :climacs-syntax-common-lisp
               :stealth-mixin)
  :serial t
  :components
  ((:file "packages")
   (:file "common-lisp-view")
   (:file "token-drawing")
   (:file "common-lisp-command-table")))
