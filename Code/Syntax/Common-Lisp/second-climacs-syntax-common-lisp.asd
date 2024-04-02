(defsystem "second-climacs-syntax-common-lisp"
  :depends-on ("incrementalist"
               "second-climacs-base"
               ; "second-climacs-syntax-common-lisp-indentation"
               "concrete-syntax-tree"
               "utilities.print-tree") ; for debugging
  :serial t
  :components ((:file "packages")
               (:file "expression")
               (:file "view")
               (:file "set-mode")
               (:file "fill-paragraph")
               (:file "debug")))
