(defsystem "second-climacs-syntax-common-lisp"
  :depends-on ("second-climacs-syntax-common-lisp-base"
               ; "second-climacs-syntax-common-lisp-indentation"
               "concrete-syntax-tree"
               "utilities.print-tree") ; for debugging
  :serial t
  :components ((:file "expression")
               (:file "view")
               (:file "set-mode")
               (:file "motion")
               (:file "fill-paragraph")
               (:file "debug")))
