(defsystem #:climacs-syntax-common-lisp-test
  :depends-on (#:climacs-syntax-common-lisp)
  :serial t
  :components
  ((:file "packages")
   (:file "vector-folio")
   (:file "folio")))
