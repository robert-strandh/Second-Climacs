(cl:in-package #:asdf-user)

(defsystem :climacs-syntax-common-lisp-indentation
  :depends-on (:climacs-syntax-common-lisp-base)
  :serial t
  :components
  ((:file "indentation-support")
   (:file "indentation")))
