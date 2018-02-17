(cl:in-package #:asdf-user)

(defsystem :climacs-syntax-common-lisp-indentation
  :depends-on (:climacs-syntax-common-lisp-base)
  :serial t
  :components
  ((:file "indentation-support")
   (:file "let-and-letstar")
   (:file "setf-and-setq")
   (:file "eval-when")
   (:file "prog1-etc")
   (:file "block")))
