(cl:in-package #:common-lisp-user)

(asdf:defsystem :climacs-syntax-common-lisp
  :components
  ((:file "packages")
   (:file "analyzer" :depends-on ("packages"))
   (:file "more-variables"
    :depends-on ("packages"))
   (:file "additional-conditions"
    :depends-on ("packages"))
   (:file "utilities"
    :depends-on ("packages" "more-variables"))
   (:file "tokens"
    :depends-on ("packages" "more-variables"))
   (:file "reader"
    :depends-on ("packages" "tokens" "more-variables" "utilities" "analyzer"))
   (:file "macro-functions"
    :depends-on ("packages" "more-variables" "utilities"))
   (:file "init"
    :depends-on ("macro-functions"))
   (:file "quasiquote-macro"
    :depends-on ("additional-conditions"))))
