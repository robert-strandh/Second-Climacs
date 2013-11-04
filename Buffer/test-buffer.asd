(cl:in-package #:common-lisp-user)

(asdf:defsystem :test-buffer
  :components
  ((:file "test-packages")
   (:file "test-buffer" :depends-on ("test-packages"))))

