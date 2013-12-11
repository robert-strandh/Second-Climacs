(cl:in-package #:common-lisp-user)

(asdf:defsystem #:chrono-tree-test
  :depends-on (:chrono-tree)
  :components
  ((:file "test-packages")
   (:file "test" :depends-on ("test-packages"))))
