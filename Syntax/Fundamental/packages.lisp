(cl:in-package #:common-lisp-user)

(defpackage #:climacs-syntax-fundamental
  (:use #:common-lisp)
  (:export
   #:cache
   #:prefix
   #:suffix
   #:push-to-prefix
   #:pop-from-prefix
   #:push-to-suffix
   #:pop-from-suffix
   #:prefix-to-suffix
   #:suffix-to-prefix))
