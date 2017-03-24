(cl:in-package #:common-lisp-user)

(defpackage #:climacs-syntax-fundamental
  (:use #:common-lisp)
  (:shadow #:list-length)
  (:export
   #:analyzer
   #:view
   #:list-length
   #:prefix
   #:suffix
   #:push-to-prefix
   #:pop-from-prefix
   #:push-to-suffix
   #:pop-from-suffix
   #:prefix-to-suffix
   #:suffix-to-prefix
   #:adjust-prefix-and-suffix
   #:scavenge))
