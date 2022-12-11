(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-syntax-fundamental
  (:use #:common-lisp)
  (:local-nicknames (#:base #:second-climacs-base))
  (:shadow #:list-length)
  (:export
   #:analyzer
   #:view
   #:list-length
   #:contents
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
