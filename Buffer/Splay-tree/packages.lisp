(cl:in-package #:common-lisp-user)

(defpackage #:splay-tree
  (:use #:common-lisp)
  (:export
   node
   make-node
   splay
   data
   left
   right
   notify-rotate
   detach-left
   detach-right
   notify-detach
   attach-left
   attach-right
   notify-attach
   ))
