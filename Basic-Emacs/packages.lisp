(cl:in-package #:common-lisp-user)

(defpackage #:climacs-basic-emacs
  (:use #:common-lisp)
  (:export
   #:forward-item
   #:backward-item
   #:insert-item
   #:delete-item
   #:erase-item
   #:item-before-cursor
   #:item-after-cursor
   #:buffer-from-stream
   ))
