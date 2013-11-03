(cl:in-package #:common-lisp-user)

(defpackage #:climacs-line
  (:use #:common-lisp))

(defpackage #:climacs-buffer
  (:use #:common-lisp)
  (:export
   #:cursor-attached
   #:cursor-detached
   #:line
   #:*empty-line-constructor*
   #:attach-line
   #:detach-line
   #:cursor
   #:detached-cursor
   #:attached-cursor
   #:detached-left-sticky-cursor
   #:detached-right-sticky-cursor
   #:left-sticky-mixin
   #:right-sticky-mixin
   #:make-left-sticky-cursor
   #:make-right-sticky-cursor
   #:insert-item
   #:delete-item
   #:erase-item
   #:beginning-of-line-p
   #:end-of-line-p
   #:beginning-of-line
   #:end-of-line
   #:item-after-cursor
   #:item-before-cursor
   #:forward-item
   #:backward-item
   #:attach-cursor
   #:detach-cursor
   #:item-count
   #:cursor-position
   #:line-count
   #:split-line
   #:join-line
   #:items
   #:make-empty-buffer
   #:line-split-line
   #:line-join-line
   ))
