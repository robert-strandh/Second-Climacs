(cl:in-package #:common-lisp-user)

(defpackage #:climacs-view
  (:use #:common-lisp)
  (:export
   #:analyzer
   #:show
   #:command-table
   #:command-key-processor
   #:cursor
   #:view
   ))
