(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:shadow #:package-name #:symbol-name)

  (:local-nicknames
   (#:base      #:second-climacs-base)
   (#:ip        #:incrementalist))

  (:export
   #:line-length
   #:item
   #:common-lisp-view
   #:set-common-lisp-mode
   #:view
   #:wad-to-cst

   #:indent-line

   #:up-expression
   #:forward-expression
   #:backward-expression
   #:mark-expression
   #:exchange-expressions
   #:beginning-of-top-level-expression
   #:end-of-top-level-expression
   #:fill-paragraph

   #:print-wad-tree
   #:first-top-level-wad))
