(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-clim-view-common-lisp
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:ip        #:incrementalist)
   (#:edit      #:text.editing)

   (#:base      #:second-climacs-base)
   (#:cl-syntax #:second-climacs-syntax-common-lisp)
   (#:clim-base #:second-climacs-clim-base)))
