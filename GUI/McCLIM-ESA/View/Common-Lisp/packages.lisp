(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-clim-common-lisp-view
  (:use #:common-lisp)
  (:export #:move-viewport-to-cursor
           #:move-cursor-to-viewport))
