(defpackage #:climacs-show
  (:use #:common-lisp)
  (:export
   #:update
   #:make-show))

(in-package #:climacs-show)

(defgeneric make-show (analyzer cursor wrap))

(defgeneric update (show))
