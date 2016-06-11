(cl:in-package #:common-lisp-user)

(defpackage #:climacs-list-output-history
  (:use #:common-lisp)
  (:export #:list-output-history
	   #:lines
	   #:time-stamp
	   #:buffer))
