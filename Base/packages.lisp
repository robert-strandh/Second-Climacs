(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-base
  (:nicknames #:climacs2-base)
  (:use #:common-lisp)
  (:export #:buffer
	   #:insert-item
	   #:standard-buffer
	   #:cluffer-buffer
	   #:analyzer
	   #:update-analyzer-from-buffer
	   #:update-analyzer
	   #:null-analyzer
	   #:view
	   #:update-view-from-analyzer
	   #:update-view
	   #:update-window-from-view
	   #:update-window))
