(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-base
  (:nicknames #:climacs2-base)
  (:use #:common-lisp)
  (:export #:buffer
	   #:insert-item
	   #:delete-item
	   #:erase-item
	   #:forward-item
	   #:backward-item
	   #:beginning-of-line
	   #:end-of-line
	   #:item-before-cursor
	   #:standard-buffer
	   #:cluffer-buffer
	   #:make-empty-standard-buffer-and-cursor
	   #:analyzer
	   #:update-analyzer-from-buffer
	   #:update-analyzer
	   #:null-analyzer
	   #:view
	   #:cursor
	   #:window
	   #:update-view-from-analyzer
	   #:update-view
	   #:hide-view
	   #:expose-view
	   #:fundamental-view
	   #:update-window-from-view
	   #:update-window
	   #:application
	   #:views))
