(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-base
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
           #:item-after-cursor
           #:standard-buffer
           #:standard-cursor
           #:cluffer-buffer
           #:make-empty-standard-buffer-and-cursor
           #:fill-buffer-from-stream
           #:analyzer
           #:update-analyzer-from-buffer
           #:update-analyzer
           #:null-analyzer
           #:view
           #:view-class
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
           #:views
           #:set-the-mark
           #:exchange-cursor-and-mark
           #:kill-region
           #:next-line
           #:previous-line
           #:beginning-of-buffer
           #:end-of-buffer
           #:forward-word
           #:backward-word
           #:mark
           #:climacs-error
           #:mark-not-set))
