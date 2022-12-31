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
           #:view-name
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
           #:move-region-to-vector
           #:kill-region
           #:unkill
           #:next-line
           #:previous-line
           #:beginning-of-buffer
           #:end-of-buffer
           #:forward-word
           #:backward-word
           #:back-to-indentation
           #:mark
           #:climacs-error
           #:mark-not-set
           #:cursor-positions
           #:set-cursor-positions
           #:search-forward))
