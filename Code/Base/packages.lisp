(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-base
  (:use #:common-lisp)

  (:local-nicknames
   (#:e #:text.editing))

  (:export #:buffer
           #:standard-buffer
           #:make-empty-standard-buffer
           #:fill-buffer-from-stream
           #:analyzer
           #:update-analyzer-from-buffer
           #:update-analyzer
           #:null-analyzer
           #:view
           #:view-class
           #:view-name
           #:site
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
           #:move-region-to-vector
           #:kill-region
           #:next-line
           #:previous-line
           #:beginning-of-buffer
           #:end-of-buffer
           #:forward-word
           #:backward-word
           #:delete-word
           #:erase-word
           #:delete-indentation
           #:mark
           #:climacs-error
           #:cursor-positions
           #:set-cursor-positions))
