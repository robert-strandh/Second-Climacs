(cl:defpackage #:second-climacs-base
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:edit        #:text.editing)
   (#:edit.search #:text.editing.search))

  (:export
   #:buffer
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
   #:climacs-error))
