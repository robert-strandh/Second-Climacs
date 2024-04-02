(cl:defpackage #:second-climacs-clim-view-fundamental
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:edit               #:text.editing)
   (#:base               #:second-climacs-base)
   (#:fundamental-syntax #:second-climacs-syntax-fundamental)
   (#:clim-base          #:second-climacs-clim-base))

  (:export
   #:fundamental-view))
