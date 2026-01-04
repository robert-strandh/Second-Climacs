(cl:defpackage #:second-climacs-clim-view-common-lisp
  (:use
   #:common-lisp)

  (:local-nicknames
   (#:ip          #:incrementalist)
   (#:edit        #:text.editing)
   (#:edit.search #:text.editing.search)
   (#:tbl         #:esclados-command-table-manipulation)
   (#:frame       #:esclados-frame)

   (#:base        #:second-climacs-base)
   (#:cl-syntax   #:second-climacs-syntax-common-lisp)
   (#:clim-base   #:second-climacs-clim-base)))
