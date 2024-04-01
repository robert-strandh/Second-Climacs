(cl:defpackage #:second-climacs-syntax-common-lisp
  (:use
   #:common-lisp)

  (:shadow
   #:package-name #:symbol-name)

  (:local-nicknames
   (#:edit     #:text.editing)
   (#:edit.exp #:text.editing.expression)
   (#:ip       #:incrementalist)

   (#:base     #:second-climacs-base))

  (:export
   #:line-length
   #:item
   #:common-lisp-view
   #:set-common-lisp-mode
   #:view

   #:indent-line

   #:up-expression
   #:forward-expression
   #:backward-expression
   #:mark-expression
   #:exchange-expressions
   #:beginning-of-top-level-expression
   #:end-of-top-level-expression
   #:fill-paragraph

   #:print-wad-tree
   #:first-top-level-wad))
