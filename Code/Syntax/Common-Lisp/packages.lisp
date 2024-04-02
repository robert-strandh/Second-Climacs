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

   #:fill-paragraph

   #:print-wad-tree
   #:first-top-level-wad))
