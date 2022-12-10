(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:shadow #:package-name #:symbol-name)
  ;; FIXME: Remove this nickname when no longer referred to.
  (:nicknames #:second-climacs-syntax-common-lisp)
  (:local-nicknames (#:reader #:eclector.reader)
                    (#:readtable #:eclector.readtable)
                    (#:base #:second-climacs-base))
  (:export
   #:folio
   #:line-count
   #:line-length
   #:line-contents
   #:item
   #:folio-stream
   #:synchronize
   #:common-lisp-view
   #:set-common-lisp-mode
   #:view
   #:analyzer
   #:scavenge
   #:read-forms

   ;; Wads
   #:wad
   #:expression-wad
   #:no-expression-wad
   #:skipped-wad
   #:comment-wad
   #:block-command-wad
   #:semicolon-comment-wad
   #:ignored-wad
   #:sharpsign-wad
   #:sharpsign-plus-wad
   #:sharpsign-minus-wad
   #:read-suppress-wad
   #:reader-macro-wad
   #:error-wad
   #:eof-wad
   #:start-line
   #:height
   #:end-line
   #:start-column
   #:end-column
   #:min-column-number
   #:max-column-number
   #:children
   #:expression
   #:token

   ;; Tokens
   #:token ; class
   #:symbol-token
   #:non-existing-package-symbol-token
   #:non-existing-symbol-token
   #:existing-symbol-token
   #:value-token
   #:numeric-token
   #:other-token

   #:package-name
   #:package-marker-1
   #:package-marker-2
   #:name
   #:cache
   #:prefix
   #:suffix
   #:push-to-prefix
   #:pop-from-prefix
   #:push-to-suffix
   #:pop-from-suffix
   #:prefix-to-suffix
   #:suffix-to-prefix
   #:min-column-number
   #:max-column-number
   #:max-line-width
   #:indent-line

   #:up-expression))