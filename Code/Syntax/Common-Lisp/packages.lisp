(cl:in-package #:common-lisp-user)

(defpackage #:second-climacs-syntax-common-lisp
  (:use #:common-lisp)
  (:shadow #:package-name #:symbol-name)
  (:local-nicknames (#:reader #:eclector.reader)
                    (#:readtable #:eclector.readtable)
                    (#:base #:second-climacs-base)
                    (#:gs #:trivial-gray-streams)
                    (#:flx #:flexichain))
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
   #:update-cache

   ;; Wads
   #:wad
   #:expression-wad
   #:labeled-object-definition-wad
   #:labeled-object-reference-wad
   #:no-expression-wad
   #:skipped-wad
   #:comment-wad
   #:block-command-wad
   #:semicolon-comment-wad
   #:word-wad
   #:misspelled
   #:ignored-wad
   #:sharpsign-wad
   #:sharpsign-plus-wad
   #:sharpsign-minus-wad
   #:read-suppress-wad
   #:reader-macro-wad
   #:error-wad
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
